open BatPervasives

module Make (Key : DatrieInterface.Key) =
struct
  type state = State of int
	
  type offset = Offset of int

  type 'a base =
    | NextOffset of offset
    | Value of 'a

  type 'a node = {
    base : 'a base;
    check : state;
  }

  type key = Key.t
      
  type 'a t = {
    nodes : 'a node option BatDynArray.t;
  }

  exception InvalidState of string
      
  let create () = {
    nodes = BatDynArray.of_array [| Some {
      base = NextOffset (Offset 1);
      check = State 0;
    } |];
  }
      
  let rec ensure_index datrie i =
    if i < BatDynArray.length datrie.nodes then
      ()
    else
      begin
        BatDynArray.add datrie.nodes None;
        ensure_index datrie i;
      end

  let node datrie state =
    match state with
      | State i ->
        if i >= BatDynArray.length datrie.nodes then
          None
        else
          BatDynArray.get datrie.nodes i
	    
  let set_node datrie state node =
    match state with
      | State i ->
        ensure_index datrie i;
        BatDynArray.set datrie.nodes i node
	  
  let base datrie state =
    match (node datrie state) with
      | None -> failwith "empty cell (base)"
      | Some node -> node.base

  let check datrie state =
    match (node datrie state) with
      | None -> failwith "empty cell (check)"
      | Some node -> node.check

  let state datrie offset input =
    match offset with
      | Offset o ->
        State (o + Key.code input)

  let next_state datrie prev_state input =
    match (node datrie prev_state) with
      | None -> failwith "state must be a branch"
      | Some { base = NextOffset offset; check = _ } ->
        Some (state datrie offset input)
      | Some _ ->
        None
	  
  let next_node datrie state input =
    match (next_state datrie state input) with
      | None ->
        None
      | Some next_state ->
        node datrie next_state
	  
  let walk datrie state input =
    match (next_state datrie state input) with
      | None ->
        None
      | Some next_state ->
        match (node datrie next_state) with
          | None -> None
          | Some { base = _; check = check_state } ->
            if state = check_state then
              Some next_state
            else
              None

  let transitions datrie state =
    BatSet.filter (fun input ->
      match (walk datrie state input) with
        | None -> false
        | Some _ -> true
    ) (Key.all ())

  let is_empty datrie index =
    match (node datrie index) with
      | None -> true
      | Some _ -> false
  
  let index datrie offset input =
    Some (state datrie offset input)

  let is_storable datrie inputs offset =
    let indexes = BatSet.filter_map (index datrie offset) inputs in
    BatSet.for_all (is_empty datrie) indexes

  let next_offset datrie expected_inputs =
    let length = BatDynArray.length datrie.nodes in
    let is_storable_inputs = is_storable datrie expected_inputs in
    let offset_of i = Offset i in
    try (BatEnum.find is_storable_inputs (BatEnum.map offset_of (0 -- (length - 1)))) with
      | Not_found -> Offset length

  let resolve_conflict datrie prev_state input =
    let old_transitions = transitions datrie prev_state in
    let new_transitions =
      BatSet.add input old_transitions in
    let old_offset = match (base datrie prev_state) with
      | NextOffset offset ->
        offset
      | Value _ ->
        failwith "prev_state must be a branch" in
    let new_offset = next_offset datrie new_transitions in
    assert (old_offset != new_offset);
    BatSet.iter (fun input ->
      let old_state = state datrie old_offset input in
      let new_state = state datrie new_offset input in
      assert (old_state != new_state);
      set_node datrie new_state (Some {
	(BatOption.get (node datrie old_state)) with
          check = prev_state	  
      });
      BatSet.iter (fun input' ->
	let child_state = state datrie old_offset input' in
        match (base datrie child_state) with
          | NextOffset offset ->
            let s = state datrie offset input' in
            set_node datrie s (Some {
              (BatOption.get (node datrie s)) with
                check = new_state
            })
          | Value _ ->
            failwith "state must be a branch"
      ) (transitions datrie old_state);
      set_node datrie old_state None
    ) old_transitions;
    set_node datrie prev_state (Some {
      (BatOption.get (node datrie prev_state)) with
        base = NextOffset new_offset
    });
    assert (BatSet.for_all (fun input ->
      let next_state = state datrie new_offset input in
      match (node datrie next_state) with
	| Some { base = _; check = check } when check = next_state ->
	  true
	| _ -> false
    ) (transitions datrie prev_state))

  let ensure_next_node ?(inserting=true) ?(updating=true) datrie current_state input base =
    let rec aux () =
      match (next_state datrie current_state input) with
        | None ->
          failwith "invalid state (none)"
        | Some next_state ->
          (match (node datrie next_state) with
            | None ->
              if not inserting then
		raise (InvalidState "key exists");
              set_node datrie next_state (Some {
                base = base;
                check = current_state;
              });
              next_state
            | Some { base = _; check = check } when check <> current_state ->
              (* WARNING: collision detected *)
              resolve_conflict datrie current_state input;
              aux ()
            | Some { base = NextOffset _; check = _ } ->
              next_state
            | Some { base = Value _; check = _ } ->
              if not updating then
		raise (InvalidState "key doesn't exist");
              set_node datrie next_state (Some {
                base = base;
                check = current_state;
              });
              next_state) in
    aux ()

  let eos datrie =
    Key.input 0

  let write ?(inserting=true) ?(updating=true) datrie key value =
    let inputs = Key.enum key in
    let rec iter current_state =
      let input = BatEnum.get inputs in
      match (node datrie current_state, input) with
        | (None, _) ->
          failwith "invalid state (write)"
        | (Some { base = NextOffset _; check = _ }, None) ->
          ignore (ensure_next_node datrie current_state (eos datrie) (Value value)
                    ~inserting:inserting
                    ~updating:updating)
        | (Some { base = NextOffset _; check = _ }, Some input) ->
          let next_offset = next_offset datrie (BatSet.singleton input) in
          iter (ensure_next_node datrie current_state input (NextOffset next_offset)
                  ~inserting:inserting
                  ~updating:updating)
        | (Some { base = Value _; check = _ }, _) ->
          failwith "invalid key" in
    iter (State 0)

  let set datrie key value =
    write datrie key value

  let add datrie key value =
    write datrie key value
      ~updating:false 

  let update datrie key value =
    write datrie key value
      ~inserting:false

  let get datrie key =
    let inputs = Key.enum key in
    let rec iter current_state =
      let input = BatEnum.get inputs in
      match (node datrie current_state, input) with
        | (None, _) ->
          failwith "invalid state (get)"
        | (Some { base = NextOffset _; check = _ }, None) ->
          (match (walk datrie current_state (eos datrie)) with
            | None ->
              None
            | Some next_state ->
              (match (node datrie next_state) with
                | None ->
                  None
                | Some { base = Value value; check = check } when check = current_state ->
                  Some value
                | Some _ ->
                  None))
        | (Some { base = NextOffset _; check = _ }, Some input) ->
          (match (walk datrie current_state input) with
            | None ->
              None
            | Some next_state ->
              iter next_state)
        | (Some { base = Value value; check = _ }, _) ->
          None in
    iter (State 0)

  let path datrie key =
    let inputs = Key.enum key in
    let rec iter current_state path =
      let input = BatEnum.get inputs in
      match (node datrie current_state, input) with
        | (None, _) ->
          failwith "invalid state (get)"
        | (Some { base = NextOffset _; check = _ }, None) ->
	  path
        | (Some { base = NextOffset _; check = _ }, Some input) ->
          (match (walk datrie current_state input) with
            | None ->
              path
            | Some next_state ->
              iter next_state (next_state :: path))
        | (Some { base = Value value; check = _ }, _) ->
          path in
    iter (State 0) [State 0]

  let delete datrie key =
    let path = path datrie key in
    let rec iter = function
      | [] -> failwith "invalid path"
      | [State 0] -> ()
      | state :: rest ->
	(match (BatOption.get (node datrie state)) with
	  | { base = NextOffset _; check = _ } ->
	    if BatSet.cardinal (transitions datrie state) > 0 then
	      ();
	    set_node datrie state None;
	    iter rest
	  | { base = Value _; check = _ } ->
	    set_node datrie state None;
	    iter rest) in
    iter path

  let key_of datrie state =
    let rec iter inputs = function
      | State 0 ->
	inputs |> BatList.enum |> Key.of_enum
      | (State i) as state ->
	let prev_state = check datrie state in
	match (base datrie prev_state) with
	  | NextOffset (Offset o) ->
	    let input = Key.input (i - o) in
	    iter (input :: inputs) prev_state
	  | Value _ ->
	    failwith "invalid datrie"
    in
    iter [] state

  let common_prefix_search datrie key =
    let inputs = Key.enum key in
    let rec iter current_state k =
      let input = BatEnum.get inputs in
      match (node datrie current_state) with
        | None ->
	  failwith "invalid state"
        | Some { base = NextOffset _; check = _ } ->
          (match (walk datrie current_state (eos datrie)) with
            | None ->
	      (match input with
		| None ->
		  k []
		| Some input ->
		  (match (walk datrie current_state input) with
		    | None ->
		      k []
		    | Some next_state ->
		      iter next_state k))
            | Some next_state ->
              (match (node datrie next_state) with
                | None ->
		  failwith "next node must exist"
		| Some { base = NextOffset _; check = _ } ->
		  failwith "next node must be a leaf"
                | Some { base = Value value; check = _ } ->
		  (match input with
		    | None ->
		      k [key_of datrie current_state, value]
		    | Some input ->
		      match (walk datrie current_state input) with
			| None ->
			  k [key_of datrie current_state, value]
			| Some next_state ->
			  iter next_state (fun x ->
			    k ((key_of datrie current_state, value) :: x)
			  ))))
        | Some { base = Value value; check = _ } ->
          k [key_of datrie current_state, value] in
    iter (State 0) identity

  let predictive_search datrie key =
    []

  let length datrie =
    BatDynArray.length datrie.nodes

  let available_states datrie =
    BatEnum.filter_map (fun i ->
      match (node datrie (State i)) with
	| None -> None
	| Some _ -> Some i
    ) (0 -- ((length datrie) - 1))

  let is_leaf datrie i =
    match (BatOption.get (node datrie (State i))) with
      | { base = NextOffset _; check = _ } ->
	false
      | { base = Value _; check = _ } ->
	true

  let is_branch datrie i =
    not (is_leaf datrie i)

  let leaves datrie =
    BatEnum.filter (is_leaf datrie) (available_states datrie)

  let branches datrie =
    BatEnum.filter (is_branch datrie) (available_states datrie)

  let key datrie i =
    key_of datrie (State i)

  let value datrie i =
    match (BatOption.get (node datrie (State i))) with
      | { base = NextOffset _; check = _ } ->
	None
      | { base = Value value; check = _ } ->
	Some value
end

module StringDatrie = Make(DatrieKey.StringKey)
