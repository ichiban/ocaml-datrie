open BatPervasives

module Make (Key : DatrieInterface.Key) =
struct

  type offset = Offset of int

  let int_of_offset = function
    | Offset offset -> offset

  type state = State of int

  let int_of_state = function
    | State i -> i

  let state offset input =
    match offset with
      | Offset o ->
        State (o + Key.code input)

  type 'a base =
    | NextOffset of offset
    | Value of 'a

  let offset = function
    | NextOffset offset -> offset
    | _ -> failwith "base must be a next offset"

  let value = function
    | Value value -> value
    | _ -> failwith "base must be a value"

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
    let index = int_of_state state in
    if index >= BatDynArray.length datrie.nodes then
      None
    else
      BatDynArray.get datrie.nodes index
	    
  let set_node datrie state node =
    let index = int_of_state state in
    ensure_index datrie index;
    BatDynArray.set datrie.nodes index node
	  
  let base datrie state =
    (node datrie state |> BatOption.get).base

  let check datrie state =
    (node datrie state |> BatOption.get).check

  let next_state datrie prev_state input =
    match (node datrie prev_state |> BatOption.get) with
      | { base = NextOffset offset; check = _ } ->
        Some (state offset input)
      | _ ->
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
      walk datrie state input |> BatOption.is_some
    ) (Key.all ())

  let is_empty datrie index =
    node datrie index |> BatOption.is_none
  
  let is_storable datrie inputs offset =
    let index offset input = Some (state offset input) in
    let indexes = BatSet.filter_map (index offset) inputs in
    BatSet.for_all (is_empty datrie) indexes

  let next_offset datrie expected_inputs =
    let length = BatDynArray.length datrie.nodes in
    let is_storable_inputs = is_storable datrie expected_inputs in
    let offset_of i = Offset i in
    let offsets = BatEnum.map offset_of (0 -- (length - 1)) in
    try (BatEnum.find is_storable_inputs offsets) with
      | Not_found -> Offset length

  let resolve_conflict datrie prev_state input =
    let old_transitions = transitions datrie prev_state in
    let new_transitions = BatSet.add input old_transitions in
    let old_offset = base datrie prev_state |> offset in
    let new_offset = next_offset datrie new_transitions in
    assert (old_offset <> new_offset);
    BatSet.iter (fun input ->
      let old_state = state old_offset input in
      let new_state = state new_offset input in
      assert (old_state <> new_state);
      set_node datrie new_state (node datrie old_state);
      BatSet.iter (fun input' ->
	let child_state = walk datrie old_state input' |> BatOption.get in
        set_node datrie child_state (Some {
          (node datrie child_state |> BatOption.get) with
            check = new_state
        })
      ) (transitions datrie old_state);
      set_node datrie old_state None
    ) old_transitions;
    set_node datrie prev_state (Some {
      (node datrie prev_state |> BatOption.get) with
        base = NextOffset new_offset
    })

  let ensure_next_node ?(inserting=true) ?(updating=true) datrie current_state input base =
    let rec aux () =
      let next_state = next_state datrie current_state input |> BatOption.get in
      match (node datrie next_state) with
        | None ->
          if not inserting then
	    raise (InvalidState "key exists");
          set_node datrie next_state (Some {
            base = base;
            check = current_state;
          });
          next_state
        | Some { base = _; check = check } when check <> current_state ->
          (* WARNING: conflict detected *)
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
          next_state in
    aux ()

  let eos datrie =
    Key.input 0

  let write ?(inserting=true) ?(updating=true) datrie key value =
    let inputs = Key.enum key in
    let rec iter current_state =
      let input = BatEnum.get inputs in
      match (node datrie current_state |> BatOption.get, input) with
        | ({ base = NextOffset _; check = _ }, None) ->
          ignore (ensure_next_node datrie current_state (eos datrie) (Value value)
                    ~inserting:inserting
                    ~updating:updating)
        | ({ base = NextOffset _; check = _ }, Some input) ->
          let next_offset = next_offset datrie (BatSet.singleton input) in
          iter (ensure_next_node datrie current_state input (NextOffset next_offset)
                  ~inserting:inserting
                  ~updating:updating)
        | ({ base = Value _; check = _ }, _) ->
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
      match (node datrie current_state |> BatOption.get, input) with
        | ({ base = NextOffset _; check = _ }, None) ->
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
        | ({ base = NextOffset _; check = _ }, Some input) ->
          (match (walk datrie current_state input) with
            | None ->
              None
            | Some next_state ->
              iter next_state)
        | ({ base = Value value; check = _ }, _) ->
          None in
    iter (State 0)

  let path datrie key =
    let inputs = Key.enum key in
    let rec iter current_state path =
      let input = BatEnum.get inputs in
      match (node datrie current_state |> BatOption.get, input) with
        | ({ base = NextOffset _; check = _ }, None) ->
	  path
        | ({ base = NextOffset _; check = _ }, Some input) ->
          (match (walk datrie current_state input) with
            | None ->
              path
            | Some next_state ->
              iter next_state (next_state :: path))
        | ({ base = Value value; check = _ }, _) ->
          path in
    iter (State 0) [State 0]

  let delete datrie key =
    let path = path datrie key in
    let rec iter = function
      | [] -> failwith "invalid path"
      | [State 0] -> ()
      | state :: rest ->
	(match (node datrie state |> BatOption.get) with
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
	let offset = base datrie prev_state |> offset |> int_of_offset in
	let input = Key.input (i - offset) in
	iter (input :: inputs) prev_state in
    iter [] state

  let common_prefix_search datrie key =
    let inputs = Key.enum key in
    let rec iter current_state k =
      let input = BatEnum.get inputs in
      match (node datrie current_state |> BatOption.get) with
        | { base = NextOffset _; check = _ } ->
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
              let value = base datrie next_state |> value in
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
		      )))
        | { base = Value value; check = _ } ->
          k [key_of datrie current_state, value] in
    iter (State 0) identity

  let all_leaves_after datrie state =
    let queue = Queue.create () in
    Queue.push state queue;
    let rec iter k =
      if Queue.is_empty queue then
	k []
      else
	let current_state = Queue.pop queue in
	match (node datrie current_state |> BatOption.get) with
	  | { base = NextOffset _; check = _ } ->
	    BatSet.iter (function input ->
	      (match (walk datrie current_state input) with
		| None -> ()
		| Some next_state ->
		  Queue.push next_state queue)
	    ) (transitions datrie current_state);
	    iter k
	  | { base = Value value; check = prev_state } ->
	    iter (fun x -> k ((key_of datrie prev_state, value) :: x)) in
    iter identity
	  
  let predictive_search datrie key =
    let inputs = Key.enum key in
    let rec iter current_state =
      match BatEnum.get inputs with
	| None ->
	  all_leaves_after datrie current_state
	| Some input ->
	  (match (walk datrie current_state input) with
	    | None ->
	      []
	    | Some next_state ->
	      iter next_state) in
  iter (State 0)

  let lookup datrie key = get datrie key

  let reverse_lookup ?(cmp=(=)) datrie value =
    let continuation = ref identity in
    BatDynArray.iter (fun node ->
      match node with
	| Some { base = Value node_value; check = prev_state } ->
	  if cmp value node_value then
	    let k = !continuation in
	    continuation := (fun x -> k ((key_of datrie prev_state) :: x))
	| _ -> ()
    ) datrie.nodes;
    !continuation []
end

module StringDatrie = Make(DatrieKey.StringKey)
