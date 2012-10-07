open BatPervasives
open DatrieKey

module type D =
sig
  type key

  type 'a t

  val create : unit -> 'a t

  val set : 'a t -> key -> 'a -> unit
    
  val add : 'a t -> key -> 'a -> unit
    
  val update : 'a t -> key -> 'a -> unit

  val get : 'a t -> key -> 'a option
end

module Make (Key : DatrieKey.Key) =
struct
  type state =
    | State of int
	
  type offset =
    | Offset of int

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
    BatSet.iter (fun input ->
      set_node datrie (state datrie new_offset input) (Some {
        base = base datrie (state datrie old_offset input);
        check = prev_state;
      });
      BatSet.iter (fun input' ->
        match (base datrie (state datrie old_offset input')) with
          | NextOffset offset ->
            let s = state datrie offset input' in
            set_node datrie s (Some {
              (BatOption.get (node datrie s)) with
                check = state datrie new_offset input
            })
          | Value _ ->
            failwith "state must be a branch"
      ) (transitions datrie (state datrie old_offset input));
      set_node datrie (state datrie old_offset input) None
    ) old_transitions;
    set_node datrie prev_state (Some {
      (BatOption.get (node datrie prev_state)) with
        base = NextOffset new_offset
    })

  let ensure_next_node ?(inserting=true) ?(updating=true) datrie current_state input base =
    let rec aux () =
      match (next_state datrie current_state input) with
        | None ->
          failwith "invalid state (none)"
        | Some next_state ->
          (match (node datrie next_state) with
            | None ->
              if inserting then
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
              if updating then
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
end

module StringDatrie = Make(DatrieKey.StringKey)
