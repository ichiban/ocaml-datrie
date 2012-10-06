open BatPervasives

type state =
  | State of int

type offset =
  | Offset of int

type 'a input =
  | Input of 'a

type 'value base =
  | NextOffset of offset
  | Value of 'value

type 'value node = {
  base : 'value base;
  check : state;
}

type ('key, 'input, 'value) t = {
  nodes : 'value node option BatDynArray.t;
  enum : 'key -> 'input input BatEnum.t;
  all : unit -> 'input input BatSet.t;
  code : 'input input -> int;
  input : int -> 'input input;
}

let make ~enum ~all ~code ~input () = {
  nodes = BatDynArray.of_array [| Some { base = NextOffset (Offset 1); check = State 0 } |];
  enum = enum;
  all = all;
  code = code;
  input = input;
}

let input_of x = Input x

let of_input = function
  | Input x -> x

let create () =
  make ()
    ~enum:(BatString.enum |- BatEnum.map input_of)
    ~all:(BatChar.enum |- BatEnum.map input_of |- BatSet.of_enum)
    ~code:(of_input |- Char.code)
    ~input:(Char.chr |- input_of)

let enum datrie =
  BatDynArray.enum datrie.nodes

let of_enum e = {
  (create ()) with
    nodes = BatDynArray.of_enum e
}

let report ?(printer=ignore) datrie =
  BatDynArray.iteri (fun i node ->
    print_int i;
    print_string "\t";
    match node with
      | None ->
          print_newline ()
      | Some { base = NextOffset (Offset offset); check = State check} ->
          print_string "offset\t";
          print_int offset;
          print_string "\t";
          print_int check;
          print_newline ()
      | Some { base = Value value; check = State check } ->
          print_string "value\t";
          printer value;
          print_string "\t";
          print_int check;
          print_newline ()
  ) datrie.nodes

let length datrie =
  BatDynArray.length datrie.nodes

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
        State (o + datrie.code input)
          
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
  ) (datrie.all ())

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
  datrie.input 0

let write ?(inserting=true) ?(updating=true) datrie key value =
  let inputs = datrie.enum key in
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
  let inputs = datrie.enum key in
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
