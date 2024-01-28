(*
I don't care for complexity - This is just a test
This is a hand written "parser".
I will soon try on doing this the "proper" way.
*)

open Vhdl

let replace = function
  | ' ' | '\x0C' | '\n' | '\r' | '\t' -> ' ' (* ' ' and '\n' cases are not useful *)
  | c -> c

let ignore_comments l =
  let prefix = "--" in
  let rec aux r  = function 
    | [] -> r
    | h::_ when String.starts_with ~prefix h -> r
    | h::t -> aux (h::r) t
  in aux [] l (* Resulting line is reversed *)

let read_line file =
  file
  |> input_line
  |> String.map replace
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> ignore_comments (* Resulting line is reversed *)

let extract str =
  let rec add_token token = function
    | [] -> []
    | [x] -> [x]
    | h::t -> h::(String.make 1 token)::add_token token t
  in

  let aux token s =
  s
  |> String.split_on_char token
  |> add_token token
  |> List.filter (fun c -> c <> "")
  in

  str
  |> aux '('
  |> List.map (aux ')')
  |> List.map (List.map (aux ';'))
  |> List.flatten
  |> List.flatten

let words file_name =
  let file = open_in file_name in
  let r = ref [] in  
  let () =
  try
    while true do
      r := (read_line file) @ !r; (* It is ok to add at the head because new line is already reversed *)
    done;
  with End_of_file ->
    close_in file;
  in
  !r
  |> List.rev
  |> List.map extract
  |> List.flatten

exception Incorrect_bracketing

let get begin_token end_token l =
  let rec aux i l = function
    | []  when i <> 0 -> raise Incorrect_bracketing
    | _ when i < 0 -> raise Incorrect_bracketing
    | [] -> List.rev l, []
    | h::t when h = end_token && i = 1 -> List.rev l, t
    | h::t when h = end_token -> aux (i-1) (h::l) t
    | h::t when h = begin_token && i = 0 -> aux 1 l t
    | h::t when h = begin_token -> aux (i+1) (h::l) t
    | h::t -> aux i (h::l) t
in aux 0 [] l

exception Syntax_error

let rec parse_io = function
  | [] -> []
  | h::":"::"in"::"std_logic"::";"::t ->
    {io_name = h; io_choice = In}::parse_io t
  | h::":"::"out"::"std_logic"::";"::t ->
    {io_name = h; io_choice = Out}::parse_io t
  | _ -> raise Syntax_error

let parse_entity = function
  | "component"::name::"is"::"port"::t
  | "entity"::name::"is"::"port"::t ->
    let io_list, remainder = get "(" ")" t in
    begin
      match remainder with
        |";"::"end"::str::tt when str = name ->
          let io_decl = parse_io io_list in
          {declaration_name = name; io_decl = io_decl}, tt
        | _ -> raise Syntax_error
    end
  | _ -> raise Syntax_error

let rec parse_declaration = function
  | [] -> [], []
  | "signal"::name::":"::"std_logic"::";"::t ->
    let parsed_decl, instr = parse_declaration t in
    (Signal {signal_name = name})::parsed_decl, instr
  | "component"::_ as l ->
    let component, t = parse_entity l in
    let parsed_decl, instr = parse_declaration t in
    (Component component)::parsed_decl, instr
  | "begin"::_ as l -> [], l
  | _ -> raise Syntax_error

let parse_expr _ = Const Up, []
let parse_port _ = []

let rec parse_instr = function
  | [] -> [], []
  | name::"<="::t ->
    let e, remainder = parse_expr t in
    let instr, remainder' = parse_instr remainder in
    let modified_signal = {signal_name = name} in
    (Assignment {modified_signal = modified_signal; expr = e})::instr, remainder'
  | inst_name::entity_name::"port"::"map"::t ->
    let port, remainder = get "(" ")" t in
    let instr, remainder' = parse_instr remainder in
    let parsed_port = parse_port port in
    let instantiation = {inst_name = inst_name; entity_model_name = entity_name; port_connections = parsed_port} in
    (Instantiation instantiation)::instr, remainder'
  | _ -> raise Syntax_error

let parse_architecture = function
  | "architecture"::arch_name::"of"::entity_name::"is"::t ->
    let decl, remainder = parse_declaration t in
    let instr, remainder' = parse_instr remainder in
    {arch_name = arch_name; entity_name = entity_name; declarations = decl; instructions = instr}, remainder'
  | _ -> raise Syntax_error

