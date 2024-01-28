(* I don't care for complexity - This is just a test *)

(*
open Vhdl

type token =
  | Keyword of keyword
  | Delimiter of delimiter
  | Assigner of assigner
  | Operation of operation
  | Name of string

and keyword =
  | K_entity
  | K_architecture
  | K_port
  | K_signal

and delimiter =
  | D_begin
  | D_end
  | D_open_bracket
  | D_close_bracket

and assigner =
  | A_is
  | A_of
  | A_type
  | A_takes_value
  | A_connected_to

and operation =
  | O_not
  | O_and
  | O_or
  | O_xor
*)

let replace = function
  | ' ' | '\x0C' | '\n' | '\r' | '\t' -> ' ' (* ' ' and '\n' cases are not useful *)
  | c -> c
;;

let ignore_comments l =
  let prefix = "--" in
  let rec aux r  = function 
    | [] -> r
    | h::_ when String.starts_with ~prefix h -> r
    | h::t -> aux (h::r) t
  in aux [] l (* Resulting line is reversed *)
;;

let read_line file =
  file
  |> input_line
  |> String.map replace
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> ignore_comments (* Resulting line is reversed *)
;;

let extract_parenthesis str =
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
  |> List.flatten
;;

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
  |> List.map extract_parenthesis
  |> List.flatten
;;
