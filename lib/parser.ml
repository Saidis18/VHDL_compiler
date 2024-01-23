(*
open Vhdl

type token =
  | Keyword of keyword
  | Delimiter of delimiter
  | Operator of operator
  | Name of name

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
*)

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
  |> List. filter (fun s -> s <> "")
  |> ignore_comments (* Resulting line is reversed *)

let words file_name =
  let file = open_in file_name in
  let r = ref [] in  
  let () =
  try
    while true do
      r := (read_line file) @ !r; (* It is ok to add in front because new line is already reversed *)
    done;
  with End_of_file ->
    close_in file;
  in
  List.rev !r
