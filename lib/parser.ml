(*
open Vhdl

type token =
  | Keyword of keyword
  | Delimiter of delimiter
  | Operator of operator
  | Name of name
*)
let words file_name =
  let file = open_in file_name in
  let r = ref [] in
  let () =
  try
    while true do
      r := (file |> input_line |> String.split_on_char ' ' |> List.rev) @ !r;
    done;
  with End_of_file ->
    close_in file;
  in
  List.rev !r
