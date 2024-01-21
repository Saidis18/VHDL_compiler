type vhdl = {
  entity_decl : entity list;
  arch_decl : architecture list
}

and entity = {
  declaration_name : string;
  io_decl : io list
}

and io = {
  io_name : string;
  io_choice : io_kind
}

and io_kind =
  | In
  | Out

and architecture = {
  arch_name : string;
  entity_name : string;
  declarations : declaration list;
  instructions : instruction list
}

and declaration =
  | Signal of signal
  | Component of entity

and signal = {signal_name : string}

and instruction =
  | Assignment of assignment
  | Instantiation of instantiation

and assignment = {
  modified_signal : signal;
  expr : expr
}

and expr =
  | Const of value
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr

and value =
  | Up
  | Down
  | Undf

and instantiation = {
  inst_name : string;
  entity_model_name : string;
  port_connections : port_connection list
}

and port_connection = {
  connected_io : string;
  connected_signal : signal
}
