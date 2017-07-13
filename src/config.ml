open Core

module Reader_config = struct
  open Reader

  type options = {
    table    : Table.Config.options    [@default Table.Config.empty_options];
    table_ex : Table_ex.Config.options [@default Table_ex.Config.empty_options];
  } [@@deriving sexp]

  let empty_options = {
    table    = Table.Config.empty_options;
    table_ex = Table_ex.Config.empty_options;
  }
end

module Printer_config = struct
  open Printer

  type options = {
    table : Table.Config.options [@default Table.Config.empty_options];
  } [@@deriving sexp]

  let empty_options = {
    table = Table.Config.empty_options;
  }
end

type t = {
  reader          : string sexp_option;
  printer         : string sexp_option;
  reader_options  : Reader_config.options  [@default Reader_config.empty_options];
  printer_options : Printer_config.options [@default Printer_config.empty_options];
} [@@deriving sexp]

let default = {
  reader          = None;
  printer         = None;
  reader_options  = Reader_config.empty_options;
  printer_options = Printer_config.empty_options;
}
