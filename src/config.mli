open Core

module Reader_config : sig
  open Reader
  type options = {
    table    : Table.Config.options;
    table_ex : Table_ex.Config.options;
  } [@@deriving sexp]
end

module Printer_config : sig
  open Printer
  type options = {
    table : Table.Config.options;
  } [@@deriving sexp]
end

type t = {
  default_reader  : string sexp_option;
  default_printer : string sexp_option;
  reader_options  : Reader_config.options;
  printer_options : Printer_config.options;
} [@@deriving sexp]

val default : t
