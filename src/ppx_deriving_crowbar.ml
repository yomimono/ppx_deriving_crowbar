open Location
open Parsetree

let deriver = Ppx_deriving.create "crowbar" ()

let () = Ppx_deriving.register deriver
