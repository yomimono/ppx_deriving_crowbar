open Location
open Parsetree

let deriver = "crowbar"
let raise_errorf = Ppx_deriving.raise_errorf

let type_decl_str (decls : type_declaration list) =
  let one_type_decl {ptype_name; ptype_params; ptype_cstrs; ptype_kind;
                     ptype_private; ptype_manifest; ptype_attributes; ptype_loc} =
    (* given that, make a parsetree.structure *)
    [
      {
        pstr_loc = Location.mknoloc "new_type_decl";
        (* defining a function is Pstr_value with the function binding? *)
        pstr_desc = Pstr_value (Asttypes.Nonrecursive, [ (*rec_flag * value_binding list *)
            {
              pvb_pat = {ppat_loc = ptype_loc;
                         ppat_attributes = ptype_attributes;
                         ppat_desc = Ppat_var (Location.mknoloc "butt")
                        };
              pvb_expr = {
                pexp_loc = ptype_loc;
                pexp_attributes = ptype_attributes;
                pexp_desc = Pexp_fun (Simple, None, 
              };
              pvb_attributes = ptype_attributes;
              pvb_loc = Location.mknoloc "o hai";

          }]);
      }
    ]

  in
  one_type_decl @@ List.hd decls (* definitely not right :) *)

let deriver = Ppx_deriving.create "crowbar" ~type_decl_str ()

let () = Ppx_deriving.register deriver
