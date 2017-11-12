open Location
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience

let deriver = "crowbar"
let raise_errorf = Ppx_deriving.raise_errorf

(* currently we ignore all options *)

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | {ptyp_desc = Ptyp_constr _ } ->
    begin
      match typ with
    | [%type: unit] -> [%expr Crowbar.const ()]
    | [%type: int] -> [%expr Crowbar.int]
    | [%type: int32]
    | [%type: Int32.t] -> [%expr Crowbar.int32]
    | [%type: int64]
    | [%type: Int64.t] -> [%expr Crowbar.int64]
    | [%type: float] -> [%expr Crowbar.float]
    | [%type: bool] -> [%expr Crowbar.bool]
    | [%type: char] -> [%expr Crowbar.(map [uint8] Char.chr)]
    | [%type: string]
    | [%type: String.t] -> [%expr Crowbar.bytes]
    end
  | { ptyp_loc } -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (Ppx_deriving.string_of_core_type typ)

(* TODO: major cargo culting here, I have no idea how this works *)
let core_type_of_decl ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] Crowbar.gen])
    type_decl
    [%type: [%t typ] Crowbar.gen]

let str_of_type ~options ~path ({ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  let generator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      expr_of_typ quoter manifest
    | Ptype_variant constrs, _ ->
      (* we must be sure that there are generators for all of the possible
         variant types, and then invoke Crowbar.choose on the list of them. *)
      let cases = constrs |> List.map (fun {pcd_name; pcd_res; pcd_args} ->
          (* under what circumstances can pcd_res be non-None and pcd_args be
             populated? *)
          match pcd_res, pcd_args with
          | None, Pcstr_tuple tuple ->
            let rec n_vars n (l : string list) =
              if n > 0 then
                n_vars (n-1) ((Ppx_deriving.fresh_var l)::l)
              else
                List.rev l
            in
            (* make a tuple of those names *)
            let desc =
              (List.map
                 (fun i -> Ast_helper.Exp.mk @@
                   Pexp_ident (Ast_convenience.lid i))
                 (n_vars (List.length tuple) []))
            in
            Ast_helper.Exp.construct (Ast_convenience.lid pcd_name.txt)
              (Some (Ast_convenience.tuple desc))
            (* wrap that tuple in the constructor *)
            (* TODO: wrap that tuple in `fun a b c d`
            let gens = List.map (expr_of_typ quoter) tuple in
            [%expr Crowbar.(map [%e gens] [%e fn])] *)
          | Some core_type, Pcstr_tuple _ | Some core_type, Pcstr_record _ ->
            (* C: T0  or C: T1 * ... * Tn -> T0 or C: {...} -> T0 *)
            expr_of_typ quoter core_type (* 
          | None, Pcstr_record labels ->
            (* C of {...} or C of {...} as t *) *)

        ) in
      Ast_helper.Exp.tuple cases
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type = Ppx_deriving.strong_type_of_type @@ core_type_of_decl ~options
      ~path type_decl in
  let generate_var = pvar (Ppx_deriving.mangle_type_decl (`Prefix "generate")
                              type_decl) in
  [Vb.mk (Pat.constraint_ generate_var out_type)
     (Ppx_deriving.sanitize ~quoter (polymorphize generator))]

let deriver = Ppx_deriving.create deriver
    ~core_type:(Ppx_deriving.with_quoter (fun quoter typ -> expr_of_typ quoter
                                             typ))
    ~type_decl_str:(fun ~options ~path type_decls ->
        [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path)
                                type_decls))])
    ()

let () = Ppx_deriving.register deriver
