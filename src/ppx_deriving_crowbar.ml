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
    let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
    begin match builtin, typ with
      | true, [%type: unit] -> [%expr fun () -> ()]
      | true, [%type: int] -> [%expr Crowbar.int]
      | true, [%type: int32]
      | true, [%type: Int32.t] -> [%expr Crowbar.int32]
      | true, [%type: int64]
      | true, [%type: Int64.t] -> [%expr Crowbar.int64]
      | true, [%type: float] -> [%expr Crowbar.float]
      | true, [%type: bool] -> [%expr Crowbar.bool]
      | true, [%type: char] -> [%expr Crowbar.(map [uint8] Char.chr)]
      | true, [%type: string]
      | true, [%type: String.t] -> [%expr Crowbar.bytes]
    end
  | { ptyp_loc } -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (Ppx_deriving.string_of_core_type typ)

(* TODO: major cargo culting here, I have no idea how this works *)
let core_type_of_decl ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] -> [%t var] -> Ppx_deriving_runtime.bool])
    type_decl
    [%type: [%t typ] -> [%t typ] -> Ppx_deriving_runtime.bool]

let str_of_type ~options ~path ({ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  let generator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      expr_of_typ quoter manifest
    | Ptype_variant constrs, _ ->
      (* TODO: this is also lifted wholesale from ppx_deriving_eq.cppo.ml *)
      let argn kind =
        Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")
      in
      let argl kind =
        Printf.sprintf (match kind with `lhs -> "lhs%s" | `rhs -> "rhs%s")
      in
      let pattn side typs =
        List.mapi (fun i _ -> pvar (argn side i)) typs in
      let pattl side labels =
        List.map (fun { pld_name = { txt = n } } -> n, pvar (argl side n)) labels in
      let rec exprn quoter typs =
        typs |> List.mapi (fun i typ ->
            app (expr_of_typ quoter typ) [evar (argn `lhs i); evar (argn `rhs i)])
      and exprl quoter typs =
        typs |> List.map (fun { pld_name = { txt = n }; pld_type = typ } ->
            app (expr_of_typ quoter typ) [evar (argl `lhs n); evar (argl `rhs n)])
      in
      let pconstrrec name fields =
        pconstr name [precord ~closed:Closed fields]
      in
      let cases =
        (constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_loc } ->
          with_default_loc pcd_loc @@ fun () ->
          match pcd_args with
          | Pcstr_tuple(typs) ->
            exprn quoter typs |>
            Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
            Exp.case (ptuple [pconstr name (pattn `lhs typs);
                              pconstr name (pattn `rhs typs)])
          | Pcstr_record(labels) ->
            exprl quoter labels |>
            Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
            Exp.case (ptuple [pconstrrec name (pattl `lhs labels);
                              pconstrrec name (pattl `rhs labels)])
          )) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
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
