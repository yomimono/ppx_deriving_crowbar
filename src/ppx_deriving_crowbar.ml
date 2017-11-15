open Location
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience

let deriver = "crowbar"
let raise_errorf = Ppx_deriving.raise_errorf

(* currently we ignore all options *)

let mangler = Ppx_deriving.(`Prefix "generate")

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let make_crowbar_list l =
  let consify add_me extant =
      Ast_helper.Exp.(construct (Ast_convenience.lid "Crowbar.::")
                        (Some (tuple [add_me; extant])))
  in
  List.fold_right consify l (Ast_helper.Exp.construct (Ast_convenience.lid
                                                 "Crowbar.[]") None)

(* TODO: this is _definitely_ not right, at least if you seed it with [] *)
let rec n_vars n (l : string list) =
  if n > 0 then n_vars (n-1) ((Ppx_deriving.fresh_var l)::l)
  else List.rev l

let last_fun arg function_body = Ast_helper.Exp.fun_ Nolabel None
    (Ast_helper.Pat.var (Location.mknoloc arg))
    function_body

let lazify e =
  [%expr lazy [%e e]]

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
    begin
      match typ with
    | [%type: unit] -> [%expr const ()]
    | [%type: int] -> [%expr int]
    | [%type: int32]
    | [%type: Int32.t] -> [%expr int32]
    | [%type: int64]
    | [%type: Int64.t] -> [%expr int64]
    | [%type: float] -> [%expr float]
    | [%type: bool] -> [%expr bool]
    | [%type: char] -> [%expr (map [uint8] Char.chr)]
    | [%type: string]
    | [%type: String.t] -> [%expr bytes]
    | [%type: bytes]
    | [%type: Bytes.t] -> [%expr map [bytes] Bytes.of_string]
    | [%type: nativeint]
    | [%type: Nativeint.t] -> [%expr map [int] Nativeint.of_int]
    (* TODO: polymorphic variants, ref, list, array, option,
       lazy_t "and their Mod.t aliases" (e.g. Option.t I guess?), result *)
    (* also TODO: do we DTRT for [@nobuiltin]? *)
    (* TODO: parametric types? *)
    (* TODO: mutually recursive types don't work, I'm pretty sure *)
    | _ ->
    let fwd = app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid mangler lid)))
        (List.map expr_of_typ args)
    in
    (* [%expr fun x -> [%e fwd] x]   (* ppx_deriving_yojson claims this is needed for "recursive groups" *) *)
    [%expr unlazy [%e fwd]]
    end
  | { ptyp_desc = Ptyp_tuple tuple } ->
    let gens, vars_to_tuple = generate_tuple quoter tuple in
    [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e vars_to_tuple])]
  | { ptyp_loc } -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (Ppx_deriving.string_of_core_type typ)
and generate_tuple quoter ?name tuple =
  let vars = n_vars (List.length tuple) [] in
  let vars_tuple = List.map
      (fun i -> Ast_helper.Exp.mk @@
        Pexp_ident (Ast_convenience.lid i)) vars |> Ast_convenience.tuple
  in
  let vars_tuple = match name with
  | Some name -> Ast_helper.Exp.construct name (Some vars_tuple)
  | None -> vars_tuple
  in
  let fn_vars_to_tuple = List.fold_right last_fun vars vars_tuple in
  let gens = List.map (expr_of_typ quoter) tuple in
  gens, fn_vars_to_tuple

(* TODO: major cargo culting here, I have no idea how this works *)
let core_type_of_decl ?(lazing=false) ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match lazing with
  | true ->
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] Crowbar.gen Lazy.t])
    type_decl
    [%type: [%t typ] Crowbar.gen Lazy.t]
  | false ->
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
    | Ptype_record labels, _ -> (* parsetree.mli promises that this will be a
                                   non-empty list *)
      let gens = labels |> List.map (fun {pld_type} -> expr_of_typ quoter pld_type) in
      let vars = n_vars (List.length labels) [] in
      let field_assignments = labels |> List.mapi (fun n {pld_name} ->
        let lid = Ast_convenience.lid pld_name.txt in
        (lid, Ast_helper.Exp.ident @@ Ast_convenience.lid @@ List.nth vars n))
      in
      let record = Ast_helper.Exp.record field_assignments None in
      let fn_vars_to_record = List.fold_right last_fun vars record in
      lazify @@ [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e fn_vars_to_record])]
    | Ptype_variant constrs, _ ->
      let cases = constrs |> List.map (fun {pcd_name; pcd_res; pcd_args} ->
          (* under what circumstances can pcd_res be non-None and pcd_args be
             populated? *)
          match pcd_res, pcd_args with
          | None, Pcstr_tuple tuple ->
            let (gens, last_fun) = generate_tuple quoter
              ~name:(Ast_convenience.lid pcd_name.txt) tuple in
            [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e last_fun])]
          | Some core_type, Pcstr_tuple _ | Some core_type, Pcstr_record _ ->
            (* C: T0  or C: T1 * ... * Tn -> T0 or C: {...} -> T0 *)
            expr_of_typ quoter core_type (*
          | None, Pcstr_record labels ->
            (* C of {...} or C of {...} as t *) *)

        ) in
      (* we must be sure that there are generators for all of the possible
         variant types, and then invoke Crowbar.choose on the list of them. *)
      lazify @@ [%expr Crowbar.choose [%e (make_crowbar_list cases)]]
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type = Ppx_deriving.strong_type_of_type @@ core_type_of_decl ~lazing:true ~options
      ~path type_decl in
  let generate_var = pvar (Ppx_deriving.mangle_type_decl mangler type_decl) in
  (* the value binding for our generator *)
  [Vb.mk (Pat.constraint_ generate_var out_type)
     (Ppx_deriving.sanitize ~quoter (polymorphize generator));
  ]

let unlazify type_decl =
  let name = Ppx_deriving.mangle_type_decl mangler type_decl in
  let lazy_name = Ast_helper.Pat.lazy_ (Ast_helper.Pat.var (mknoloc name)) in
  let body = Exp.ident (Ast_convenience.lid name) in
  Str.value Nonrecursive [Vb.mk lazy_name body]

let deriver = Ppx_deriving.create deriver
    ~core_type:(Ppx_deriving.with_quoter (fun quoter typ -> expr_of_typ quoter
                                             typ))
    ~type_decl_str:(fun ~options ~path type_decls ->
        let bodies = List.concat (List.map (str_of_type ~options ~path) type_decls) in
        (Str.value Recursive bodies) ::
        (List.map unlazify type_decls))
    ()

let () = Ppx_deriving.register deriver
