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
    | _ ->
    let fwd = app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "generate") lid)))
        (List.map expr_of_typ args)                                         
    in                                                                          
    [%expr [%e fwd]]   
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

let make_crowbar_list l =
  let consify add_me extant =
      Ast_helper.Exp.(construct (Ast_convenience.lid "Crowbar.::")
                        (Some (tuple [add_me; extant])))
  in
  List.fold_right consify l (Ast_helper.Exp.construct (Ast_convenience.lid
                                                 "Crowbar.[]") None)

let str_of_type ~options ~path ({ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  let generator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      expr_of_typ quoter manifest
    | Ptype_record labels, _ -> (* parsetree.mli promises that this will be a
                                   non-empty list *)
      let gens = labels |> List.map (fun {pld_type;} -> expr_of_typ quoter
                                        pld_type) in
      Ast_convenience.tuple gens
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
            let vars = n_vars (List.length tuple) [] in
            (* make a tuple of those names *)
            let desc = List.map
                 (fun i -> Ast_helper.Exp.mk @@
                   Pexp_ident (Ast_convenience.lid i)) vars
            in
            let res = Ast_helper.Exp.construct (Ast_convenience.lid pcd_name.txt)
                (Some (Ast_convenience.tuple desc))
            in
            let last_fun arg function_body = Ast_helper.Exp.fun_ Nolabel None
                (Ast_helper.Pat.var (Location.mknoloc arg))
                function_body in
            let last_fun = List.fold_right last_fun vars res in
            let gens = List.map (expr_of_typ quoter) tuple in
            (* how do we get all of these gens into the right structure for
               Crowbar?  It just looks like a list, it isn't really one. *)
            [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e last_fun])]
          | Some core_type, Pcstr_tuple _ | Some core_type, Pcstr_record _ ->
            (* C: T0  or C: T1 * ... * Tn -> T0 or C: {...} -> T0 *)
            expr_of_typ quoter core_type (* 
          | None, Pcstr_record labels ->
            (* C of {...} or C of {...} as t *) *)

        ) in
      [%expr Crowbar.choose [%e (make_crowbar_list cases)]]
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
