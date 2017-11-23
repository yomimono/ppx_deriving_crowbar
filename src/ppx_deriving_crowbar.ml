open Location
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience

let deriver = "crowbar"
let raise_errorf = Ppx_deriving.raise_errorf

(* currently we ignore all options *)
let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
      | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s"
               deriver name)

let mangler = Ppx_deriving.(`Suffix "to_crowbar")
let unlazify_attribute_name = "crowbar_recursive_typedef_please_unlazy"
  (* TODO: actually make sure this is unique *)

let attr_generator attrs =
  Ppx_deriving.(attrs |> attr ~deriver "generator" |> Arg.(get_attr ~deriver expr))

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

let lazify e = [%expr lazy [%e e]]

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  match attr_generator typ.ptyp_attributes with
  | Some generator -> Ppx_deriving.quote quoter generator
  | None ->
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
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
    | [%type: bytes]
    | [%type: Bytes.t] -> [%expr Crowbar.(map [bytes] Bytes.of_string)]
    | [%type: nativeint]
    | [%type: Nativeint.t] -> [%expr Crowbar.(map [int] Nativeint.of_int)]
    (* TODO: polymorphic variants *)
    (* also TODO: do we DTRT for [@nobuiltin]?  nope. *)
    | [%type: [%t? typ] option] ->
      [%expr Crowbar.(map [bool; [%e expr_of_typ typ]]
                        (fun a b -> if a then Some b else None))]
    | [%type: [%t? typ] ref] ->
      [%expr Crowbar.(map [[%e expr_of_typ typ]] (fun a -> ref a))]
    | [%type: [%t? typ] list] ->
      [%expr Crowbar.(list [%e expr_of_typ typ])]
    | [%type: [%t? typ] array] ->
      [%expr Crowbar.(map [list [%e expr_of_typ typ]] Array.of_list)]
    | [%type: [%t? typ] lazy_t]
    | [%type: [%t? typ] Lazy.t] ->
      [%expr Crowbar.(map [[%e expr_of_typ typ]] (fun a -> lazy a))]
    | [%type: ([%t? ok_t], [%t? err_t]) result]
    | [%type: ([%t? ok_t], [%t? err_t]) Result.result] ->
      [%expr Crowbar.(map [bool; [%e expr_of_typ ok_t]; [%e expr_of_typ err_t]]
             (fun b x y ->
               if b then (Result.Ok x)
               else (Result.Error y)
             ))
      ]

    | _ ->
    let fwd = app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid mangler lid)))
        (List.map expr_of_typ args)
    in
    let matches (loc, _) = (0 = String.compare loc.txt unlazify_attribute_name) in
    match List.exists matches typ.ptyp_attributes with
    | true -> [%expr Crowbar.unlazy [%e fwd]]
    | false -> [%expr [%e fwd]]
    end
  | { ptyp_desc = Ptyp_tuple tuple } ->
    let gens, vars_to_tuple = generate_tuple quoter tuple in
    [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e vars_to_tuple])]
  | { ptyp_desc = Ptyp_var name } -> Ast_convenience.evar ("poly_"^name)
  | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
  | { ptyp_desc = Ptyp_variant (fields, openness, labels);ptyp_loc} ->
        (* I think we don't care about open vs closed, we just want to wrap thee
           things in the right rows; similarly we don't care about labels *)
        (* just like for non-poly variants, we need to choose from the set of
           available things (which we can't get more clues about than this here
           typedef... hm, unless the labels are clues, actually; TODO think
           about that a bit more, I think they're not but make sure). *)
    let translate = function
      | Rinherit typ -> expr_of_typ typ
      | Rtag (label, attrs, _, []) ->
        (* nullary, just use the label name *)
        [%expr Crowbar.const [%e Ast_helper.Exp.variant label None]]
      | Rtag (label, attrs, _, [{ptyp_desc = Ptyp_tuple tuple}]) ->
        (* good ol' tuples *)
        let (gens, last_fun) =
          generate_tuple quoter
            ~constructor:(Ast_helper.Exp.variant label) tuple in
        [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e last_fun])]
      | Rtag (label, attrs, _, [typ] (* one non-tuple thing *)) ->
        let var = "a" in
        let body = Ast_helper.Exp.(variant label
            (Some (ident @@ Ast_convenience.lid var))) in
        let fn = last_fun var body in
        [%expr Crowbar.(map [[%e expr_of_typ typ]] [%e fn])]
                                                 
      | Rtag (_,_,_,_) -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (Ppx_deriving.string_of_core_type typ)
    in
    let cases = List.map translate fields in
    [%expr Crowbar.choose [%e (make_crowbar_list cases)]]
  | { ptyp_loc } -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (Ppx_deriving.string_of_core_type typ)
and generate_tuple quoter ?constructor tuple =
  let vars = n_vars (List.length tuple) [] in
  let vars_tuple = List.map Ast_convenience.evar vars |> Ast_convenience.tuple in
  let vars_tuple = match constructor with
  | Some constructor -> constructor (Some vars_tuple)
  | None -> vars_tuple
  in
  let fn_vars_to_tuple = List.fold_right last_fun vars vars_tuple in
  let gens = List.map (expr_of_typ quoter) tuple in
  gens, fn_vars_to_tuple

let core_type_of_decl ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] Crowbar.gen])
    type_decl
    [%type: [%t typ] Crowbar.gen Lazy.t]

let str_of_type ~options ~path ({ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  (* TODO: generalize this to "a list of things that have a type and attributes"
     rather than labels; we could use it more generally *)
  let gens_and_fn_of_labels ?name labels =
    let gens = labels |> List.map (fun {pld_type; pld_attributes} ->
        match attr_generator pld_attributes with
        | Some generator -> generator
        | None -> expr_of_typ quoter pld_type) in
    let vars = n_vars (List.length labels) [] in
    let field_assignments = labels |> List.mapi (fun n {pld_name} ->
      let lid = Ast_convenience.lid pld_name.txt in
      (lid, Ast_helper.Exp.ident @@ Ast_convenience.lid @@ List.nth vars n))
    in
    let record = Ast_helper.Exp.record field_assignments None in
    let record = match name with
    | None -> record
    | Some name -> Ast_helper.Exp.construct name (Some record)
    in
    let fn_vars_to_record = List.fold_right last_fun vars record in
    (gens, fn_vars_to_record)
  in
  let generator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_open, _ -> raise_errorf "%s cannot be derived for open type" deriver (* TODO: can we do better? *)
    | Ptype_abstract, Some manifest ->
      expr_of_typ quoter manifest
    | Ptype_abstract, None -> begin
        match attr_generator type_decl.ptype_attributes with
        | Some generator -> generator
        | None ->
          (* we have a ptype_name foo, so try foo_to_crowbar in the namespace *)
          app (Exp.ident (Ast_convenience.lid
                            (Ppx_deriving.mangle_type_decl mangler type_decl)))
          []
      end
    | Ptype_record labels, _ -> (* parsetree.mli promises that this will be a
                                   non-empty list *)
      let (gens, fn_vars_to_record) = gens_and_fn_of_labels labels in
      [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e fn_vars_to_record])]
    | Ptype_variant constrs, _ ->
      let cases = constrs |>
                  List.map (fun {pcd_attributes; pcd_name; pcd_res; pcd_args} ->
          (* under what circumstances can pcd_res be non-None and pcd_args be
             populated? *)
          match pcd_res, pcd_args with
          | None, Pcstr_tuple [] ->

            let name = Ast_convenience.constr pcd_name.txt [] in
            [%expr Crowbar.(const [%e name])]
          | None, Pcstr_tuple tuple ->
            let (gens, last_fun) = generate_tuple quoter
                ~constructor:(
                  Ast_helper.Exp.construct @@ Ast_convenience.lid pcd_name.txt)
                    tuple in
            [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e last_fun])]
          | Some core_type, Pcstr_tuple _ | Some core_type, Pcstr_record _ ->
            (* C: T0  or C: T1 * ... * Tn -> T0 or C: {...} -> T0 *)
            expr_of_typ quoter core_type
          | None, Pcstr_record labels ->
            (* C of {...} or C of {...} as t *)
            let gens, fn_vars_to_record = gens_and_fn_of_labels
              ~name:(Ast_convenience.lid pcd_name.txt) labels in
            [%expr Crowbar.(map [%e (make_crowbar_list gens)] [%e fn_vars_to_record])]
        ) in
      (* we must be sure that there are generators for all of the possible
         variant types, and then invoke Crowbar.choose on the list of them. *)
      [%expr Crowbar.choose [%e (make_crowbar_list cases)]]
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type = Ppx_deriving.strong_type_of_type @@ core_type_of_decl ~options
      ~path type_decl in
  let generate_var = pvar (Ppx_deriving.mangle_type_decl mangler type_decl) in
  [Vb.mk (Pat.constraint_ generate_var out_type)
     (Ppx_deriving.sanitize ~quoter (polymorphize (lazify generator)));
  ]

let tag_recursive_for_unlazifying type_decls =
  let add_tag core_type =
    let loc = Location.mknoloc unlazify_attribute_name in
    let payload : Parsetree.payload =
       (PStr [(Ast_helper.Str.mk @@ Pstr_eval ([%expr "Crowbar.unlazy"], []))]) in
    let new_tag : Parsetree.attribute = loc, payload in
    Ast_helper.Typ.attr core_type new_tag
  in
  let rec tag_on_match (needle : type_declaration) core_type =
    let core_type = match core_type.ptyp_desc with
      | Ptyp_constr (name, args) ->
        (* need to tag the top-level thing too, if it matches *)
        let core_type =
          if (0 = String.compare (Longident.last name.txt) needle.ptype_name.txt)
          then add_tag core_type
          else core_type
        in
        {core_type with ptyp_desc =
                        Ptyp_constr (name, List.map (tag_on_match needle) args)}
      | Ptyp_tuple l -> {core_type with ptyp_desc =
                                 Ptyp_tuple (List.map (tag_on_match needle) l)}
    | _ -> core_type
    in
    if (0 = String.compare (Ppx_deriving.string_of_core_type core_type) needle.ptype_name.txt)
    then add_tag core_type
    else core_type
  in
  let rec descender needle type_decl =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      {type_decl with ptype_manifest = Some (tag_on_match needle manifest) }
    | Ptype_abstract, None -> type_decl
    | Ptype_record labels, _ ->
      let check label = { label with pld_type = (tag_on_match needle label.pld_type)} in
      let labels = List.map check labels in
      {type_decl with ptype_kind = (Ptype_record labels)}
    | Ptype_variant constrs, _ ->
      let constrs = constrs |> List.map @@ fun constr ->
        match constr.pcd_res with
        | Some core_type -> {constr with pcd_res = Some (tag_on_match needle core_type)}
        | None -> match constr.pcd_args with
          | Pcstr_tuple tuple ->
            { constr with pcd_args = Pcstr_tuple (List.map (tag_on_match needle) tuple) }
          | Pcstr_record labels ->
            let check label = { label with
                                pld_type = (tag_on_match needle label.pld_type)}
            in
            { constr with pcd_args = Pcstr_record (List.map check labels)}
      in
      {type_decl with ptype_kind = (Ptype_variant constrs)}
    | Ptype_open, _ -> type_decl (* TODO: I don't know what else we could do here *)
  in
  (* each top-level element in the list has to be fully considered with respect
     to both itself and other items *)
  List.fold_left (fun l needle -> List.map (descender needle) l) type_decls type_decls

let unlazify type_decl =
  let name = Ppx_deriving.mangle_type_decl mangler type_decl in
  let fn_name_ident = Exp.ident (Ast_convenience.lid name) in
  let args = Ppx_deriving.fold_right_type_decl
      (fun str args -> (Asttypes.Nolabel, Exp.ident (Ast_convenience.lid
                                                       ("poly_"^str)))::args)
      type_decl []
  in
  match args with
  | [] ->
    let lazy_name = Ast_helper.Pat.lazy_ (Ast_helper.Pat.var (mknoloc name)) in
    Str.value Nonrecursive [Vb.mk lazy_name (Ast_convenience.evar name)]
  | args ->
    let apply_fn = Exp.apply fn_name_ident args in
    (* TODO: we assume Lazy has not been shadowed :/ *)
    let lazy_fn = Exp.apply (Exp.ident (Ast_convenience.lid "Lazy.force"))
      [Asttypes.Nolabel, apply_fn] in
    let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
    Str.value Nonrecursive [Vb.mk (pvar name) (polymorphize lazy_fn)]

let deriver = Ppx_deriving.create deriver
    ~core_type:(Ppx_deriving.with_quoter
                  (fun quoter typ -> expr_of_typ quoter typ))
    ~type_decl_str:(fun ~options ~path type_decls ->
        let type_decls = tag_recursive_for_unlazifying type_decls in
        let bodies = List.concat (List.map (str_of_type ~options ~path) type_decls) in
        (Str.value Recursive bodies) ::
        (List.map unlazify type_decls))
    ()

let () = Ppx_deriving.register deriver
