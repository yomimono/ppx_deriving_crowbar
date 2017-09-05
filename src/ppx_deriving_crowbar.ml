(* "A type deriving function accepts a type and returns a corresponding derived expression" *)

(* So what's a core type? A type extension?  Those must come from Parsetree.
   ...except no, they don't, even though somewhere in here it's specifically
   called out as Parsetree.core_type ???  No, it's in there or the one we'll
   actually look at, it's a record with a description and a location and
   addributes I guess. *)

(* OK, what can we do with that? *)

(* Probably make some expressions :) *)

(* In order to make one, I need a location; presumably ppx_deriving has nice
   ways to fill that in for me? *)

let exp = Parsetree.({
    pexp_desc = Pexp_constant (Pconst_char 'a');
    pexp_loc = Location.none;
    pexp_attributes = [] })

let always = fun _ -> exp

let deriver = Ppx_deriving.create "crowbar" ~core_type:always ()

