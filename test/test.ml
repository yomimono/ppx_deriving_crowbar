module Glorn : sig
  type snorp
end = struct
  type snorp = int
   end

type plot = int [@generator Crowbar.const 1]
[@@deriving crowbar]

type foo = A of int [@generator (Crowbar.const (A 2))]
         | B of int [@generator Crowbar.(map [Crowbar.float] (fun f -> B (int_of_float f)))]
and quux = Q of int | R of foo | D of foo list
[@@deriving crowbar]

type ploomple = int option * float * bool ref
                  [@generator Crowbar.const (None, 4., ref false)]
[@@deriving crowbar]

type strorple = [
    `Thorcla of string
  | `Mlorstri of int
] and clist = [
    `Omon
  | `Kilder of bool * strorple list
  ] [@@deriving crowbar]

let q = `Thorcla "spiders"

type knorp = {
  a : float [@generator Crowbar.const 4.]
}
[@@deriving crowbar]

type fkeen = | A of int
and meep = | B of fkeen
[@@deriving crowbar]

module Rdjeimbo = struct
  type homp = (int * float)
  and pnorst = (homp * int)
  and knipp = (string * pnorst)
  and florn = | Fjnie of knipp
  [@@deriving crowbar]

  type t = pnorst * homp [@@deriving crowbar]
end

module Clorstro = struct
  type t = [ `Morgthorp of Rdjeimbo.t ] [@@deriving crowbar]
end

type bar = {
  justice: bool;
  purrs: int array;
  fangs: ploomple;
  clorntro: string Lazy.t;
  rejweo: quux;
}
[@@deriving crowbar]

type clippy = | A of int | B of {a: int; b: float}
[@@deriving crowbar]

type oops = (int, string) result
[@@deriving crowbar]

type disaster = | A of (int, string) result
[@@deriving crowbar]

type oh_no = ((int * float), string) result
[@@deriving crowbar]

type hlifd = {b: (int, string) result;}
[@@deriving crowbar]

type 'a norple = {
  kwijwor : int;
  nipstel : 'a;
}
and pune = | A
and plongle = pune norple
[@@deriving crowbar]

type krord =
  | Empty
  | Nonempty of hlifd list [@generator Crowbar.(map [list1 hlifd_to_crowbar] (fun l -> Nonempty l))]
[@@deriving crowbar]

type stdord =
  | Empty
  | Nonempty of hlifd list
[@@deriving crowbar { nonempty = true }]

type poly_variant = [
  | `A of int [@generator Crowbar.const (`A 0)]
]
[@@deriving crowbar]


let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [foo_to_crowbar; bar_to_crowbar; quux_to_crowbar]
             (fun foo _bar _quux -> check @@ match foo with
      | A 2 -> true
      | A i -> false
      | _ -> true
             ));
  Crowbar.(add_test ~name:"nothing is bad"
             [stdord_to_crowbar] (function
                 | Nonempty [] -> Crowbar.fail "ugh"
                 | _ -> ()));
  Crowbar.(add_test ~name:"polymorphic variants"
             [poly_variant_to_crowbar] (function
                 | `A 0 -> ()
                 | _ -> Crowbar.fail "bah"));
