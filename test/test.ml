(* type snorp

type foo = A of snorp [@generator Crowbar.const 2] | B of quux
and quux = Q of int | R of foo | D of foo list
[@@deriving crowbar] *)

type ploomple = int option * float * bool ref
[@@deriving crowbar]

type fkeen = | A of int
and meep = | B of fkeen
[@@deriving crowbar]

let homp_to_crowbar = Crowbar.const (2, 2.)

module Rdjeimbo = struct
  type homp = (int * float) [@nobuiltin]
  and pnorst = (homp * int)
  and knipp = (string * pnorst)
  and florn = | Fjnie of knipp
  [@@deriving crowbar]
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

let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [foo_to_crowbar; bar_to_crowbar; quux_to_crowbar]
             (fun foo _bar _quux -> check @@ match foo with
      | A 2 -> true
      | A i -> false
      | B f -> Printf.printf "a float: %f\n%!" f; true
      | C _ -> (Printf.printf "OMG, something exotic!\n%!"); true)
          )
