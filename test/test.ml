type snorp = int
[@@deriving crowbar]

type foo = A of int | B of float | C of quux
and quux = Q of int | R of foo | D of foo list
[@@deriving crowbar]

type ploomple = int option * float * bool ref
[@@deriving crowbar]

type fkeen = | A of int
and meep = | B of fkeen
[@@deriving crowbar]

type homp = (int * float)
and pnorst = (homp * int)
and knipp = (string * pnorst)
and florn = | Fjnie of knipp
[@@deriving crowbar]

type bar = {
  justice: bool;
  purrs: int array;
  fangs: ploomple;
  clorntro: string Lazy.t;
  rejweo: quux;
}
[@@deriving crowbar]
type 'a norple = {
  kwijwor : int;
  nipstel : 'a;
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

let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [generate_foo; generate_bar; generate_quux] (fun foo _bar _quux -> check @@ match foo with
      | A i -> Printf.printf "an int: %d\n%!" i; true
      | B f -> Printf.printf "a float: %f\n%!" f; true
      | C _ -> (Printf.printf "OMG, something exotic!\n%!"); true)
          )
