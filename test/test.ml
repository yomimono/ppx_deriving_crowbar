type foo = A of int | B of float | C of quux
and quux = Q of int | R of foo
[@@deriving crowbar]

type ploomple = int option * float * bool ref
[@@deriving crowbar]

type bar = {
  justice: bool;
  purrs: int;
  fangs: ploomple;
  rejweo: quux;
}
[@@deriving crowbar]

type clippy = | A of int | B of {a: int; b: float}
[@@deriving crowbar]

let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [generate_foo; generate_bar; generate_quux] (fun foo _bar _quux -> check @@ match foo with
      | A i -> Printf.printf "an int: %d\n%!" i; true
      | B f -> Printf.printf "a float: %f\n%!" f; true
      | C _ -> (Printf.printf "OMG, something exotic!\n%!"); true)
          )
