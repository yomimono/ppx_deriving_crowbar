type foo = A of int | B of float | C of quux
and quux = Q of int | R of foo
[@@deriving crowbar]

type ploomple = int * float
[@@deriving crowbar]

(* 
type bar = {
  justice: bool;
  purrs: int;
  fangs: ploomple;
  rejweo: quux;
}
[@@deriving crowbar]
let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [generate_foo] (fun foo -> check @@ match foo with
      | A i -> Printf.printf "an int: %d\n%!" i; true
      | B f -> Printf.printf "a float: %f\n%!" f; true
      | C _ -> (Printf.printf "OMG, something exotic!\n%!"); true)
          ) *)
