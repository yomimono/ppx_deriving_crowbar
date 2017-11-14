type foo = A of int | B of float | C of int * float
[@@deriving crowbar]

type quux = Q of int | B of foo
[@@deriving crowbar]

type ploomple = int * float
[@@deriving crowbar]

(* type bar = {
  justice: bool;
  purrs: int;
}
[@@deriving crowbar] *)
let () =
  Crowbar.(add_test ~name:"everything is awesome"
             [generate_foo] (fun foo -> check @@ match foo with
      | A i -> Printf.printf "an int: %d\n%!" i; true
      | B f -> Printf.printf "a float: %f\n%!" f; true
      | C (i, f) -> (Printf.printf "OMG, an int AND a float: %d %f\n%!" i f); true)
          )
