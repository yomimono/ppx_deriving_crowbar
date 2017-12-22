module type Krampus = sig
  type binary = | Yes | No | File_not_found
  type nest = {
    enabled : binary;
    super_secure_password : string;
  }
end [@@deriving crowbar]

let () =
  Crowbar.(add_test ~name:"files are always found"
             [Krampus_to_crowbar.binary_to_crowbar] (function
                 | Yes -> check true
                 | No -> check true
                 | File_not_found -> raise (Failure "oh no")
               ))
