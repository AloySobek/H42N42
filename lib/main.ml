let myservice =
  Eliom_service.create
    ~path:(Eliom_service.Path [""]) (* Root *)
    ~meth:(Eliom_service.Get (Eliom_parameter.(string "myparam" ** int "i")))
    ()

let () =
  Eliom_registration.Html.register ~service:myservice
    (fun (myparam, _i) () ->
      Lwt.return
         Eliom_content.Html.F.(html (head (title (txt "")) [])
         (body [h1 [txt myparam]; div [h1 [txt "Hello from here"]]])))

open List

(** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

let rec last = function
    | [] -> None
    | [l] -> Some l
    | _ :: t -> last t

let rec last_two = function
    | [] -> None
    | _ :: [] -> None
    | x :: y :: [] -> Some (x, y)
    | _ :: t -> last_two t

let rec nth lst i = match lst with
    | [] -> None
    | h :: t -> if i > 0 then nth t (i - 1) else Some h

let length lst =
    let rec _length lst acc = match lst with
        | [] -> acc
        | _ :: t -> _length t (acc + 1)
    in _length lst 0

let rec rev lst = match lst with
    | [] -> []
    | h :: t -> rev t @ [h]

type 'a tree =
    | Leaf
    | Node of {data: 'a; left: 'a tree; right: 'a tree}

let prod lst = let rec _prod lst acc =
    match lst with
        | [] -> acc
        | h :: t -> _prod t (acc * h)
    in _prod lst 1

let conc lst = let rec _conc lst acc =
    match lst with
        | [] -> acc
        | h :: t -> _conc t (acc ^ h)
    in _conc lst ""

let first_is_bigred lst = match lst with
    | [] -> false
    | h :: _ -> if h = "bigred" then true else false

let length_of_two_or_four lst = match lst with
    | []-> false
    | [_; _] -> true
    | [_; _; _; _] -> true
    | _ :: _ -> false

let first_two_are_equal lst = match lst with
    | [] -> false
    | [_] -> false
    | f :: s :: _ -> f = s 

let fifth_of_list lst  = if List.length lst < 5 then 0 else List.nth lst 4

let desc_sort lst = List.sort (fun a b -> if a > b then -1 else if a < b then 1 else 0) lst

let last_of_list lst = List.nth lst ((List.length lst) - 1)

let any_zeroes lst = List.exists (fun x -> x = 0) lst

let take n lst =
    let rec _take n lst res = 
        match lst with 
            | [] -> res
            | h :: t -> if n > 0 then _take (n - 1) t (res @ [h]) else res
    in _take n lst []

let rec take2 n lst =
    match lst with 
    | [] -> []
    | h :: t -> if n > 0 then [h] @ take2 (n - 1) t else []

let rec drop n lst =
    match lst with
        | [] -> []
        | _ :: t -> if n > 1 then drop (n - 1) t else t
        
let is_unimodal lst =
    let rec _is_unimodal lst asc =
        match lst with
            | [] -> true
            | [_] -> true
            | x :: y :: t -> 
                if y > x && not asc then
                    false
                else if y < x && asc then
                    _is_unimodal lst false
                else
                    _is_unimodal (y :: t) asc
    in _is_unimodal lst true

let powerset = ()

let rec print_int_list lst = match lst with
    | [] -> ()
    | h :: t -> print_endline (string_of_int h); print_int_list t

let print_int_list' lst = match lst with
    | [] -> ()
    | _ -> List.iter (fun i -> print_endline (string_of_int i)) lst

type student = {first_name : string; last_name : string; gpa : float}

let s = {first_name = "Foo"; last_name = "Bar"; gpa = 3.5}

let get_name = function
    | {first_name; last_name; _} -> first_name ^ " " ^ last_name

let create_student first_name last_name gpa = {first_name = first_name; last_name = last_name; gpa = gpa}
