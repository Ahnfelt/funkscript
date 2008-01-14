type v
    = VFunction of (v -> v)
    | VList of v list
    | VString of string
    | VNumber of float
    | VBool of bool
    | VUnit
    ;;

exception E of v;;

let value_to_list (VList l) = l;;

let number_to_string n = let s = string_of_float n in
    if String.get s (String.length s - 1) = '.' 
    then String.sub s 0 (String.length s - 1)
    else s;;

let rec list_to_string l = match l with
    | f::n::r -> value_to_string f ^ ", " ^ list_to_string (n::r)
    | [f] -> value_to_string f
    | [] -> ""

and value_to_string v = match v with
    | VFunction _ -> "{}"
    | VList l -> "[" ^ list_to_string l ^ "]"
    | VString s -> "\"" ^ s ^ "\""
    | VNumber n -> number_to_string n
    | VBool b -> if b then "true" else "false"
    | VUnit -> "()";;
    
let rec range f t c s = if c f t 
    then VNumber f :: range (f +. s) t c s else [];;

let uchar_to_float c = let n = float_of_int (UChar.uint_code c) in
    if n >= 0.0 then n else float_of_int max_int -. n;;

let rec a l r = match l with
    | VFunction f -> f r
    | VList s -> (match r with
        | VString "Print" -> print_string (value_to_string l); print_newline (); VUnit
        | VString "ToString" -> VString (value_to_string l)
        | VString "Length" -> VNumber (float_of_int (List.length s))
        | VString "Reverse" -> VList (List.rev s)
        | VString "Head" -> List.hd s
        | VString "Tail" -> VList (List.tl s)
        | VString "Concatenate" -> VFunction (fun (VList t) -> VList (s @ t))
        | VString "Get" -> VFunction (fun (VNumber t) -> List.nth s (int_of_float t))
        | VString "Each" -> VFunction (fun (VFunction t) -> let f e = ignore (t e) in List.iter f s; VUnit)
        | VString "Map" -> VFunction (fun (VFunction t) -> VList (List.map t s))
        | VString "Filter" -> VFunction (fun (VFunction t) -> let f e = t e = VBool true in VList (List.filter f s))
        | VString "FoldLeft" -> VFunction (fun t -> VFunction (fun (VFunction u) -> 
            let f e1 e2 = let VFunction u2 = u e1 in u2 e2 in List.fold_left f t s))
        | VString "FoldRight" -> VFunction (fun t -> VFunction (fun (VFunction u) -> 
            let f e1 e2 = let VFunction u2 = u e1 in u2 e2 in List.fold_right f s t))
        | VString "Sort" -> VFunction (fun (VFunction t) -> let f e1 e2 = 
            if a (t e1) e2 = VBool true then -1 else
            if a (t e2) e1 = VBool true then 1 else 0 in VList (List.sort f s))
        | VString "All" -> VFunction (fun (VFunction t) -> 
            let f e = t e = VBool true in VBool (List.for_all f s))
        | VString "Exists" -> VFunction (fun (VFunction t) -> 
            let f e = t e = VBool true in VBool (List.exists f s))
        | VString "Equal" -> VFunction (fun v -> VBool (l = v))
        | _ -> raise (E (VString ("List cannot be applied to: " ^ value_to_string r)))
        )
    | VString s -> (match r with
        | VString "Print" -> print_string s; print_newline (); VUnit
        | VString "ToString" -> l
        | VString "ToNumber" -> VNumber (float_of_string s)
        | VString "Unicodes" -> let q = ref [] in 
            UTF8.iter (fun c -> q := VNumber (uchar_to_float c) :: !q) s; VList (List.rev !q)
        | VString "Length" -> VNumber (float_of_int (UTF8.length s))
        | VString "Uppercase" -> VString (String.uppercase s) (* NOTE: These only change ASCII characters *)
        | VString "Lowercase" -> VString (String.lowercase s)
        | VString "Capitalize" -> VString (String.capitalize s)
        | VString "Uncapitalize" -> VString (String.uncapitalize s)
        | VString "Concatenate" -> VFunction (fun (VString t) -> VString (s ^ t))
        | VString "Left" -> VFunction (fun (VNumber t) -> VString (String.sub s 0 (UTF8.nth s (int_of_float t))))
        | VString "Right" -> VFunction (fun (VNumber t) -> let last = UTF8.next s (UTF8.last s) in 
            let start = (UTF8.move s last (-int_of_float t)) in
            VString (String.sub s start (last - start)))
        | VString "Between" -> VFunction (fun (VNumber t) -> VFunction (fun (VNumber u) -> 
            let start = (UTF8.nth s (int_of_float t)) in
            let stop = (UTF8.nth s (int_of_float u)) in
            VString (String.sub s start (stop - start))))
        | VString "From" -> VFunction (fun (VNumber t) -> VFunction (fun (VNumber u) -> 
            let start = (UTF8.nth s (int_of_float t)) in
            VString (String.sub s start ((UTF8.move s start (int_of_float u)) - start))))
        (* NOTE: There can be different representations of the same character, is this handled? *)
        | VString "Less" -> VFunction (fun (VString t) -> VBool (UTF8.compare s t < 0))
        | VString "Greater" -> VFunction (fun (VString t) -> VBool (UTF8.compare s t > 0))
        | VString "LessEqual" -> VFunction (fun (VString t) -> VBool (UTF8.compare s t <= 0))
        | VString "LessGreater" -> VFunction (fun (VString t) -> VBool (UTF8.compare s t >= 0))
        | VString "Equal" -> VFunction (fun (VString t) -> VBool (UTF8.compare s t = 0))
        | _ -> raise (E (VString ("String cannot be applied to: " ^ value_to_string r)))
        )
    | VNumber n -> (match r with
        | VString "Print" -> print_string (number_to_string n); print_newline (); VUnit
        | VString "ToString" -> VString (number_to_string n)
        | VString "IsDefined" -> VBool (classify_float n <> FP_nan)
        | VString "IsPositiveInfinity" -> VBool (n = infinity)
        | VString "IsNegativeInfinity" -> VBool (n = neg_infinity)
        | VString "Negate" -> VNumber (-.n)
        | VString "Sign" -> VNumber (if n >= 0.0 then 1.0 else -.1.0)
        | VString "Floor" -> VNumber (floor n)
        | VString "Ceil" -> VNumber (ceil n)
        | VString "Round" -> VNumber (floor (n +. 0.5)) (* FIXME: This is probably wrong *)
        | VString "Min" -> VFunction (fun (VNumber t) -> VNumber (min n t))
        | VString "Max" -> VFunction (fun (VNumber t) -> VNumber (max n t))
        | VString "To" -> VFunction (fun (VNumber t) -> VList (range n t (<=) 1.0))
        | VString "ToStep" -> VFunction (fun (VNumber t) -> VFunction (fun (VNumber s) -> VList (range n t (<=) s)))
        | VString "Until" -> VFunction (fun (VNumber t) -> VList (range n t (<) 1.0))
        | VString "UntilStep" -> VFunction (fun (VNumber t) -> VFunction (fun (VNumber s) -> VList (range n t (<) s)))
        | VString "Mod" -> VFunction (fun (VNumber t) -> VNumber (mod_float n t))
        | VString "Add" -> VFunction (fun (VNumber t) -> VNumber (n +. t))
        | VString "Subtract" -> VFunction (fun (VNumber t) -> VNumber (n -. t))
        | VString "Multiply" -> VFunction (fun (VNumber t) -> VNumber (n *. t))
        | VString "Divide" -> VFunction (fun (VNumber t) -> VNumber (n /. t))
        | VString "Power" -> VFunction (fun (VNumber t) -> VNumber (n ** t))
        | VString "Less" -> VFunction (fun (VNumber t) -> VBool (n < t))
        | VString "Greater" -> VFunction (fun (VNumber t) -> VBool (n > t))
        | VString "LessEqual" -> VFunction (fun (VNumber t) -> VBool (n <= t))
        | VString "LessGreater" -> VFunction (fun (VNumber t) -> VBool (n >= t))
        | VString "Equal" -> VFunction (fun v -> VBool (l = v))
        | _ -> raise (E (VString ("Number cannot be applied to: " ^ value_to_string r)))
        )
    | VBool b -> (match r with
        | VString "Print" -> print_string (if b then "true" else "false"); print_newline (); VUnit
        | VString "ToString" -> VString (if b then "true" else "false")
        | VString "AndThen" -> VFunction (fun (VFunction t) -> VBool (b && let (VBool r) = t VUnit in r))
        | VString "OrElse" -> VFunction (fun (VFunction t) -> VBool (b || let (VBool r) = t VUnit in r))
        | VString "And" -> VFunction (fun (VBool t) -> VBool (b && t))
        | VString "Or" -> VFunction (fun (VBool t) -> VBool (b || t))
        | VString "Xor" -> VFunction (fun (VBool t) -> VBool ((b && not t) || (t && not t)))
        | VString "Not" -> VBool (not b)
        | VString "Then" -> VFunction (fun f -> if b then a f VUnit else VUnit)
        | VString "Else" -> VFunction (fun f -> if b then VUnit else a f VUnit)
        | VString "ThenElse" -> VFunction (fun f1 -> VFunction (fun f2 -> if b then a f1 VUnit else a f2 VUnit))
        | VString "Equal" -> VFunction (fun v -> VBool (l = v))
        | _ -> raise (E (VString ("Bool cannot be applied to: " ^ value_to_string r)))
        )
    | VUnit -> (match r with
        | VString "Print" -> print_string "()"; print_newline (); VUnit
        | VString "ToString" -> VString "()"
        | VString "Equal" -> VFunction (fun v -> VBool (v = VUnit))
        | _ -> raise (E (VString ("Unit cannot be applied to: " ^ value_to_string r)))
        )
    ;;


