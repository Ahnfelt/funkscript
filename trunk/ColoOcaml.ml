(*
    TODO: Module system
    TODO: Standard library
    TODO: Unicode
    TODO: Better underscore: 
        Where {_ Mod 2 = 0 || _ Floor <> _}
        fails because of the implicit lambda around _ Floor <> _
*)

open ColoTypes;;

exception UpdatePatternTooComplex;;

let rec separate s l = match l with
    | e::n::r -> e ^ s ^ separate s (n::r)
    | [e] -> e
    | [] -> "";;

let rec format_pattern pattern = match fst pattern with
    | PList (l, Some r) ->
        let (lss, lvs) = List.split (List.map format_pattern l) in 
        let (rs, rv) = format_pattern r in
        ("(VList (" ^ separate " :: " lss ^ (if l <> [] then " :: " else "") ^ rs ^ ")", rv @ List.flatten lvs)
    | PList (l, None) -> 
        let (lss, lvs) = List.split (List.map format_pattern l) in 
        ("(VList (" ^ separate " :: " lss ^ (if l <> [] then " :: " else "") ^ "[]))", List.flatten lvs)
    | PString s -> ("(VString " ^ "\"" ^ s ^ "\"" ^ ")", [])
    | PNumber n -> ("(VNumber " ^ string_of_float n ^ ")", [])
    | PBool b -> ("(VBool " ^ (if b then "true" else "false") ^ ")", [])
    | PId i -> ("c_" ^ i, [i])
    | PWildcard -> ("_", [])
    | PUnit -> ("VUnit", []);;

let rec format_vars l b m a = match l with
    | v::r -> b ^ "v_" ^ v ^ m ^ "c_" ^ v ^ a ^ format_vars r b m a
    | [] -> "";;

let rec format_match (p, e) = 
    let (s, v) = format_pattern p in 
    "| " ^ s ^ " -> " ^ format_vars v "let " " = ref " " in " ^ format_expression e

and format_before_mutual i = "let v_" ^ i ^ " = ref VUnit in\n"
and format_after_mutual i e = "v_" ^ i ^ " := " ^ format_expression e ^ ";\n"

and format_mutual e is es = match fst e with
    | ESequence ((ELet ((PId i, _), ((ELambda _, _) as x)), _), n) ->
        format_mutual n (is ^ format_before_mutual i) (es ^ format_after_mutual i x)
    | ELet ((PId i, _), ((ELambda _, _) as e)) -> is ^ format_before_mutual i ^ es ^ format_after_mutual i e ^ "VUnit; "
    | _ -> is ^ es ^ format_expression e

and format_expression expression_pos = match fst expression_pos with
    | ESequence ((ELet ((PId _, _), (ELambda _, _)), _), _) as e -> format_mutual (e, snd expression_pos) "" ""
    | ESequence ((ELet _, _) as l, r) -> format_expression l ^ "\n" ^ format_expression r
    | ESequence (l, r) -> format_expression l ^ ";\n" ^ format_expression r
    | ELet ((PId i, _), ((ELambda _, _) as e)) -> format_before_mutual i ^ format_after_mutual i e ^ "VUnit; "
    | ELet (p, e) -> let (s, v) = format_pattern p in 
        "let " ^ s ^ " = " ^ format_expression e ^ " in " ^ format_vars v "let " " = ref " " in "
    | ESet ((PId i, _), e) -> "v_" ^ i ^ " := " ^ format_expression e ^ "; VUnit "
    | ESet (p, e) -> let (s, v) = format_pattern p in 
        "let " ^ s ^ " = " ^ format_expression e ^ " in " ^ format_vars v "" " := " "; " ^ "VUnit"
    | EList (l, Some r) -> "(VList ((" ^ separate " :: " (List.map format_expression l) ^ 
        " :: []) @ value_to_list (" ^ format_expression r ^ ")))"
    | EList (l, None) -> "(VList (" ^ separate " :: " (List.map format_expression l) ^ " :: []))"
    | ELambda (m, o) -> "(" ^ (if o then "let rec v___ = ref " else "") ^ "(VFunction (fun p -> " ^ 
        (if m = [] then " VUnit" else "match p with\n" ^ separate "\n" (List.map format_match m)) ^ 
        (if o then ")) in !v___)" else ")))")
    | EString s -> "(VString " ^ "\"" ^ s ^ "\"" ^ ")"
    | ENumber n -> "(VNumber " ^ string_of_float n ^ ")"
    | EBool b -> "(VBool " ^ (if b then "true" else "false") ^ ")"
    | EId i -> "!v_" ^ i
    | EApply (l, r) -> "(a " ^ format_expression l ^ " " ^ format_expression r ^ ")"
    | EUnit -> "VUnit";;

let format (_, _, expression) =
    "open ColoOcamlInclude;; let _ = try ignore (\n" ^
    (format_expression expression) ^ 
    ")\nwith E m -> print_string (\"Exception: \" ^ value_to_string m ^ \"\n\");;";;

