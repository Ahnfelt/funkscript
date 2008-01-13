open ColoTypes;;

let format_float f = 
    let s = string_of_float f in
        if String.get s (String.length s - 1) = '.' 
        then String.sub s 0 (String.length s - 1)
        else s;;
    
let rec separate s l = match l with
    | e::n::r -> e ^ s ^ separate s (n::r)
    | [e] -> e
    | [] -> "";;

let rec format_pattern pattern = match fst pattern with
    | PList (l, Some r) -> "[" ^ separate ", " (List.map format_pattern l) ^ " :: " ^ format_pattern r ^ "]"
    | PList (l, None) -> "[" ^ separate ", " (List.map format_pattern l) ^ "]"
    | PString s -> "\"" ^ s ^ "\""
    | PNumber n -> format_float n
    | PBool b -> if b then "true" else "false"
    | PId i -> i
    | PUnit -> "()";;

let rec format_match (p, e) = "|" ^ format_pattern p ^ "| " ^ format_expression e

and format_expression expression = match fst expression with
    | ELet (p, e) -> ":" ^ format_pattern p ^ " " ^ format_expression e
    | ESet (p, e) -> "!" ^ format_pattern p ^ " " ^ format_expression e
    | ESequence (l, r) -> format_expression l ^ ";\n" ^ format_expression r
    | EList (l, Some r) -> "[" ^ separate ", " (List.map format_expression l) ^ " :: " ^ format_expression r ^ "]"
    | EList (l, None) -> "[" ^ separate ", " (List.map format_expression l) ^ "]"
    | ELambda (m, _) -> "{" ^ separate "\n" (List.map format_match m) ^ "}"
    | EString s -> "\"" ^ s ^ "\""
    | ENumber n -> format_float n
    | EBool b -> if b then "true" else "false"
    | EId i -> i
    | EApply (l, r) -> "(" ^ format_expression l ^ " " ^ format_expression r ^ ")"
    | EUnit -> "()";;

let rec format_port ports prefix = match ports with
    | port::rest -> 
        let p = fst port in 
            prefix ^ fst p ^ "[" ^ 
            separate ", " (snd p) ^ "]\n" ^ 
            format_port rest prefix
    | [] -> "";;        

let format (exports, imports, expression) =
    (format_port exports "<- ") ^ 
    (format_port imports "-> ") ^
    (format_expression expression);;

