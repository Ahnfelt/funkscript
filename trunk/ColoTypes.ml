type position = (int * int);;

type 'a pos = 'a * position;;

type pattern
    = PWildcard
    | PId of string
    | PUnit
    | PBool of bool
    | PNumber of float
    | PString of string
    | PList of pattern pos list * pattern pos option
    ;;

type expression 
    = EId of string
    | EUnit
    | EBool of bool
    | ENumber of float
    | EString of string
    | EList of expression pos list
    | ELambda of (pattern pos * expression pos) list * bool
    | ESequence of expression pos * expression pos
    | EApply of expression pos * expression pos
    | ELet of pattern pos * expression pos
    | ESet of pattern pos * expression pos
    ;;

type port = string * string list;;

type program = port pos list * port pos list * expression pos;;

