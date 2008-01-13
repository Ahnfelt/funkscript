%{
open ColoTypes;;

let parse_error s =
  let f = "" in
  let l = string_of_int 0 in 
  let c = string_of_int 0 in
      print_endline ("[" ^ f ^ ":" ^ l ^ ":" ^ c ^ "] " ^ s);
      flush stdout;;

let operator o l r p = (EApply ((EApply (l, (EString o, p)), p), r), p);;  

let pick_semi (lp, la) (rp, ra) =
    if la && ra = false then (rp, ra) else (lp, la);;
    
let rec simple_patterns l e = match l with
    | ((PId _, _) as f)::n::r -> (f, (ELambda ([simple_patterns (n::r) e], false), snd f))
    | [(PId _, _) as f] -> (f, e)
    | ((PWildcard, _) as f)::n::r -> (f, (ELambda ([simple_patterns (n::r) e], false), snd f))
    | [(PWildcard, _) as f] -> (f, e)
    | [] -> ((PUnit, (0, 0)), e)
    | _ -> parse_error "Nested patterns must be simple variables only."; raise Parsing.Parse_error;;

let patterns l e = match l with
    | f::n::r -> (f, (ELambda ([simple_patterns (n::r) e], false), snd f))
    | [f] -> (f, e)
    | [] -> ((PUnit, (0, 0)), e);;
%}

%token <ColoTypes.position * float> TNumber
%token <ColoTypes.position * string> TId TString TMember
%token <ColoTypes.position * bool> TSemi TBool
%token <ColoTypes.position> TThis TWildcard
%token <ColoTypes.position> TDot TComma TColon TDoubleColon TPipe TSet TLeft TRight
%token <ColoTypes.position> TParenA TCurlyA TBracketA
%token <ColoTypes.position> TParenL TParenR TCurlyL TCurlyR TBracketL TBracketR
%token <ColoTypes.position> TNegate TAdd TSub TMult TDiv TPow TOr TAnd TConcat TStringConcat 
%token <ColoTypes.position> TEqual TNotEqual TGreater TLess TGreaterEqual TLessEqual 
%token TEof

%left TOr
%left TAnd
%left TStringConcat TConcat
%left TAdd TSub
%left TMult TDiv
%nonassoc TNegate
%nonassoc TPow

%type <ColoTypes.program> program
%type <ColoTypes.expression pos> expression assignment apply atom
%type <string * string list> port
%type <(string * string list) pos list> exports imports
%type <string list> lower_list

%start program

%%

program
: optional_semis exports imports expression TEof
{ ($2, $3, $4) }
;

exports
: TLess TSub port exports
{ ($3, $1)::$4 }
|
{ [] }
;

imports
: TRight port imports
{ ($2, $1)::$3 }
|
{ [] }
;

port
: TString TBracketL lower_list TBracketR optional_semis
{ (snd $1, $3) }
| TString TBracketL TBracketR optional_semis
{ (snd $1, []) }
;

lower_list
: TId TComma lower_list
{ (snd $1)::$3 }
| TId
{ [snd $1] }
;

optional_semis
: semis
{ () }
| 
{ () }
;

semis
: TSemi semis
{ pick_semi $1 $2 }
| TSemi
{ $1 }
;

expression
: assignment semis expression
{ (ESequence ($1, $3), fst $2) }
| assignment semis
{ if snd $2 then $1 else (ESequence ($1, (EUnit, fst $2)), fst $2) } 
| assignment
{ $1 }
;

assignment
: TColon pattern apply
{ (ELet ($2, $3), $1) }
| TSet non_id_pattern apply
{ (ESet ($2, $3), $1) }
| TSet TId TNegate
{ (ESet ((PId (snd $2), fst $2), (EApply ((EId (snd $2), $1), (EString "Negate", $3)), $1)), $1) }
| TSet TId TAdd
{ (ESet ((PId (snd $2), fst $2), operator "Add" (EId (snd $2), $3) (ENumber 1.0, $3) $3), $1) }
| TSet TId TSub
{ (ESet ((PId (snd $2), fst $2), operator "Subtract" (EId (snd $2), $3) (ENumber 1.0, $3) $3), $1) }
| TSet TId TStringConcat apply
{ let op = operator "Concatenate" (EApply ((EId (snd $2), $3), (EString "ToString", $3)), $3) 
                                  (EApply ($4, (EString "ToString", $3)), $3) $3 in
                                  (ESet ((PId (snd $2), fst $2), op), $1) }
| TSet TId normal_operator apply
{ (ESet ((PId (snd $2), fst $2), operator (snd $3) (EId (snd $2), $1) $4 (fst $3)), $1) }
| TSet TId apply
{ (ESet ((PId (snd $2), fst $2), $3), $1) }
| apply
{ $1 }
;

normal_operator
: TAdd
{ ($1, "Add") }
| TSub
{ ($1, "Subtract") }
| TMult
{ ($1, "Multiply") }
| TDiv
{ ($1, "Divide") }
| TPow
{ ($1, "Power") }
| TConcat
{ ($1, "Concatenate") }
;

apply
: apply logical
{ (EApply ($1, $2), snd $2) }
| logical
{ $1 }
;

logical
: logical TOr logical
{ operator "OrElse" $1 (ELambda ([((PId "_", $2), $3)], false), $2) $2 }
| logical TAnd logical
{ operator "AndThen" $1 (ELambda ([((PId "_", $2), $3)], false), $2) $2 }
| arithmetics TEqual arithmetics
{ operator "Equal" $1 $3 $2 }
| arithmetics TNotEqual arithmetics
{ (EApply (operator "Equal" $1 $3 $2, (EString "Not", $2)), $2) }
| arithmetics TGreater arithmetics
{ operator "Greater" $1 $3 $2 }
| arithmetics TLess arithmetics
{ operator "Less" $1 $3 $2 }
| arithmetics TGreaterEqual arithmetics
{ operator "Or" (operator "Equal" $1 $3 $2) (operator "Greater" $1 $3 $2) $2 }
| arithmetics TLessEqual arithmetics
{ operator "Or" (operator "Equal" $1 $3 $2) (operator "Less" $1 $3 $2) $2 }
| arithmetics
{ $1 }
;

arithmetics
: TNegate arithmetics
{ (EApply ($2, (EString "Negate", $1)), $1) }
| arithmetics TAdd arithmetics
{ operator "Add" $1 $3 $2 }
| arithmetics TSub arithmetics
{ operator "Subtract" $1 $3 $2 }
| arithmetics TMult arithmetics
{ operator "Multiply" $1 $3 $2 }
| arithmetics TDiv arithmetics
{ operator "Divide" $1 $3 $2 }
| arithmetics TPow arithmetics
{ operator "Power" $1 $3 $2 }
| arithmetics TConcat arithmetics
{ operator "Concatenate" $1 $3 $2 }
| arithmetics TStringConcat arithmetics
{ operator "Concatenate" (EApply ($1, (EString "ToString", $2)), $2) 
                         (EApply ($3, (EString "ToString", $2)), $2) $2 }
| multi_apply
{ $1 }
;

multi_apply
: multi_apply TParenA expression_list TParenR
{ List.fold_left (fun a sum -> (EApply (a, sum), $2)) $1 $3 }
| multi_apply TParenA TParenR
{ (EApply ($1, (EUnit, $2)), $2) }
| multi_apply TCurlyA optional_semis lambda TCurlyR
{ (EApply ($1, ($4, $2)), $2) }
| multi_apply TBracketA expression_cons TBracketR
{ (EApply ($1, ($3, $2)), $2) }
| multi_apply TDot atom
{ (EApply ($1, $3), $2) }
| atom
{ $1 }
;

atom
: TParenL TParenR
{ (EUnit, $1) }
| TParenL expression TParenR
{ $2 }
| TBracketL expression_cons TBracketR
{ ($2, $1) }
| TCurlyL optional_semis lambda TCurlyR
{ ($3, $1) }
| TCurlyL optional_semis lambda TCurlyR
{ ($3, $1) }
| TNumber
{ (ENumber (snd $1), fst $1) }
| TString
{ (EString (snd $1), fst $1) }
| TMember
{ (EApply ((EId "__", fst $1), (EString (snd $1), fst $1)), fst $1) }
| TBool
{ (EBool (snd $1), fst $1) }
| TId
{ (EId (snd $1), fst $1) }
| TWildcard
{ (EId "_", $1) }
| TThis
{ (EId "__", $1) }
;

expression_cons
: expression_list TDoubleColon expression
{ EList ($1, Some $3) }
| expression_list
{ EList ($1, None) }
| 
{ EList ([], None) }
;

expression_list
: expression TComma expression_list
{ $1::$3 }
| expression TComma 
{ [$1] }
| expression
{ [$1] }
;

lambda
: set_this expression
{ ELambda ([((PId "_", snd $2), $2)], $1) }
| set_this matches
{ ELambda ($2, $1) }
;

set_this
: TColon TThis optional_semis
{ true }
| 
{ false }
;

matches
: TPipe patterns TPipe optional_semis expression matches
{ (patterns $2 $5)::$6 }
| TPipe patterns TPipe optional_semis matches
{ (patterns $2 (EUnit, $3))::$5 }
|
{ [] }
;

patterns
: pattern patterns
{ $1::$2 }
| pattern
{ [$1] }
;

pattern
: id_pattern
{ $1 }
| non_id_pattern
{ $1 }
;

id_pattern
: TId
{ (PId (snd $1), fst $1) }
;

non_id_pattern
: TParenL TParenR
{ (PUnit, $1) }
| TBracketL pattern_cons TBracketR
{ ($2, $1) }
| TString
{ (PString (snd $1), fst $1) }
| TNumber
{ (PNumber (snd $1), fst $1) }
| TBool
{ (PBool (snd $1), fst $1) }
| TWildcard
{ (PWildcard, $1) }
;

pattern_cons
: pattern_list TDoubleColon pattern
{ PList ($1, Some $3) }
| pattern_list
{ PList ($1, None) }
| 
{ PList ([], None) }
;

pattern_list
: pattern TComma pattern_list
{ $1::$3 }
| pattern
{ [$1] }
;

%%

