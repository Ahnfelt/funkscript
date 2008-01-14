{
open ColoParser;;
open ExtString;;
exception Eof;;
let inside s = String.sub s 1 (String.length s - 2);;
let double_quotes s = inside s;;
let single_quotes s = 
    let rec replace_all s = 
    let (b, s) = String.replace s "''" "'" in 
    if b then replace_all s else s in
    replace_all (inside s);;
let p l = let r = l.Lexing.lex_curr_p in (r.Lexing.pos_lnum, r.Lexing.pos_cnum - r.Lexing.pos_bol);;
let ap = ref (0, 0);;
let a l = ap := p l;;
let apply_position (l, c) = l = fst !ap && c = snd !ap + 1;;
let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
        };;
exception Brace;;  
let brace_stack = let s = Stack.create () in Stack.push '{' s; s;;
let brace b = match b with
    | '{' -> Stack.push '{' brace_stack
    | '}' -> ignore (Stack.pop brace_stack)
    | '(' -> Stack.push '(' brace_stack
    | ')' -> ignore (Stack.pop brace_stack)
    | '[' -> Stack.push '[' brace_stack
    | ']' -> ignore (Stack.pop brace_stack)
    | _ -> raise Brace;;
let allow_auto_semi = ref true;;
let auto_semi () = !allow_auto_semi && Stack.top brace_stack = '{';;
}

rule lexer = parse
    [' ' '\t' '\r'] {lexer lexbuf}
  | ('#' [^'\n']*)? '\n' {incr_linenum lexbuf; if auto_semi () then TSemi (p lexbuf, true) else lexer lexbuf }
  | "--" [' ' '\t']* "ManualSemicolons" {allow_auto_semi := false; lexer lexbuf}
  | "--" [' ' '\t']* "AutomaticSemicolons" {allow_auto_semi := true; lexer lexbuf}
  | "0?" {a lexbuf; TBool (p lexbuf, false)}
  | "1?" {a lexbuf; TBool (p lexbuf, true)}
  | ['0'-'9']+ ('.' ['0'-'9']+)? (['e' 'E'] ('+'|'-')? ['0'-'9']+)?
    {a lexbuf; TNumber (p lexbuf, float_of_string (Lexing.lexeme lexbuf))}
  | '"' [^'"']* '"' {a lexbuf; TString (p lexbuf, double_quotes (Lexing.lexeme lexbuf))}
  | '\'' ([^'\'']|"''")* '\'' {a lexbuf; TString (p lexbuf, single_quotes (Lexing.lexeme lexbuf))}
  | ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* {a lexbuf; TString (p lexbuf, Lexing.lexeme lexbuf)}
  | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']* {a lexbuf; TId (p lexbuf, Lexing.lexeme lexbuf)}
  | '@' ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* {a lexbuf; TMember (p lexbuf, String.lchop (Lexing.lexeme lexbuf))}
  | '@' {a lexbuf; TThis (p lexbuf)}
  | ":@" {a lexbuf; TSetThis (p lexbuf)}
  | '_' {a lexbuf; TWildcard (p lexbuf)}
  | ':' {TColon (p lexbuf)}
  | ';' {TSemi (p lexbuf, false)}
  | ',' {TComma (p lexbuf)}
  | '.' {TDot (p lexbuf)}
  | '|' {TPipe (p lexbuf)}
  | '!' {TSet (p lexbuf)}
  | '{' {brace '{'; let pos = (p lexbuf) in if apply_position pos then TCurlyA pos else TCurlyL pos}
  | '}' {brace '}'; a lexbuf; TCurlyR (p lexbuf)}
  | '[' {brace '['; let pos = (p lexbuf) in if apply_position pos then TBracketA pos else TBracketL pos}
  | ']' {brace ']'; a lexbuf; TBracketR (p lexbuf)}
  | '(' {brace '('; let pos = (p lexbuf) in if apply_position pos then TParenA pos else TParenL pos}
  | ')' {brace ')'; a lexbuf; TParenR (p lexbuf)}
  | '~' {TNegate (p lexbuf)}
  | '+' {TAdd (p lexbuf)}
  | '-' {TSub (p lexbuf)}
  | '*' {TMult (p lexbuf)}
  | '/' {TDiv (p lexbuf)}
  | "^" {TPow (p lexbuf)}
  | '&' {TConcat (p lexbuf)}
  | ".." {TStringConcat (p lexbuf)}
  | ("="|"==") {TEqual (p lexbuf)}
  | ("!="|"<>") {TNotEqual (p lexbuf)}
  | '>' {TGreater (p lexbuf)}
  | '<' {TLess (p lexbuf)}
  | ">=" {TGreaterEqual (p lexbuf)}
  | "<=" {TLessEqual (p lexbuf)}
  | "||" {TOr (p lexbuf)}
  | "&&" {TAnd (p lexbuf)}
  | eof {TEof}

