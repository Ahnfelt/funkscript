(*
:newPoint3d (|x y z :: newPoint2d x y :: newObject3d x y z|
    |Z| z
)
Constructor notation:
First comes the parameters, then the calls to parent constructors, 
then the initialization code, and then the methods. The methods must
be on the form |String param1 p2 p3 etc| action.
A couple of extra methods are defined: ReflectParents, ReflectMethods, 
ReflectConstructor, that returns information about which methods and 
parents the object has, plus the constructor.
Local functions of the same name (only lowercase first member) might
be nice, so you could define your own reflectors on top of these.
The constructor can have number-of-args information, since that's 
fixed, but it's harder for methods, since they can be curried. 
However, this could simply be ignored and just return the number of 
arguments that are not curried.
For each parent 'o' that doesn't take parameters, a local variable 
parent'o' is provided. For each parent 'p' that does take parameters,
the "new" is cut off and replaced with parent to form the name of a 
local variable that holds the result of calling the constructor.
*)
(*
Scripts could instead be
imports/exports
function/object definitions
{initializer}
to facilitate intermodular circular dependencies.
*)

let parse () =
    let program = Std.input_file "Test.colo" in
    let buffer = Lexing.from_string(program) in
    let tree = ColoParser.program ColoLexer.lexer buffer in
    let ocaml = (ColoOcaml.format tree) in
        Std.output_file "Test.ml" ocaml;;

let _ = parse ();;

