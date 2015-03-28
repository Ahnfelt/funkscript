# Note #

I am currently working on a new version of funk whose syntax is somewhat simplified (no 1? and 0?, no strange conditional syntax, no more optional . or special parenthesis rules for calls). There are more small changes, and the new version will also have types and a syntax for (optionally) specifying these.

# Comments #

Comments start with a hash `#` and lasts to the end of the line. They are ignored by the compiler.

# Union #

The union value corresponds roughly to _null_ or _nil_ in other languages. It is useful if you have no other meaningful value to return (when your function's only purpose is to perform a side effect).

```
()          # The union value
```

# Booleans #

Truth value literals matche the regular expression `[01][?]`.

```
1?          # True
0?          # False
```

# Numbers #

A number literal matches `[0-9]+([.][0-9]+)([eE][-+]?[0-9]+)?` and has the obvious corresponding floating point value.
```
20          # The number twenty
3.141592    # A bad approximation of pi
1.2e9       # 1.2 billion
```

# Strings #

String literals match `["]([^"]|\\["])*["])` or `[']([^']|[']['])*['])`. Double quote strings can have escape sequences in them (a backslash followed by a code), while single quote strings interpret backslash as an ordinary character. Two single quotes after each other in a single quote string is interpreted as a literal single quote within the string. All strings are in Unicode. Additionally, identifiers that start with a uppercase letter are strings too.

```
"Hello, Funk"  # A text string: Hello, Funk
"\"Funky\""    # Another string: "Funky"
'It''s good'   # A third string: It's good
MyFunString    # The last string: MyFunString
```

# RegExes #

Regular expressions (actually a superset) start and end with a ` (back quote). If options are required they can be put in back quotes immediately after (no spaces in between). The syntax of regular expressions inside have not been decided yet, but I'll look at Perl when designing them. Suggestions are welcomed.

```
`[a-zA-Z]+`  # Matches any sequence of English letters.
:f {
    |`f(|s|o*)bar`|   # Matches foooobar (any number of os) 
                      # and binds the matched os to the s
                      # variable.
}
```

# Lists #

List literals start with `[` and ends with `]`. Within them are expressions separated by commas, and an optional comma at the end.

```
[]             # The empty list
[1, 2, 3, 4]   # A list of four numbers: 1 2 3 4
[A, B, ]       # Demonstrating the optional trailing comma.
```

# Dictionaries #

Associative container literals start with `[` and `]` like lists, but the keys are enclosed in vertical bars `| |`.

```
[|]              # The empty dictionary (notice the bar that distinguishes
                 # it from an empty list).
[|X| 15 |Y| 26]  # The dictionary where "X" maps to 15 and "Y" maps to 26.
```

# Conditionals #

Conditionals are a way to perform decisions. It is a list of pairs of conditions and expressions, optionally followed by an expression to evaluate in case none of the conditions are true. The conditions are tried in order, until one of them evaluates to true, and then the corresponding expression is evaluated and returned.

```
(|x < 0| -x |x > 0| x | 0)
```

The above example reads "if `x < 0` then return `-x`, or if `x > 0` then return `x`, otherwise return `0`. If there had been no default expression (the last one), it would return `()` when `x` is zero.

# Patterns #

A pattern matches a set of values. All the literals discussed above can be matched using the same syntax as is used to construct them. The pattern that matches anything is written `_`. If the value is needed within the expression, an variable pattern can be used, which consists of a variable identifier. It matches any value and binds that value to the corresponding value. There is a special pattern for matching variable-length lists which is a list pattern followed by the concatenation operator `&` and a pattern that matches the rest of the list.

```
12               # A pattern that matches the number 12
[A, B, C]        # Matches the list of the three stings "A" "B" "C"
[a, _]           # Matches any list with two elements and binds the first element to a
[_] & r          # Matches any list with at least one element, and binds a list that
                 # contains all elements but the first to `r`.
```

# Functions #

Functions are enclosed in `{ }` (curly braces). A function contains any number of matching pairs of patterns and expressions, that are tried in the order they appear in the code. If an expression is empty, it's assumed that it is unit `()`. If there are no patterns, but only a single expression, the function's sole element is bound to the special variable `_` (underscore). If the curly brace is followed by `:@`, the function itself is bound to the special variable `@`. Multiple arguments are implemented with [currying](http://en.wikipedia.org/wiki/Currying), but have a shorter notation that consists of putting multiple patterns next to each other within the vertical bars `| |`. Patterns matching the second, third, or any following argument must be simple: either a variable pattern or the wildcard pattern `_`.

```
{}               # The function that discards it's sole argument and returns ()
                 # without any side effects.
{|p1 p2| r}      # The function that takes two arguments which is required to
                 # match the p1 and p2 patterns respectively, and returns r.
{:@ @}           # The function that returns itself when called.
```

Using multiple pattern/expression pairs you can define an object constructor:

```
:point {|x y| {
    |X| x
    |Y| y
    |Add p| point(x + p.X, y + p.Y)
    |ToString| "("..x..", "..y..")"
}}
```

# Variables #

Variables are mutable (non-constant) and are named by an identifier matching `[a-z][a-zA-Z0-9]*`. They have lexical scoping and you can modify a variable from an outer scope within an inner scope.

```
x                # A variable called x
myLongVariable   # A variable called myLongVariable
```

## Declaration ##

You declare a variable by using the _let_ expression that starts with a `:` (colon), then a pattern and then an expression (followed by a `;` (semicolon) and the expression in which it's visible). The pattern can be a variable pattern, or have such patterns nested in it - these are the variables that will be declared (and initialized to what they match).

```
:x 23            # Let x be the number 23
:[a, b] [5, 7]   # Let a be 5 and b be 7
```

To support mutual recursion, any sequence of variable declarations in the form `:v f` where v is an arbitrary variable name and f is any function literal, can use each other and themselves in the declaration of the function.

```
:even {
    |0| 1?
    |n| odd(n - 1) 
}
:odd {
    |0| 0?
    |n| even(n - 1) 
}
```

## Updates ##

Since variables are mutable, you can replace their value with something else. This is done the same way as declaring variables, except `!` (exclamation mark) instead of `:` (colon). Variable patterns will update the corresponding variables with the variables it matches instead of introducing new variables.

```
:x 10      # Let x be the number 10
!x 20      # Set x to the number 20
!x x + 1   # Increase x by one
```

Increasing and decreasing variables can be tedious, since you have to repeat the variable name. Special syntax is available for this, when it comes to the operators `+`, `-`, `*`, `/`, `&` and `..`.

```
!x +       # Increase x by one
!x + 3     # Increase x by 3
!x - n     # Decrease x by n
```

# Sequences #

A sequence consists of two expressions separated by a `;` (semicolon). The first expression is evaluated, and the result is discarded. Then the second expression is evaluated, and the result of it is the result of the sequence. Optionally, you can put a `;` after an expression without having an expression following it - in that case the semicolon will be ignored.

```
print "First do this"; 
print "Then do this";
```

Since semicolons most of the time occurs at the end of the line, the compiler tries to guess where you wanted them. It simply inserts a semicolon at the end of each line, unless the innermost enclosing parenthesis are `( )` or `[ ]`. If you prefer to write all semicolons yourself, you can use the `-- ManualSemicolons` directive on a line by itself in the code to turn it off.

```
:l [This, List,
    Spans, Multiple,
    Lines]
print "This expression is evaluated next"
:f {
    :some "function"
    "with a sequence of expressions"
}
l     # And l is the result of the sequence.
```

Note that with automatic semicolons, any expression that spans multiple lines should only break a line within some kind of brackets, such as `( )`, `[ ]` or `{ }`.

A Funk source file is simply a sequence of expressions.

# Operators #

Operators are syntactic sugar for method calls. The following table lists the operators and the method calls they correspond to. Operators with higher precedence are towards the bottom of the table. All of them associate to the left, except precedences 3, 7 and 8, which or are non-associative.

| | `(a)` | | `a` |
|:|:------|:|:----|
| 1 | `a b` |  | `a b` (call `a` with the argument `b`) |
| 2 | `a && b` |  | `a AndThen {b}` |
| 2 | _see bottom_ |  | `a OrElse {b}` |
| 3 | `a = b` | `a == b` | `a Equal b` |
| 3 | `a <> b` | `a != b` | `a Equal b Not` |
| 3 | `a < b` |  | `a Less b` |
| 3 | `a <= b` |  | `a LessEqual b` |
| 3 | `a > b` |  | `a Greater b` |
| 3 | `a >= b` |  | `a GreaterEqual b` |
| 4 | `a & b` |  | `a Concatenate b` |
| 4 | `a..b` |  | `(a ToString) Concatenate (b ToString)` |
| 5 | `a + b` |  | `a Add b` |
| 5 | `a - b` |  | `a Subtract b` |
| 6 | `a * b` |  | `a Multiply b` |
| 6 | `a / b` |  | `a Divide b` |
| 7 | `~a` |  | `a Negate` |
| 8 | `a ^ b` |  | `a Power b` |
| 9 | `a . b` | a(b) | `a b` (call `a` with the argument `b`) |

The `Or` operator is the double vertical bar `||`, but cannot appear within a table in this Wiki. Note that there is no space between the expression and the parenthesis in `a(b)` - this is important, since if you leave in a space, the precedence will be the lowest. If your function takes just one parameter, and it's a list or function literal, you can leave out the parenthesis. For example `a[1, 2, 3]` or `a{|n| n * 2}`. Notice that since operators are defined in terms of methods, you can make them work on your own objects just by defining those methods.

# Modules #

Modules are named collections of values. They are exported using the `export` function and imported using the `import` function (or the equivalent syntactic sugar).

## Importing ##

Variables that have been exported from other modules can be used by qualifying the variable with the module name. You can also import it into your own namespace, which makes it possible to use the imported variables without qualification.

```
print Math:sin(Math:pi)            # Use with qualification (and syntactic sugar)
:[sin, pi] import Math [Sin, Pi]   # Import sin and pi from the Math module
Math:[sin, pi]                     # Same thing, but using syntactic sugar
print sin(pi)                      # Use without qualification
```

## Exporting ##

Exporting values enables other modules to import them. You can reexport values, but the importing modules will need to import the values after the reexport to discover the new value.

```
:greet {|person|
    print "Hello, "..person
}
export MyModule [|Greet| greet]    # Export the greet value as MyModule:Greet
```

You can freeze a module after exporting to it, to prevent future scripts from changing the module. Once a module is frozen, it can never be modified or unfrozen.

```
freeze MyModule                    # Disallow any future changes to this module
```