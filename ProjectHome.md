**Funk** is a scripting language that is very simple at it's core: It has functions (as values), pattern matching and real variables. That's it - the rest is just nice notation for defining different kinds of functions - every value is a function. Object oriented programming is implemented via pattern matching, and the notation is shorter than most other dynamic (and static) languages.

# Implementation status #
The current implementation is written in the [OCaml](http://caml.inria.fr/download.en.html) language and translates the language into OCaml. It also requires [ExtLib](http://sourceforge.net/projects/ocaml-lib/). Please use a Subversion client to check out the source if you want to play around with it. Modules and standard libraries are not implemented at this point.

**I am no longer working on Funk.**

# A short introduction #
_The details aren't entirely accurate, see the [Reference](Reference.md) for specifics._
```
:fib {
    |0| 0
    |1| 1
    |n| fib(n - 1) + fib(n - 2)
}
```
This is a naive implementation of the Fibonacci function (returns element `n` in the sequence 1, 1, 2, 3, 5, 8, 13, 21, ...). It uses a simple form of _pattern matching_: if the argument matches the pattern `0`, that is, if it is zero, the function returns zero. If the argument matches `1`, it returns one, and otherwise it calls itself to find the answer.

```
:point {|x y| { 
    |X| x
    |Y| y
    |Add p| point(x + p.X, y + p.Y)
    |ToString| "("..x..", "..y..")"
}}
```
The above is a constructor function that constructs a point (x, y). The first thing to notice is the `|x y|` pattern. It simply says that the parameter takes two arguments, `x` and `y`. The interesting part is what it returns - a new object. This new object has methods `X` and `Y` that returns the `x` and `y` that the constructor was called with (notice that those arguments are never forgotten in the entirety of the constructed objects lifetime).

The `Add` method takes an argument `p`, which is a point, and returns a new point with coordinates that are the sums of the individual coordinates of this point and the `p` point. The `ToString` method simply returns a nice string representation of the point.

```
:p1 point(5, 7)    # let p1 be the point (5, 7)
:p2 point(2, 3)    # let p2 be the point (2, 3)
print p1.X         # prints "5"
print p1.Add(p2)   # prints "(7, 10)"
```
This example shows the basics of how to define variables and how to call methods (oh yeah, and line comments begin with a hash (`#`) and are ignored). Apart from the colon syntax for defining variables, it should be straightforward if you already know OOP. If not, don't worry, I'll explain it without reference to OOP later on.

There are no keywords in Funk. No `if`, no `while` and no `print`. The `if` statement has what must seem like a rather peculiar replacement in this language:
```
(x > 42).Then({print "x is great!"})
```
As you may have guessed, this reads "if x is greater than 42, then print >>x is great!<<". But `Then` is no keyword - it is a method in a boolean type, and it only executes the print statement if the boolean is true.

You may have noticed that `print "x is great!"` is enclosed in curly braces. As in the first example, the curly braces signifies that it is a function. Since there is no arguments enclosed in `| |`, it simply discards the argument it's called with.

The above syntax for calling methods is a bit clumsy in this case, which is why there is an alternative notation for the same thing:
```
x > 42 Then {print "x is great!"}
```
Note that the method call in this notation (called juxtaposition) has lower precedence than anything else, meaning that `x > 42` will be evaluated before calling the `Then` method on the result.

Let's see if we can make the language look like C, Java, you name it, by defining our very own while loop:
```
:while {|condition body|
    condition Then {body(); while(condition, body)}
}
```
So, `while` is the function that takes a `condition` and a `body`, and if the condition is true, it executes the `body`, and then repeats itself. The repetition is obtained by using recursion - that is, by calling the very function we're defining (thus performing the same thing again).

The non-cluttered version of the `Then` call in the previous example can also be used for regular function calls. And if a function takes more than one argument, they're separated by spaces instead of commas in this notation:
```
:numbers [3, 15, 5, 16, 2, 12, 1, 7, 11, 18]  # Let numbers be a list of some numbers
:i 0
while {i < numbers.Length} {
    print numbers.Get(i)      # Print the i'th element of the list
    !i +                      # Shorthand for increasing i by one
}
```
This snippet prints all the numbers in the list, one by one. Notice how the `while` loop looks entirely like a while loop from C or Java, except that it uses curly braces around the condition instead of parenthesis. In other words, by writing ordinary functions, you can obtain an elegant notation that is hard coded into most other languages. Note that `print` is also just a function. There is a number of predefined functions (including `while` and `print`), that are available to you automatically - you don't have to define them as I just did.

Iterating over a list with a while loop is quite tedious, and it's a very common operation, so lists have a method called `Each` that will perform this iteration with much less code. To rewrite the above example using this method:
```
numbers Each {|element|
    print element
}
```
Not bad, eh? Or how about `numbers Each {print _}`? If a function contains no argument list (in `| |`), it names it's argument `_` (underscore). Of course, `numbers Each print` is even shorter!

The list contains other such methods to make your life easier. Let's say we wanted to have a sorted list in descending order of twice of those of the numbers that are greater than ten. Quite a mouthful in English, and some programming languages.
```
numbers Filter {_ > 10} Map {_ * 2} Sort {|a b| a > b}
```
Let's dissect it: The `Filter` method call returns a list of all the elements in the `numbers` list that are greater than ten. This new lists `Map` method is then called, and returns a list where all the elements have been multiplied by two. Finally, the returned lists `Sort` method makes sure that any element `a` in the final list comes before `b` if `a` is greater than `b`. Funky, isn't it?

If you've ever been using a functional language, it is very likely that you've seen these higher order functions before. In fact, that is why the language is called Funk - because it sounds like func<sup>tional</sup>. And because it makes you [feel good](http://www.youtube.com/watch?v=5RZcENc5ON8&feature=related). If it doesn't ring a bell, [try here](http://www.joelonsoftware.com/items/2006/08/01.html).

Before you e-mail me and suggest that I call it _Smalltalk_ or _Ruby_, consider this: _there is no object_. There are no methods. It's all an illusion. Functions and pattern matching is everywhere. It is all around us. Even now, in this very tutorial. You can see it when you look at your constructors or when you call your methods. You can feel it when you use inheritance... when you encapsulate... when you pay your taxes. No wait, not the last one. It is the syntax that has been pulled over your eyes to blind you from the truth.

With apologies to [The Wachowski Brothers](http://en.wikipedia.org/wiki/Wachowski_brothers) for the last paragraph, I will dig a bit into pattern matching before explaining exactly how the language can appear to be object oriented without having any special support for it what so ever.

Let's say we wanted to write a simple calculator that could evaluate expressions like
```
eval(["Add", ["Add", ["Num", 10], ["Num", 5]], ["Num", 4]]) 
```
You may recall the list syntax from earlier examples. The expression we're trying to represent is `(10 + 5) + 4`.

Recall that the patterns are written within `| |`. Patterns that matches lists have a similar syntax to lists in expressions. In a pattern, a literal such as a string or a number matches itself - that is, the pattern matches if the value in question is equal to the value of the literal. A list pattern matches if each pattern in it matches the corresponding element in the value. Variable patterns (starts with a lowercase letter) matches anything, and remembers what it matched, while the `_` (underscore) pattern matches anything and discards it.
```
:eval {
    |["Add", x, y]| eval(x) + eval(y)
    |["Num", n]| n
}
```
The call in the previous example triggers a match of the first pattern in `eval`: it is a list, it has three arguments, `"Add"` matches `"Add"`, `x` matches the second element and remembers it, and `y` matches third, and remembers that it's `["Num", 4]`. Since the pattern matches, the corresponding expression is evaluated, namely `eval(x) + eval(y)`. `eval(y)` is equivalent to `eval(["Num", 5])`, since that is what `y` was bound to in the pattern. In this call the first pattern doesn't match, since it requires a list of three elements (and besides, `"Add"` is not equal to `"Num"`). The second pattern succeeds, since the value and the list both have two elements, the strings are equal, and a variable matches anything. The `n` expression evaluates to `4`. I'm sure that by now you can evaluate `eval(x)` from the previous expression yourself, and perform the rest of the calculation yourself.

This scripting language is implemented using pattern matching and recursion, much in the same way as in that example.

It gets awfully tiresome to write all those double quotes everywhere, but fortunately Funk has a special syntax for such strings - any identifier that starts with an uppercase letter is a string. That is, `Add` means exactly the same as `"Add"`, and `Num` the same as `"Num"`. We thus have
```
eval([Add, [Add, [Num, 10], [Num, 5]], [Num, 4]]) 
```

But wait, how does Funk figure out that `Each`, `Map` and `Apply` are methods, while `Num` and `Add` are strings? It doesn't! There are no methods. In fact, the `Each` example far above could have been written
```
list("Each", {print _})
```
because the definition of the `Each` "method" in the list is equivalent to:
```
  ...
    |Each action| (list.Length > 0) Then {action(@Head); @Tail.Each(action)}
  ...
```
That is, a pattern that matches the string `"Each"` and then takes another argument `action` that matches anything. If it doesn't make sense right now, you're free to just remember the syntax for pattern matching and "methods" as two different things. It'll take a lot of time before you need a deep understanding of it in practice.

In the above example, I used the _this_ construct `@` (also known as _self_ or _me_). It is syntactic sugar for referring to the enclosing function. In order to use it, you must start a function with `{:@` instead of just `{`. The `@` will then refer to the innermost enclosing function that begun with `{:@`. In effect, you can use it to refer to the object you're currently defining.

The `Head` method returns the first element of a list, while the `Tail` method returns the rest of the elements (that is, all elements but the one returned by `Head`). Note that "methods" that take no arguments should never be called with an "empty argument list" `()`. This would attempt to call the returned value, which is most likely _not_ what you want.

When you want to write more interesting programs, you're going to have to use functions from other modules. One way to do this is to qualify the function with the module name:
```
    print Math:sin(Math:pi)
```
This is nice if you only need the symbols once or twice, but if you use them often, it can get cumbersome. You can avoid repeating yourself by importing the methods into your own namespace first:
```
    Math:[sin, pi]    # Import sin and pi from the math module
    print sin(pi)     # Then they can be used without qualification
```

That concludes this tutorial. Check the [Reference](Reference.md) to discover the available "methods" for the different "objects". I'm tired of double quotes, so let's just agree to call them _methods_ and _objects_, even if it's an unusual implementation.