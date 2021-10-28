---
title: Y-combinator in Matlab
date: 2012-09-09
---

Y-combinator in Matlab
======================

For over 3 years my main programming language was Matlab. Matlab was designed
for scientific computations - it has a lot of build in functions for numerical
computation as well as some syntactic sugar which allows to manipulate arrays
easily. Matlab is imperative, supports object oriented programming (though the
implementation is very bad) and uses dynamic typing, so all type checking is
done at runtime. One of Matlab's features is the ability to store function
handles in variables. Does this ring a bell?

Yes! Functions as first-class citizens. This should allow to do some functional
programming, right? I decided to give it a try and write Y-combinator in Matlab.

A few words about Y-combinator
==============================

Let me first write a few words about Y-combinator in case you're not familiar
with it. Look at this recursive definition of Fibonacci function:

```matlab
function val = fib( n )
  if ( n == 0 || n == 1 )
    val = 1;
  else
    val = fib( n - 1 ) + fib( n - 2 );
  end
end
```

This recursive function - and probably all other recursive functions that you've
seen - works because we are able to give name to a function, which allows the
definition to refer to itself. What would happen however if we were unable to
give name to a function? This might seem a bit abstract, but think about
anonymous lambdas. As the name suggests they are anonymous. They have no name
and therefore cannot refer to themselves. But there is a way to make anonymous
recursive functions by using the Y-combinator. I will not go into details of how
and why the Y-combinator works the way it does, but I strongly encourage readers
to explore this subject. The best way to learn about Y-combinator is to walk
through its derivation. This is a truly mind-bending exercise. I needed about 5
days to understand how Y-combinator works but when I finally did it was one of
these "ooooohh" moments.

You will find a derivation of Y-combinator in the 9th chapter of "The Little
Schemer". The book might be a bit hard to find and I consider this derivation to
be a bit criptic (though the book itself is great). Luckily Peteris Krumins
[extended derivation from "The Little
Schemer"](http://www.catonmat.net/posts/derivation-of-ycombinator/). I will base
my post on his results. So, the final version of the Y-combinator written in
Scheme is:

```scheme
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
```

and the example of usage (also in Scheme) is:

```scheme
((Y (lambda (length)
     (lambda (list)
       (cond
         ((null? list) 0)
         (else
          (add1 (length (cdr list))))))))
 '(a b c d e f g h i j))
```

The above listing shows an anonymous recursive function that calculates the
length of a list.

I will present my results to match those above as closely as possible.

Anonymous functions in Matlab
=============================

In order to work with Y-combinator we will have to define anonymous functions.
In the Scheme code above an anonymous function for calculating the length of a
list is passed as a parameter to Y-combinator. It turns out however that
anonymous functions in Matlab have some limitations. Let's take a look at the
[documentation](http://www.mathworks.com/help/techdoc/matlab_prog/f4-70115.html):

> The syntax for creating an anonymous function from an expression is
>
> `fhandle = @(arglist) expr`
>
> Starting from the right of this syntax statement, the term expr represents the
> body of the function: the code that performs the main task your function is to
> accomplish. **This consists of any single, valid MATLAB expression**.

The fact that Matlab allows anonymous functions to consist of only one
expressions has serious consequences. Imperative languages divide all their
language constructs into two categories: expressions, which return some value
and statements, which don't return any value[^1]. Sadly, the control-flow
constructs like `if` and `for` are statements, which means that we can't include
them in an anonymous function. This is problem, because `length` function shown
above needs a conditional instruction to check if the list passed to it is empty
or not.

Therefore, our first step is to create a new version of `if` construct which
will be an expression and not a statement. There are a few different ways to
achieve this. I decided to use cell arrays. Cell arrays are Matlab's data
structure similar to arrays, except for the fact that every cell can hold
different type of value. My custom `if` instruction will take two parameters: a
predicate that evaluates either to 1 (Matlab's true) or 0 (Matlab's false) and a
cell array with two function handles. The code looks like this:

```
if_ = @( pred_, cond_ ) cond_{ 2 - pred_ }();
```

The `pred_` variable is the predicate - either 0 or 1 - and `cond_` is a cell
array. If the predicate is 0 then second function in `cond_` cell array will be
used. If the `pred_` is 1 then `if_` will use first function in `cond_` cell
array ((Matlab uses 1-based indexing )). Notice that there's `()` after cell
array index. This is a function application. This means that after selecting one
of two function handles, the function pointed by it is executed immediately and
`if_` returns value returned by that function. Here's an example[^2]:

```
>> if_ ( 1 == 2, { @() disp( 'The predicate is true'), @() disp( 'The predicate is false' ) } )
The predicate is false
```

Had we removed the parens, `if_` would return a function handle allowing it to
be evaluated later:

```
>> if_ ( 1 == 2, { @() disp( 'The predicate is true'), @() disp( 'The predicate is false' ) } )
ans =
  @()disp('The predicate is false')
```

This is somewhat similar to lazy evaluation.

These are not the only limitations of Matlab. Consider the example below.

```matlab
f = @(x) x == 1
g = @(x) x
```

We created two functions: `f` tests its parameter for equality with 1, while `g`
is the identity function - it returns its parameter. If we apply `g` to `f`, we
should get `f`, that is a handle to a function that tests its parameter for
equality with one:

```
>> g(f)
ans =
  @(x)x==1
```

That is what we expected. We got a function handle to anonymous function that
accepts one parameter. It is reasonable to expect that we can now pass parameter
to that handle. Unfortunately, Matlab will not allow us to do so:

```
>> (g(f))(1)
(g(f))(1)
|
Error: Unbalanced or unexpected parenthesis or bracket.

>> g(f)(1)
Error: ()-indexing must appear last in an index expression.
```

So we cannot chain anonymous function calls that easily. We have to use Matlab's
`feval` function, that evaluates a function handle with given parameters:

```
>> feval(g(f), 1)
ans =
  1
```

The Y-Combinator
================

With this knowledge we are now able to rewrite Scheme code presented earlier.
Here's how Y-combinator looks like:

```
Y   = @( le ) feval(            ...
          @( f ) f( f ),        ...
          @( h )                ...
            le( @( x ) feval( h( h ), x ) ) );
```

This is almost the same as Scheme code presented earlier. We just replaced
lambda with @ (both denote anonymous function declaration) and changed function
application syntax to match Matlab. Before we rewrite the length function in
Matlab let us define some helper functions:

```
if_  = @( pred_, cond_ ) cond_{ 2 - pred_ }();
add1 = @( x ) x + 1;
cdr  = @( list ) list( 2 : end );
```

We've already seen `if_`, but it's here just to remind you that we need that
declaration. The `add1` function increments its parameter by one, while `cdr`
emulates Scheme's `cdr` function that returns tail of a list. Finally we can see
Y-combinator in action:

```
feval( Y ( @( length_ )                           ...
  @( list )                                       ...
    if_(                                          ...
      isempty( list ), {                          ...
        @() 0,                                    ...
        @() add1( length_( cdr( list ) ) ) } ) ), ...
  \['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' \] )
```

This code can be placed into a script (say `Y_combinator.m`) and evaluated:

```
>> Y_combinator
ans =
  10
```

Conclusions and further reading
===============================

As you can see, Matlab's support for function handles allows to write
Y-combinator. The result looks fairly simple, but I must admit that it wasn't
straightforward and I had some hard time struggling with syntax. Matlab's
functional programing capabilities are very limited, but there are a few more
things that can be done. A more in-depth treatment can be found on A Page of
Insanity blog [here](http://apageofinsanity.wordpress.com/2011/12/01/functional-programming-in-matlab/).
The solution presented there is very similar to mine. Check out also [this
gist](https://gist.github.com/2493945) to see a different approach. See also
Mike Vanier's blog for [more details on Y-combinator](http://mvanier.livejournal.com/2897.html).
I find Mike's derivation a bit harder to follow, but Mike discusses both strict
ans lazy versions of Y-combinator (I used only strict version).

[^1]: Please tell me if there exists an imperative language that does not have
this distinction.

[^2]: Text preceded by `>>` is typed into Matlab prompt.

