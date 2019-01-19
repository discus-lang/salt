# Salt Intermediate Language

Salt is what you get when you leave C out in the sun for too long.

Salt is the compilation target that functional programmers always wanted.

Salt is a [System-F](https://en.wikipedia.org/wiki/System_F) based intermediate language intended as a compilation target for higher level languages. Hand written code can also be used to implement runtime systems for the same compilers. The [Disco Discus Compiler](https://github.com/discus-lang/ddc) uses an earlier version of Salt (v1), and its runtime system is written in it. This current repo contains a newer version of the Salt language (v2) that is being split out into its own project. Salt v1 compiles into [LLVM](https://llvm.org/) code, and is significantly easier for humans to deal with than such an abstract assembly language. Similar compilation of Salt v2 into LLVM is work in progress.

## Example

We have some small "Hello World" type examples and test cases, but no larger programs yet. See the [demo](test/01-demo) and [syntax](test/10-syntax) directories in this repo. The syntax looks like:

```
term reverse @[a: #Data] [xx: #List a]: #List a
 = case #list'case @a xx of
        { nil  [] → [list a|]
        ; cons [x: a, xs: #List a]
           → append @a [reverse @a xs, #list'one @a x] }

test eval reverse
 = reverse @#Nat [list #Nat| 10, 12, 13, 14]
```

The `@[a: #Data]` indicates a type parameter, and `@a` in the body is a type argument. Names starting with `#` are primitive constructors and operators. Test cases can be defined inline with the code and evaluated within the IDE via the Language Server. The syntax permits standard unicode symbols for punctuation, but there are plain ASCII alternatives.


## Features

The new version of the language includes:

 * A [System-F](https://en.wikipedia.org/wiki/System_F) substrate with distinct namespaces for types and terms.
 * Mixed names and [de Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index) representation for variables, using bump indexes similar to Isabelle.
 * Functions that work on vectors of argument and return values, as described in [Types are Calling Conventions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/tacc-hs09.pdf) by Max Bolingbroke and Simon Peyton Jones, Haskell Symposium 2009. This approach expresses arity information directly in the types of functions, instead of it needing to be maintained as separate meta-data.
 * A coeffect system using `box` and `run` casts, as described in [Capabilities and Coeffects](http://blog.discus-lang.org/2013/12/capabilities-and-coeffects.html). This system is also part of the Discus language, and is related to work on "Graded Monads", but the monadic structure is baked into the language rather than being encoded.
 * Structural record and variant types.

## Status

 * The current implementation includes a parser, type checker, big step interpreter, and server for the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/).
 * The language server produces parser and type checker diagnostics, and allows test cases to be run interactively using the interpeter.
 * The LLVM code generator itself has not yet been ported across from DDC. This will happen next.


## Support

The `salt` executable includes a Language Server that speaks [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). You should be able to get this working with your favourite editor or IDE. A matching [Visual Studio Code](https://code.visualstudio.com/) extension and color theme is available here:

* https://github.com/discus-lang/salt-vscode
* https://github.com/discus-lang/salt-theme

## Documentation

The [doc](doc) directory in this repo contains further documentation.

 * [Language Grammar](doc/reference/01-grammar.md)
 * [Kinding Rules](doc/reference/02-kinding.md)
 * [Typing Rules](doc/reference/03-typing.md)

## More Information

Salt is part of the Discus project, so ask questions on the discus mailing list.

* The GitHub site:        http://github.com/discus-lang/salt
* Development Blog:       http://blog.discus-lang.org
* Mailing List:           https://groups.google.com/forum/#!forum/discus-lang
