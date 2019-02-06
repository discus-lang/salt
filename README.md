# Salt Intermediate Language [![Build Status](https://travis-ci.com/discus-lang/salt.svg?branch=master)](https://travis-ci.com/discus-lang/salt)

Salt is what you get when you leave C out in the sun for too long.

Salt is the compilation target that functional programmers always wanted.

Salt is a [System-F](https://en.wikipedia.org/wiki/System_F) variant intended as an intermediate language between higher level languages and an abstract assembly language ([LLVM](https://llvm.org/)). Hand written code can also be used to implement runtime systems. The [Disco Discus Compiler](https://github.com/discus-lang/ddc) uses an earlier version of Salt (v1), and its runtime system is written in it. This current repo contains a newer version of Salt (v2) which is being split out into its own project. Salt v1 has a working LLVM backend, but the one for v2 in this repo is still a work in progress.

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

The `@[a: #Data]` indicates a type parameter, and `@a` in the body is a type argument. Names starting with `#` are primitive constructors and operators. Test cases can be defined inline with the code and evaluated within the IDE (via the Language Server). The Salt syntax supports standard unicode symbols for punctuation, but there are also plain ASCII alternatives.


## Features

The new version of the language includes:

 * A [System-F](https://en.wikipedia.org/wiki/System_F) substrate with distinct namespaces for types and terms.
 * Mixed named and [de Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index) representation for variables.
 * Functions work on vectors of argument and return values, as described in [Types are Calling Conventions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/tacc-hs09.pdf). Arity information is expressed directly in the types of functions, instead of needing to be maintained as separate meta-data.
 * A coeffect system using `box` and `run` casts, as described in [Capabilities and Coeffects](http://blog.discus-lang.org/2013/12/capabilities-and-coeffects.html). This system is also part of the Discus language.
 * Structural record and variant types.

## Status

 * The current implementation includes a parser, type checker, big step interpreter, and server for the [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/).
 * The language server produces parser and type checker diagnostics, and allows test cases to be run interactively using the interpeter.
 * The LLVM code generator itself has not yet been ported across from DDC. This will happen next.


## Support

The `salt` executable includes a server that speaks [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). You should be able to get this working with your favourite editor or IDE. Matching [Visual Studio Code](https://code.visualstudio.com/) extension and color themes are available here:

* [Salt Extension for Visual Studio Code](https://github.com/discus-lang/salt-vscode)
* [Salt Plain Theme for Visual Studio Code](https://github.com/discus-lang/salt-theme)

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
