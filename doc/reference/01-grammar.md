
## Symbols

```
Var     → (variable name,       like "name")
Lbl     → (label name,          like "foo")
Con     → (constructor name,    like "Con")
Sym     → (symbol name,         like "'Name")
Prm     → (primitive name,      like "#add")
```

- `Var` are names of type and term variables.
- `Con` are names of type and term constructors.
- `Sym` are abstract symbol names.
- `Prm` are names of type and term primitives that are baked into the system.
- `Lbl` are names of labels used to identify record fields.


## Declarations

```
Decl
 ::=  'type'  Con TypeParams?  ':' Type '=' Type      (type bindings)
  |   'term'  Var TermParams?  ':' Type '=' Term      (term bindings)

  |   'test' 'kind'   (Name '=')? Type                (print the kind of a type)
  |   'test' 'type'   (Name '=')? Term                (print the type of a term)
  |   'test' 'eval'   (Name '=')? Term                (print the result of term evaluation)
  |   'test' 'assert' (Name '=')? Term                (assert that a term evaluates to true)

```

- `type` declarations specify type synonyms. If there are no parameters or result kind given then the synonym is assumed to have kind `Data`. If the synonym has any other kind it must be specified.

- `term` declarations specify term synonyms. The result type must be specified.

- `test .. kind ..`   kind checks a type and prints the inferred kind.

- `test .. type ..`   type checks a term and prints the inferred type.

- `test .. eval ..`   evaluates a term and prints the result.

- `test .. assert ..` evaluates a term and checks that the result is `#true`


## Types

```
Type
 ::=  tvar Var                      (Var)
  |   tcon Con                      (Con)
  |   tprm Prm                      (#Prm)

  |   tarr Type+ Type               (Types '⇒' Type)
  |   tfun Type* Type*              (Types '→' Types)

  |   tapp Type Type+               (Type Types)
  |   tabs Var+ Type+ Type          ('λ' TypeParams '⇒' Type)

  |   tall Var+ Type+ Type          ('∀' TypeParams '.' Type)
  |   text Var+ Type+ Type          ('∃' TypeParams '.' Type)

  |   trec Lbl* Types*              ('∏' TypeFields)
  |   tvnt Lbl* Types*              ('∑' TypeFields)

  |   tsyn                          ('sync')
  |   tpur                          ('pure')
  |   tsum Type*                    (Type '+' Type '+' ...)

  |   TypePrims

Types
 ::=  '[' Type;+ ']'

TypeFields
 ::=  '[' (Lbl ':' Type),* ']'

TypeParams
 ::=  '[' (Var ':' Type),+ ']'

Prm
 :+=  '#Data' | '#Region'
  |   '#Unit' | '#Bool' | '#Nat' | '#Int' | '#Text' | '#Symbol'
  |   '#List' | '#Set'  | '#Map' | '#Option'
```

- `tvar`, `tcon`, `tprm` are type variables, type constructors ans primitive types. Type variables start with a lower-case letter, type constructors an upper-case letter and primitive types a '#' and upper-case letter.

- `tarr` is the arrow kind, used to express kinds of type constructors such as `List : #Data → #Data`

- `tfun` the type of a function taking a vector of arguments and returning a vector of results.

- `tapp` and `tabs` are type application and type abstraction. The application form applies a type operator to multiple type arguments at once.

- `tall` and `text` are universal and existential quantification of type variables.

- `trec` and `tvnc` are record and variant types. The lists of labels and field type vectors must have the same length, and are treated as a list of pairs. Variant and record types with the same (label, types) pairs, but in a different order, are different types.

- `tsyn` and `tpur` are the top and bottom of the effect lattice. Computations with the 'sync' effect are assumed to interfere with all others, and computations with the 'pure' effect interfere with no others.

- `tsum` is used to combine effect types.

- `Prm` gives the list of baked-in primitive types which are needed to classify type and term level constructs that are described in the language definition. The implementation may also provide other machine level types, but they are listed separately. `#Data` and `#Region` are the kinds of data and region types. The others are standard type constructors.


### Type Sugar

```
Types => Type                       ≡ Types ⇒ Type

fun     TypeParams -> Type          ≡ λ TypeParams -> Type
forall  TypeParams .  Type          ≡ ∀ TypeParams .  Type
exists  TypeParams .  Type          ≡ ∃ TypeParams .  Type

[record|]                           ≡ ∏[]
[record| L1 : T1, ... Ln : Tn]      ≡ ∏[L1 : T1, ... Ln : Tn]
[L1 : T1, ... Ln : Tn]              ≡ ∏[L1 : T1, ... Ln : Tn]

[variant|]                          ≡ ∑[]
[variant| L1 : T1, .. Ln : Tn]      ≡ ∑[L1 : T1, ... Ln : Tn]
<L1 : T1, ... Ln : Tn>              ≡ ∑[L1 : T1, ... Ln : Tn]

Types -> Types                      ≡ Types → Type
```

All type expressions can be written without using unicode characters, using the sugar described above. The record type `[L1 : T1 .. Ln : Tn]` must have at least one field to disambiguate the syntax it from the empty type vector `[]`.


## Terms

```
Term
 ::=  mvar   Var                    (Var)
  |   mcon   Con                    (Con)
  |   msym   Sym                    ('Sym)
  |   mprm   Prm                    (#Prm)

  |   mmmm n Termⁿ                  ('[' Term,* ']')

  |   mthe n Typeⁿ Term             ('the' Types 'of' Term)

  |   mapp   Term TermArgs          (Term  TermArgs)
  |   mabs   TermParams Term        ('λ'   TermParams '→'  Term)

  |   mlet n Varⁿ Term Term         ('let' TermBind ';' Term)

  |   mifs n Termⁿ Termⁿ Termⁿ      ('if' '{' (Term '→' Term);* 'otherwise' '→' Term '}' )

  |   mrec n Lblⁿ Termⁿ             (∏ '[' (Lbl '=' Term),* ']')
  |   mprj   Term Lbl               (Term '.' Lbl)

  |   mvnt   Lbl  Term Type         ('the' Type  'of' '`' Lbl Term)
  |   mcse n Term Lblⁿ Typeⁿ Termⁿ  ('case' Term 'of'
                                        '{' (Lbl '[' (Var ':' Type),* ']' → Term);+ '}')

  |   mbox   Term                   ('box' Term)
  |   mrun   Term                   ('run' Term)

  |   mlst n Type Termⁿ             ('[list' Type '|' Term,* ']')
  |   mset n Type Termⁿ             ('[set'  Type '|' Term,* ']')
  |   mmap n Type Type Termⁿ Termⁿ  ('[map'  Type Type '|' TermMapBind,* ']')


TermParams
 ::=  mpst n Varⁿ Typeⁿ             ('@' '[' (Var ':' Type),* ']')
  |   mpsm n Varⁿ Typeⁿ             (    '[' (Var ':' Type),* ']')

TermArgs
 ::=  mgst n Typeⁿ                  ('@' '[' Type,* ']')
  |   mgsm n Termⁿ                  (    '[' Term,* ']')
  |   mgsv   Term                   (Term)

TermBind
 ::=  mbnd n Varⁿ Term              ('[' Var;* ']' '=' Term)

TermMapBind
 ::=  mpbd   Term Term              (Term ':=' Term)
```

### Term Sugar

```
Term ':' Type                       ≡ the Type. Term

Term @Type                          ≡ Term @[Type]

fun TermParams -> Term              ≡ λ TermParams → Term

let Var = Term; Term                ≡ let [Var] = Term; Term
do { Var = Term; ... Term }         ≡ let [Var] = Term; ...  Term

if M1 then M2 else M3               ≡ if { M1 → M2; otherwise → M3 }

[record|]                           ≡ ∏[]
[record| L1 = M1, .., Ln = Mn]      ≡ ∏[L1 = T1, ... Ln = Tn]
[L1 = T1, ... Ln = Tn]              ≡ ∏[L1 = T1, ... Ln = Tn]
```

All term expressions can be written without using unicode characters, using the sugar described above.

The term/type application sytnax `Term @Type` desugars to `Term @[Type]`, because evaluation of the argument will always produce a single normal form type, rather than a vector of types. In contrast, the type/type application syntax `Term₁ Term₂` is valid without desugaring as application of a term to a single term is expressed directly in abstract syntax as `(mapp Term₁ (mgsv Term₂))`

Let expression syntax that binds a single value is equivalent to binding a vector containing a single value. Do-expression syntax is desugared to let-expression syntax, where the do-block must end with a statement rather than a binding.

The record term `[ L1 = M1, ... Ln = Mn ]` must have at least one field to disambiguate the syntax from the empty term vector `[]`.

