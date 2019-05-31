
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
 :+=  '#Type'
  |   '#Data' | '#Region'
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

- `Prm` gives the list of baked-in primitive types which are needed to classify type and term level constructs that are described in the language definition. The implementation may also provide other machine level types, but they are listed separately. `#Type` (at level 3) classifies well formed kinds. `#Data` and `#Region` (at level 2) are the kinds of data and region types. The others (at level 1) are standard type constructors.


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

  |   mifs n Termⁿ Termⁿ Term       ('if' '{' (Term '→' Term);* '}'
                                          'else' Term)

  |   mrec n Lblⁿ Termⁿ             (∏ '[' (Lbl '=' Term),* ']')
  |   mprj   Term Lbl               (Term '.' Lbl)

  |   mvnt   Lbl  Term Type         ('the' Type  'of' '`' Lbl Term)

  |   mcse n Term Lblⁿ Typeⁿ Termⁿ  ('case' Term 'of'
                  Term?                   '{' (Lbl '[' (Var ':' Type),* ']' → Term);+ '}'
                                          ('else' Term)?)

  |   mbox   Term                   ('box' Term)
  |   mrun   Term                   ('run' Term)

  |   mlst n Type Termⁿ             ('[list' Type '|' Term,* ']')
  |   mset n Type Termⁿ             ('[set'  Type '|' Term,* ']')
  |   mmap n Type Type Termⁿ Termⁿ  ('[map'  Type Type '|' TermMapBind,* ']')

  |   mprc Proc                     ('proc' Proc)
  |   mblc Bloc                     ('bloc' Bloc)

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


## Procs and Blocs

Procs (Procedures) provide the statement/expression model of computation, with graph-like control flow and mutable storage cells. Control constructs appear as statements only, which ensures the expressions can always be flattened into straight line code during compilation. Storage cells are abstract, in the sense that references (addresses) to them cannot be taken. This means that updates to them can always be converted into Static Single Assignment (SSA) form.

Blocs (Code Blocks) provide the pure SSA model of computation, with tree-like control flow. SSA is particularly easy to convert into target assembly languages such as LLVM. Control flow is restricted to be tree-like so that join points do not appear within the bloc body. Join points should be expressed using calls to continuation functions, which is cleaner than using the phi-nodes of other SSA representations. Blocs do not support mutable storage cells, so updates should be translated into SSA form, or performed using primitive load/store operators that work on raw memory addresses.

The grammar for procs and blocs shares forms for expressions. The common forms are listed below as `_Exp`, `_Args` and `_MapBinds`. These forms are intended to be instantiated to `ProcExp`, `ProcArgs` and `ProcMapBinds` for procs, and `BlocExp`, `BlocArgs` and `BlocMapBinds` for blocs.


### Procs

```
Proc
 ::=  pyld   Term                               ('yield'  Term)
  |   pclv n Var  TermArgsⁿ                     ('call'   Var TermArgsⁿ)
  |   pclp n Prm  TermArgsⁿ                     ('call'   Prm TermArgsⁿ)
  |   pseq n Varⁿ Proc Proc                     ('seq'    Var '=' Proc ';' Proc)

  |   plch   Types of Proc                      ('launch' Types 'of' Proc)
  |   pret   Term                               ('return' Term)

  |   pcel   Name Type Term Proc                ('cell'   Bind ':' Type '←' Term ';' Proc)
  |   pupd   Name Term Proc                     ('update' Bound '←' Term ';' Proc)

  |   pwhs n (Term Proc)ⁿ Proc                  ('whens'  '{' ProcWhensAlt;+ '}' ';' Proc)

  |   pmch   Term ProcAlt+ Proc                 ('match'  Term '{' ProcMatchAlt;+ '}' ';' Proc)

  |   pllp   Proc Proc                          ('loop'   Proc ';' Proc)
  |   pbrk                                      ('break')
  |   pcnt                                      ('continue')

  |   pwll   Term Proc ';' Proc                 ('while'  Term Proc ';' Proc)

  |   pent   Term ProcBind                      ('enter'  Term 'with' ProcBind ';' Proc)
  |   plve                                      ('leave')

ProcWhensAlt ::= Term Proc                      (Term '→' Proc)
ProcMatchAlt ::= Lbl (Var Type)* Proc           (Lbl '[' (Var ':' Type)* ']' → Proc)
ProcBind     ::= Name TermParams+ Type Proc     (Name TermParams+ ':' Type '=' Proc)
```


### Blocs

```
Bloc
 ::=  bloc n BlocBody                       ('bloc' BlocBody)

BlocBody
 ::=  blet n Varⁿ BlocExp BlocBody BlocBody ('let' '[' (Var (':' Type)?),* ']' '=' BlocExp ';' BlocBody)

  |   bifs n BlocExpⁿ BlocBodyⁿ BlocBody    ('if' '{' (BlocExp    '→' BlocBody);*
                                                      'otherwise' '→' BlocBody '}')

  |   bcse n BlocExp Lblⁿ Typeⁿ BlocBodyⁿ   ('case' BlocExp 'of'
                                             '{' (Lbl '[' (Var ':' Type),* ']' → BlocBody);+ '}')

  |   bexp   BlocExp                        (BlocExp)
  |   bblk   Bloc                           (Bloc)

BlocExp
 ::=  ... shared Exp forms ...
```

Blocs contain a body with tree-like control flow, rather than graph-like control flow as with procs. Bloc bodies are a proper fragment of proc bodies, namely the ones that can be written without the proc 'do' construct or cell load.

Bloc bodies consist of `let`-binding, `if`-branching, `case`-branching, expression evaluation and nested `bloc` constructs. Note that the `if` form requires an `otherwise` branch to ensure the control flow is tree-like.

Bloc expressions are the shared forms only, without the ability to load from storage cells.

