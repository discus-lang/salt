
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
 ::=    'type'  Con TypeParams? (':' Type)? '=' Type    (type bindings)
  |     'term'  Var TermParams?  ':' Type   '=' Term    (term bindings)

  |     'test' 'kind'   ('.' Var) Type                  (print the kind of a type)
  |     'test' 'type'   ('.' Var) Term                  (print the type of a term)
  |     'test' 'eval'   ('.' Var) Term                  (print the result of term evaluation)
  |     'test' 'assert' ('.' Var) Term                  (assert that a term evaluates to true)

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
 ::=  tvar Var                                  (Var)
  |   tcon Con                                  (Con)
  |   tprm Prm                                  (#Prm)
  |   tprm TypePrim

  |   tarr Type+ Type                           (Types '⇒' Type)
  |   tfun Type* Type*                          (Types '→' Types)

  |   tapp Type Type+                           (Type Types)
  |   tabs Var+ Type+ Type                      ('λ' TypeParams '⇒' Type)
  |   tall Var+ Type+ Type                      ('∀' TypeParams '.' Type)
  |   text Var+ Type+ Type                      ('∃' TypeParams '.' Type)
  |   trec Lbl* Type*                           ('∏' TypeFields)
  |   tvnt Lbl* Type*                           ('∑' TypeFields)

  |   TypePrims

Types
 ::=  '[' Type;+ ']'

TypeFields
 ::=  '[' (Lbl ':' Type),* ']'

TypeParams
 ::=  '[' (Var ':' Type),+ ']'

TypePrim
 ::=  '#Data' | '#Region'
  |   '#Unit' | '#Bool' | '#Nat' | '#Int' | '#Text' | '#Symbol'
  |   '#List' | '#Set'  | '#Map' | '#Option'

```

- `tvar`, `tcon`, `tprm` are type variables, type constructors ans primitive types. Type variables start with a lower-case letter, type constructors an upper-case letter and primitive types a '#' and upper-case letter.

- `tarr` is the arrow kind, used to express kinds of type constructors such as `List : #Data → #Data`

- `tapp` and `tabs` are type application and type abstraction. The application form applies a type operator to multiple type arguments at once.

- `tall` and `text` are universal and existential quantification of type variables.

- `trec` and `tvnc` are record and variant types. The lists of labels and types must have the same length, and are treated as set of pairs.

- `tfun` the type of a function taking a vector of arguments and returning a vector of results.

- `tdat` and `trgn` are the kind of data types and region types, which are baked-in primitives.

- `TypePrim` gives the list of baked-in primitive types which are needed to classify type and term level constructs that are described in the language definition. The implementation may also provide other machine level types, but they are listed separately. `#Data` and `#Region` are the kinds of data and region types. The others are standard type constructors.


### Type Sugar

```
 Types '=>' Type                ≡ Types '⇒' Type

 'fun' TypeParams '->' Type     ≡ 'λ' TypeParams '->' Type
 'forall' TypeParams '.' Type   ≡ '∀' TypeParams '.'  Type
 'exists' TypeParams '.' Type   ≡ '∃' TypeParams '.'  Type

 [record|]                      ≡ ∏[]
 [record| L1 : T1 ... Ln : Tn]  ≡ ∏[L1 : T1 ... Ln : Tn]
 [L1 : T1 ... Ln : Tn]          ≡ ∏[L1 : T1 ... Ln : Tn]

 [variant|]                     ≡ ∑[]
 [variant| L1 : T1 .. Ln : Tn]  ≡ ∑[L1 : T1 ... Ln : Tn]
 <L1 : T1 ... Ln : Tn>          ≡ ∑[L1 : T1 ... Ln : Tn]

 Types '->' Types               ≡ Types '→' Type
```

All type expressions can be written without using unicode characters, using the sugar described above. The record type `[L1 : T1 .. Ln : Tn]` must have at least one fields to disambiguate the syntax it from the empty type vector `[]`.


## Terms

```
Term
 ::=  Con                                       (mcon Con)
  |   Prm                                       (mprm Prm)
  |   Sym                                       (msym Sym)

  |   Var                                       (mvar Var)
  |   'λ'   TermParams '→'  Term                (mabs TermParams Term)
  |   Term  TermArgs                            (mapp Term TermArgs)

  |   'let' TermBind   'in' Term                (mlet Var* Term Term)

  |   '⟨' (Lbl '=' Term),* '⟩'                  (mrec Lbl* Term*)
  |   Term '.' Lbl                              (mprj Term Lbl)

  |   '`' Lbl Term                              (mvnt Lbl  Term)

  |   'case'  Term
        'of'  '{' (Lbl → Term);+ '}'
        ('else' Term)?                          (mcse Term Lbl* Term* Term?)


  |   '[list' Type '|' Term,* ']'               (mlst Type Term*)
  |   '[set'  Type '|' Term,* ']'               (mset Type Term*)
  |   '[map'  Type Type '|' TermMapBind,* ']'   (mmap Type Type Term* Term*)

  |   '[' Term,* ']'                            (mmmm Term+)

TermParams
 ::=    '@' '[' (Var ':' Type),* ']'            (mpst Var* Type*)
  |         '[' (Var ':' Type),* ']'            (mpsm Var* Type*)

TermArgs
 ::=    '@' '[' Type,* ']'                      (mgst Type*)
  |         '[' Term,* ']'                      (mgsm Term*)
  |         Term                                (mgsv Term)

TermBind
 ::=    '{' Var;* '}' '=' Term

TermMapBind
 ::=    Term ':=' Term
```

