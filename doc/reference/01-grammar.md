
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
 ::=  Data                                      (tdat)
  |   Types '⇒' Type                            (tarr Type+ Type)

  |   Var                                       (tvar Var)
  |   Con                                       (tcon Con)
  |   Prm                                       (tprm Prm)
  |   Type Types                                (tapp Type Type*)
  |   Types '→' Types                           (tfun Type* Type*)
  |   '∏' TypeFields                            (trec Lbl* Type*)
  |   '∑' TypeFields                            (tvnt Lbl* Type*)
  |   'λ' TypeParams '⇒' Type                   (tabs Var+ Type+ Type)
  |   '∀' TypeParams '.' Type                   (tall Var+ Type+ Type)
  |   '∃' TypeParams '.' Type                   (text Var+ Type+ Type)

Types
 ::=  '[' Type;+ ']'

TypeFields
 ::=  '[' (Lbl ':' Type),* ']'

TypeParams
 ::=  '[' (Var ':' Type),+ ']'
```


## Terms

```
Term
 ::=  Con                                       (mcon Con)
  |   Prm                                       (mprm Prm)
  |   Sym                                       (msym Sym)

  |   '[' Term,* ']'                            (mmmm Term+)

  |   Var                                       (mvar Var)
  |   'λ'   TermParams '→'  Term                (mabs TermParams)
  |   Term  TermArgs                            (mapp Term TermArgs)

  |   'let' TermBind   'in' Term                (mlet Var* Term Term)

  |   '⟨' (Lbl '=' Term),* '⟩'                  (mrec Lbl* Term*)
  |   Term '.' Lbl                              (mprj Term Lbl)

  |   '`' Lbl Term                              (mvnt Lbl  Term)
  |   'case' Term 'of' '{' (Pat → Term);+ '}'   (mcse Term Pat* Term*)

  |   '[list' Type '|' Term,* ']'               (mlst Type Term*)
  |   '[set'  Type '|' Term,* ']'               (mset Type Term*)
  |   '[map'  Type Type '|' TermMapBind,* ']'   (mmap Type Type Term* Term*)

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

