
## Symbols

```
Var     → (variable name,       like "name")
Con     → (constructor name,    like "Con")
Sym     → (symbol name,         like "'Name")
Prm     → (primitive name,      like "#add")
Lbl     → (label name,          like "foo")
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
 ::=    tdat                                    (Data)
  |     tarr Types Type                         (Types '⇒' Type)

  |     tvar Var                                (Var)
  |     tcon Con                                (Con)
  |     tprm Prm                                (Prm)
  |     tapp Type Type*                         (Type Types)
  |     tabs Var+ Type+ Type                    ('λ' TypeParams '⇒' Type)
  |     tall Var+ Type+ Type                    ('∀' TypeParams '.' Type)
  |     text Var+ Type+ Type                    ('∃' TypeParams '.' Type)
  |     tfun Type* Type*                        (Types '→' Types)
  |     trec Lbl*  Type*                        ('[' (Lbl ':' Type)* ']')

Types
 ::=    '{' Type;+ '}'                          (type sequence)

TypeParams
 ::=    '{' (Var ':' Type);+ '}'                (type parameters)

TypeRecord
 ::=    '[' (Lbl ':' Type),* ']'                (record type)
```


## Terms

```
Term
 ::=    Terms                                   (term sequence)

  |     Con | Prm | Sym                         (term literal)

  |     Var                                     (term variable)
  |     'λ'   TermParams '→'  Term              (term abstraction)
  |     Term  TermArgs                          (term application)

  |     'let' TermBind   'in' Term              (let expression)

  |     TermRecord                              (record construction)
  |     Term '.' Lbl                            (record projection)

  |     '[list|' Term,* ']'                     (list construction)
  |     '[set|'  Term,* ']'                     (set construction)
  |     '[map|'  (Term ':=' Term),* ']'         (map construction)

Terms
 ::=    '{' Term;+ '}'                          (term sequence)

TypeSigs
 ::=    '{' (Var ':' Type);* '}'

TermParams
 ::=    '@' TypeSigs  | TypeSigs                (type or term parameters)

TermArgs
 ::=    '@' Types     | Terms                   (type or term arguments)

TermBind
 ::=    '{' Var;* '}' '=' Term                  (let binding)

TermRecord
 ::=    '[' (Lbl '=' Term),* ']'                (record construction)
```
