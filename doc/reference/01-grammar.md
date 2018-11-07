
## Symbols

```
Var     → (variable name,       like "name")
Con     → (constructor name,    like "Con")
Sym     → (symbol name,         like "'Name")
Prm     → (primitive name,      like "#add")
Lbl     → (label name,          like "foo")
```


## Modules

```
Module
 ::=    'type'  Con TypeParams ':' Type '=' Type
  |     'term'  Var TermParams ':' Type '=' Term

  |     'test' (Var)? 'print' Term
  |     'test' (Var)? 'assert' Term
  |     'test' Term

```

## Types

```
Type
 ::=    Var                                     -- type variable
  |     Con                                     -- type constructor
  |     Type Types                              -- type application
  |     'λ' TypeParams '→' Type                 -- type abstraction
  |     '∀' TypeParams '.' Type                 -- forall type
  |     Types '→' Types                         -- function type
  |     TypeRecord                              -- record type

Types
 ::=    '{' Type;+ '}'                          -- type sequence

TypeParams
 ::=    '{' (Var ':' Type);+ '}'                -- type parameters

TypeRecord
 ::=    '[' (Lbl ':' Type),* ']'                -- record type
```


## Terms

```
Term
 ::=    Terms                                   -- term sequence

  |     Con | Prm | Sym                         -- term literal

  |     Var                                     -- term variable
  |     'λ'   TermParams '→'  Term              -- term abstraction
  |     Term  TermArgs                          -- term application

  |     'let' TermBind   'in' Term              -- let expression

  |     TermRecord                              -- record construction
  |     Term '.' Lbl                            -- record projection

  |     '[list|' Term,* ']'                     -- list construction
  |     '[set|'  Term,* ']'                     -- set construction
  |     '[map|'  (Term ':=' Term),* ']'         -- map construction

Terms
 ::=    '{' Term;+ '}'                          -- term sequence

TypeSigs
 ::=    '{' (Var ':' Type);* '}'

TermParams
 ::=    '@' TypeSigs  | TypeSigs                -- type or term parameters.

TermArgs
 ::=    '@' Types     | Terms                   -- type or term arguments.

TermBind
 ::=    '{' Var;* '}' '=' Term                  -- let binding

TermRecord
 ::=    '[' (Lbl '=' Term),* ']'                -- record construction
```
