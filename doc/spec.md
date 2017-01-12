Operations
================================================================================

Core Operatives
--------------------------------------------------------------------------------

### @op

    (@op <formal: pattern> <eformal: pattern>
      <body: expr*>)

Create an operative. When the operative is applied, it will match the operand
list with `formal` and the dynamic environment with `eformal`, extending the
static environment. The `body` forms are then evaluated in this extended
environment as if by `@begin`.

#### Derivation

    (@op <formal: pattern> <eformal: pattern> <body: expr*>)
    =
    (#@op <fsym: symbol> <efsym: symbol>
      (@let ((env* (-> ,efsym
                       (assoc/match ,formal ,fsym)
                       (assoc/match ,eformal ,efsym))))
        (@if (= (key env*) 'Some)
          (eval (list* @begin ,body) env*)
          (error "failed match" ,formal ,fsym))))

### @fn

    (@fn <formal: pattern>
      <body: expr*>)

Create an applicative. When the applicative is applied, it will evaluate its
operands to create an argument list which it will then match with `formal` and
the dynamic environment with `eformal`, extending the static environment. The
`body` forms are then evaluated in this extended environment as if by `@begin`.

#### Derivation

    (@op <formal: pattern> <body: expr*>)
    =
    (wrap (@op <formal: pattern> #_ <body: expr*>))

### @def

    (@def <pat: pattern> expr) --> env*

#### Derivation

    (@def @def
      (@op (pat expr) env
        (assoc/match env pat (eval expr env))))

### @begin

    (@begin <stmts: expr*>)

#### Derivation

    (@def @begin
      (@op stmts env
        (@match stmts
          (((and ('@def & #_) definition) & stmts)
           (eval (list* @begin stmts) (eval definition env)))
          (() ())

Core Applicatives
--------------------------------------------------------------------------------

### `assoc/match`

    (assoc/match env pat val) --> (tag Some env*) | (tag None)

Pattern match `val` against `pat` and `assoc` any bindings created in the
process to `env`. Return

* `(tag Some env*)` when the match succeeds
* `(tag None)` when the match fails

Patterns are datums with the syntax:

    pattern ::= <symbol>
              | #_
              | (<pattern*>)
              | (<pattern*> & <pattern>)
              | [<pattern*>]

### `eval`

    (eval env expr)

Evaluate the datum `expr` in `env` and return the result.

### `tag`

    (tag tag value)

Create a tagpair.

### `type`

    (type value)

Return the type descriptor of `value`, e.g. `:symbol` for symbols and the tag
for tagpairs.

### `value`

    (value value)

Return the value component of tagpairs, map entries and the like.

Data
================================================================================

Atoms
--------------------------------------------------------------------------------

### Int `...-1, 0, 1...`

#### Evaluation

Self-evaluating.

### Symbol `foo`

Symbols are like strings, but they

* are 'hash-consed'/'interned'
    - => two symbols with the same print name are always `identical?`
* precompute their hash
    - => hashing a symbol is an amortized O(1)! operation

#### Purpose

Acting as variables for evaluation.

#### Evaluation

Look symbol up in current environment (as if by `get`).

### Keyword `:foo`

Keywords are like symbols except that they are self-evaluating.

#### Purpose

General purpose tags and keys.

#### Evaluation

Self-evaluating.

### Ignore `#_`

#### Purpose

When used as a pattern completely ignores the value to match against, performing
no checking nor binding.

#### Evaluation

Self-evaluating.

### Unit `#()`

#### Purpose

A useless, inert value that is returned by side-effecting operatives and
applicatives, `(@begin)` etc.

#### Evaluation

Self-evaluating.

Collections
--------------------------------------------------------------------------------

### List `(<element*>)`

#### Purpose

Representing combinations in code. General purpose immutable singly linked list
(although singly linked lists are rarely the best option).

#### Evaluation

Evaluate head, then call the result with (unevaluated) tail. If head evaluated
to an applicative, the tail will be evaluated elementwise to a vector (as if by
`(into [] (map (@_ eval #_ (current-env)) operands))`).

### Map `{<(key value)*>}`

#### Purpose

First class environments, general purpose associative data structure.

#### Evaluation

Returns a corresponding map with the keys and values evaluated. If two key exprs
evaluate to the same value, an error is signaled.

### Vector `[<element*>]`

#### Purpose

General purpose immutable array.

#### Evaluation

Returns a corresponding vector with the values evaluated.

### Tagpair

#### Purpose

Custom types for dispatch and encapsulation.

#### Evaluation

Self-evaluating. Note that the tag and value are not evaluated *even if they are
not self-evaluating*.
