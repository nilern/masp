# Core Operatives

## @op

    op ::= (@op <formal: pattern> <eformal: pattern>
                <body: expr*>)

Create an operative. When the operative is applied, it will match the operand
list with `formal` and the dynamic environment with `eformal`, extending the
static environment. The `body` forms are then evaluated in this extended
environment as if by `@begin`.

### Derivation

    (@op <formal: pattern> <eformal: pattern> <body: expr*>)
    =
    (#@op <fsym: symbol> <efsym: symbol>
      (@let ((env* (-> ,efsym
                       (assoc/match ,formal ,fsym)
                       (assoc/match ,eformal ,efsym))))
        (@if (= (key env*) 'Some)
          (eval (list* @begin ,body) env*)
          (error "failed match" ,formal ,fsym))))

## @fn

    fn ::= (@fn <formal: pattern>
                <body: expr*>)

Create and applicative. When the applicative is applied, it will evaluate its
operands to create an argument list which it will then match with `formal` and
the dynamic environment with `eformal`, extending the static environment. The
`body` forms are then evaluated in this extended environment as if by `@begin`.

### Derivation

    (@op <formal: pattern> <body: expr*>)
    =
    (wrap (@op <formal: pattern> #_ <body: expr*>))

## @def

    (@def <pat: pattern> expr) --> env*

### Derivation

    (@def @def
      (@op (pat expr) env
        (assoc/match env pat (eval expr env))))

## @begin

    (@begin <stmts: expr*>)

### Derivation

    (@def @begin
      (@op stmts env
        (@match stmts
          (((and ('@def & #_) definition) & stmts)
           (eval (list* @begin stmts) (eval definition env)))
          (() ())

# Core Applicatives

# assoc/match

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
