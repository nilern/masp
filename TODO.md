- [x] Scope
    * [x] Blocks and definitions (let*-parity)
        - (@begin <stmts: expr*>) runs stmts with `:stmt` continuations
        - (@def <pat> <expr>) will use `:continue-env` to replace the env in the
          `:stmt` continuation, leading to execution of the remaining statements
          in the new environment
    * [x] Recursive definitions (letrec*-parity)
        - (@declare <names: symbol*>) will create assignable slots in env
        - (@def <pat> <expr>) will assign values to these slots (instead of
          `assoc`:ing)
        - A form of mutation!
        - But not so bad, since:
            * `@declare` will replace any previous values with *write-only
              values* (as if by (assoc _ _ #<unbound>))
            * Thus trying to read a `@declare`:d but non-`@def`'d value will
              fail exactly like it would without the `@declare` and forgetting
              the `@declare` will not permit accidental mutation of a parent env
            * The general strategy can also be used to create cyclic lists and
              suchlike
        - `@declare` is a bit annoying but it does permit us to create new
          defining forms (such as `@defn`) without `@begin` having to know about
          them (in order to effectively infer the `@declare`).
            * Even if `@begin` were to be made extensible, it would not be
              possible to define a new definer and use it in the same scope!
            * Defining `@letrec*/@letfn` for those who loathe `@declare` more
              than the proliferation of indentation and scopes is trivial.
