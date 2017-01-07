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
- [x] Iteration
    * Got tail recursion and fexprs
- [x] Selection
    * [x] `@if` (other branches?)
        - Since we have maps and closures the lambda calculus/Smalltalk trick
          can be used:
            * `(@if foo bar baz)` == `((tagval foo) bar baz)`
            * (assuming `#t` == `(tag :True (@op (then _) env (eval then env))`))
            * This avoids adding a branching primitive to the language.
            * Tracing can remove overhead.
    * [ ] Ad-hoc polymorphism (everything is 'generic' already)
        - JS/Lua style (`(send foo :bar 3)` == `((get foo :bar) 3)`)?
        - Multimethods?
            * CLOS/Julia-style?
            * Predicate dispatch?
            * Clojure-style?
        - Type classes?
        - Avoid inheritance!
        - Should be possible to do this within the language (like Scheme, but
          nevertheless having some standard approach)
- [x] Properties
    * Can everything have props?
        - i.e. in Ruby even ints can have instance variables
        - Doesn't really make sense for everything to have general-purpose
          associative storage on the side.
            * In this sense, everything is not an object. (Neither is everything
              based on message passing, since we have closures **and** want more
              general dispatch than sends provide).
        - However, the Clojure metadata approach could be used to ameliorate the
          lack of syntax objects somewhat.
            * Could also store that info in a pointer-hash-indexed map on the
              side if most objects don't have metadata
    * Prop visibility
        - Public/private segregation?
            * Maybe store privates in the metadata?
        - All or nothing?
            * Kernel encapsulation/Arc tagging hides the fact that an object
              has props at all.
    * Conclusion: Maps are maps, other things are not. Tagpairs can be used for
      encapsulation (as well as dispatch somewhere down the line). Metadata is a
      user-friendly idea, but not central to the 'object model'.
