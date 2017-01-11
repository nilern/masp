# Registers

* ctrl
* arg
* env
* cont

## `ctrl`

The `ctrl` register holds

* the expression to evaluate (`EVAL`)
* the value to continue orexit with (`CONTINUE`)
* the combiner to combine with (`APPLY`)

## `arg`

The `arg` register holds the value of the operands/arguments at `APPLY`. It is
not otherwise utilized.

## `env`

The `env` register holds the current environment for `EVAL` and `APPLY`.

## `cont`

The `cont` register holds the current continuation, which is used by all states.

# States

* EVAL
* CONTINUE
* APPLY

## `EVAL`

* If `ctrl` holds a self-evaluating datum
```
goto CONTINUE
```
* If `ctrl` holds a symbol
```
ctrl <- env[ctrl]
goto CONTINUE
```
    - If the symbol is unbound, should signal an error. But how exactly?
* If `ctrl` holds a list, vector or map
```
ctrl <- the part of ctrl to start evaluation with
cont <- the old cont extended with the appropriate info
goto EVAL
```

## `CONTINUE`

Either restore some things from `cont` to registers and jump to some other
state or manipulate the `cont` and registers and jump to `EVAL` or exit with a
value.

## `APPLY`

* For `PrimOp`:s, call the contained function, letting it change VM state.
* For `CompoundOp`:s, set up things in typical closure fashion.
* For `Applicative`:s, manipulate the continuation so that the operand gets
  evaluated to an argument list.
* For `Continuation`:s, take the continuation to use, `ctrl <- arg` and jump to
  `CONTINUE`.
