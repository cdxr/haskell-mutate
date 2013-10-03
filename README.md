# mutate

`mutate` is a Haskell package providing typeclasses that are polymorphic over
mutable variables. It is an attempt to formalize properties that are common to
IORefs, STRefs, TVars, and StateVars.

`Control.Mutate` exports the typeclasses ReadVar, WriteVar, and EditVar for
variables that support retrieving, overwriting, and modifying. Each typeclass
is parameterized over the variable type and the monad in which the variable is
mutable.

`Control.Mutate.Atomic` seeks to provide similar abstractions for concurrent
state variables. It is currently in early development.


## motivation

Suppose you have written the following function:

```haskell
addTwo :: IORef Int -> IO ()
addTwo v = modifyIORef v (+2)
```

It is easy to see at a glance that this function does not read the value of the
IORef, it only maps a function over it. However, the type alone does not
guarantee that the value has not been read and stored somewhere else. Using
`Control.Mutate`, we can write:

```haskell
addTwo' :: (EditVar IO v) => v Int -> IO ()
addTwo' v = editVar v (+2)
```

The `IORef` instance of `EditVar` implements `editVar` as `modifyIORef`, so
these two definitions are equivalent. However, the `EditVar` typeclass does not
provide an equivalent of `readIORef`, so this constraint guarantees that
`addTwo'` can modify the variable but may not observe it. As an added bonus,
`addTwo'` will now work with `StateVar`s as well as any other `v` for which
`EditVar IO v` is implemented.

`Control.Mutate` provides a few related types and typeclasses enable further
abstraction.
