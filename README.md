# Mutate

`mutate` is a Haskell package that provides abstractions for variables that are
mutable in a particular base monad. For example, TVar is mutable in STM, and
IORef is mutable in IO.

Control.Mutate exports the typeclasses ReadVar, WriteVar, and EditVar for
variables that support retrieving, overwriting, and modifying. Each typeclass
is parameterized over the variable type and the monad. Generic combinators are
provided that lift the typeclass operations from the base monad to any monad
transformer stack.

Control.Mutate.State exports VarState, an abstract monad transformer that
implements the MonadState typeclass. It can be used to operate over any
variable that is an instance of both ReadVar and WriteVar using the familiar
MonadState functions get and put.
