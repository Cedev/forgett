
module Control.Monad.Forget.Class where:


class MonadForget m where
    remember :: m ()
    forget :: m () -> m ()

    -- laws
    -- remember <* a = a *> remember
    -- forget (remember <* a) = void a
    