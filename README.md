gore-and-ash-async
==================

The module provides API for embedding concurrent IO actions into main game loop for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/Teaspot-Studio/gore-and-ash-async.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `AsyncT`:
``` haskell
type AppStack = ModuleStack [AsyncT, ... other modules ... ] IO
```

And derive `MonadAsync` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadAsync)
```