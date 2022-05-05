module Effectful.Colog.Core.Effect
  ( Log,
    LogActionEff,
    runLog,
    logMsg,
    logMsgs,
    withLog,
  )
where

import Colog.Core (LogAction (..), hoistLogAction)
import Data.Foldable (traverse_)
import Effectful
  ( Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    UnliftStrategy (SeqUnlift),
    raise,
    type (:>),
  )
import Effectful.Dispatch.Dynamic (localLiftUnlift, reinterpret, send)
import Effectful.Reader.Static (ask, local, runReader)

-- | An effect for composable, contravariant and comonadic logging using the @co-log@ library.
data Log msg :: Effect where
  LogMsg :: msg -> Log msg m ()
  WithLog :: (LogAction m msg -> LogAction m msg) -> m a -> Log msg m a

type instance DispatchOf (Log msg) = 'Dynamic

-- | 'LogAction' that works directly with the 'Eff' monad.
type LogActionEff es msg = LogAction (Eff es) msg

-- | Run a 'Log' effect.
--
-- This function is the effectful version of 'Colog.usingLoggerT'
runLog :: forall msg a es. LogActionEff es msg -> Eff (Log msg ': es) a -> Eff es a
runLog logAction = reinterpret (runReader logAction) $ \env -> \case
  LogMsg msg -> do
    LogAction action <- ask
    raise $ action msg
  -- This implementation looks and feels strange,
  -- however this is the only way I would get it to typecheck.
  WithLog f m -> localLiftUnlift env SeqUnlift $ \lift unlift -> do
    action <- ask
    let f' :: LogActionEff es msg -> LogActionEff es msg
        f' = hoistLogAction (runReader action . unlift) . f . hoistLogAction (lift . raise)
    local f' (unlift m)

-- | Perform logging action with given @msg@.
--
-- The effectful version of 'Colog.logMsg'.
logMsg :: forall msg es. Log msg :> es => msg -> Eff es ()
logMsg = send . LogMsg

-- | The effectful version of 'Colog.logMsgs'.
logMsgs :: forall msg f es. (Foldable f, Log msg :> es) => f msg -> Eff es ()
logMsgs = traverse_ logMsg

-- | The effectful version of 'Colog.withLog'.
withLog :: forall msg a es. Log msg :> es => (LogActionEff es msg -> LogActionEff es msg) -> Eff es a -> Eff es a
withLog f m = send (WithLog f m)