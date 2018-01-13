module Element where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import DOM (DOM)

foreign import data JQuery :: Type

foreign import data JQueryEvent :: Type

foreign import body
  :: forall eff
   . Eff (dom :: DOM | eff) JQuery

foreign import on
  :: forall eff a
   . String
   -> (JQueryEvent -> JQuery -> Eff (dom :: DOM | eff) a)
   -> JQuery
   -> Eff (dom :: DOM | eff) Unit

foreign import getPageX
  :: forall eff
   . JQueryEvent
  -> Eff (dom :: DOM | eff) Number


foreign import getElementById
  :: forall eff
   . String
  -> Eff (dom :: DOM | eff) JQuery

foreign import getPageY
  :: forall eff
   . JQueryEvent
  -> Eff (dom :: DOM | eff) Number

