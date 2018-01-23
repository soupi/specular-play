module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Monoid (mempty)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (fixFRP_, weaken)
import Specular.FRP.Base (foldDyn)


main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = counter


-- | A simple counter widget.
-- 
-- It will look like this:
--
-- Count: 7
-- ____________ _____________
-- | Increment| | Decrement |
-- ------------ -------------
--
-- Important notes:
-- 1) The value of the counter is dependant on the events of the two buttons
-- 2) Elements are drawn in order of the bind, so in the case of `e1 *> e2`, e1 will be drawn before e2 in the html
-- 3) We want to display the counter before displaying the buttons
-- 4) The buttons return `Event`s (`incrE` and `decrE`) which are needed in order to calculate the counter value
-- 5) How do we reference the events `incrE` and `decrE`?
-- => We use `fixFRP_` which will feed back the result of the computation, if we return the events, we can reference them in the beginning
--
counter :: forall m. MonadWidget m => m Unit
counter = do
  el "div" do
    fixFRP_ $ \omega -> do
      -- Use the events of the buttons to create a dynamic value that counts the key presses
      incrs <- foldDyn (+) 0 (1 <$ omega.incrE)
      decrs <- foldDyn (+) 0 (1 <$ omega.decrE)

      -- Substract the number of decrements from the number of increments
      let val = (\x y -> x - y) <$> incrs <*> decrs
      -- Display that value
      el "p" $ dynText $ weaken $ map (("Count: " <> _) <<< show) val

      -- Display buttons which return click events
      incrE <- buttonOnClick (pure mempty) $ text "Increment"
      decrE <- buttonOnClick (pure mempty) $ text "Decrement"

      -- Return the click events, which will be fed back to this very function
      pure {incrE, decrE}

  pure unit
