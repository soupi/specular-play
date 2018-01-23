module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap (singleton)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (dynText, el, elAttr, text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInput, textInputValue)
import Specular.FRP (Event, filterMapEvent, fixFRP, fixFRP_, foldDyn, holdDyn, holdWeakDyn, mergeEvents, never, tagDyn, weaken)

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
      -- Use the events of the buttons to create a dynamic functions that counts the key presses
      let incrs' = ((\x -> x + 1) <$ omega.incrE)
      let decrs' = ((\x -> x - 1) <$ omega.decrE)

      -- Compose all of the functions and then fold them
      val <- foldDyn ($) 0 $ foldlEvents (>>>)
          [ incrs'
          , decrs'
          , const <$> omega.setterE
          ]

      -- Display that value
      el "p" $ dynText $ weaken $ map (("Count: " <> _) <<< show) val

      -- Display buttons which return click events
      incrE <- buttonOnClick (pure mempty) $ text "Increment"
      decrE <- buttonOnClick (pure mempty) $ text "Decrement"
      setterE <- setter

      -- Return the click events, which will be fed back to this very function
      pure {incrE, decrE, setterE}

  pure unit

compose2 :: forall a b c. (b -> c) -> (a -> a -> b) -> a -> a -> c
compose2 f g x y = f (g x y)

foldlEvents :: forall a. (a -> a -> a) -> Array (Event a) -> Event a
foldlEvents f = foldl (mergeEvents pure pure (compose2 pure f)) never


setter :: forall m. MonadWidget m => m (Event Int)
setter = el "div" $ do
  txtE <- fixFRP $ \omega -> do
    attrs <- weaken <$> holdDyn mempty never
    txt <- textInput
      { initialValue: ""
      , attributes: attrs
      , setValue: "" <$ omega.setE
      }
    setE <- buttonOnClick (pure mempty) $ text "Set"
    let filtered = filterMapEvent fromString $ tagDyn (textInputValue txt) setE

    pure (Tuple {setE: filtered} $ tagDyn (textInputValue txt) setE)

  isValid <- holdWeakDyn $ flip map txtE $
      \str -> case fromString str of
        Nothing -> "Not a number."
        Just _ -> ""

  elAttr "p" (singleton "style" "color: red") $ dynText isValid

  pure $ filterMapEvent fromString txtE

