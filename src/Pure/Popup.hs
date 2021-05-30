module Pure.Popup where

import Pure (Pure(..))
import Pure.Elm hiding (active,not,ref,Start)
import Pure.Data.Prop.TH
import Pure.Data.Lifted (nullJSV)

import Data.Coerce
import Data.Function ((&))

-- TODO: calculate optimal positioning within onStatusDefault
-- TODO: supply callback with both trigger host and tooltip host nodes

data Status = Opened | Closed
  deriving (Eq,Ord,Show)

data Popup = Popup_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , onStatus :: IO () -> Status -> IO (Features -> Features)
    , variant :: SomeTheme
    }

deriveLocalComponent ''Popup

instance Default Popup where
  def =
    Popup_
      { as = \fs cs -> Div & Features fs & Children cs
      , features = mempty
      , children = mempty
      , onStatus = onStatusDefault (coerce body)
      , variant = mkSomeTheme @Popup
      }

onStatusDefault :: Node -> IO () -> Status -> IO (Features -> Features)
onStatusDefault root toggle = \case
  Opened -> pure (OnDoc "click" (const toggle) . OnMouseLeave (const toggle))
  Closed -> pure (OnMouseEnter (const toggle))

data Model = Model
  { active :: Bool
  , ref    :: Node
  , props  :: Features -> Features
  }

data Msg = Startup | Toggle | SetNode Node

instance Pure Popup where
  view = run (App [Startup] [] [] (pure mdl) upon render)
    where
      mdl = Model False (coerce nullJSV) id

      upon :: Elm Msg => Msg -> Popup -> Model -> IO Model
      upon Startup popup model = do
        f <- (onStatus popup) (command Toggle) Closed
        pure (model :: Model)
          { props = f
          }

      upon (SetNode n) popup model = do
        pure model { ref = n }

      upon Toggle popup model
        | active model = do
          f <- (onStatus popup) (command Toggle) Closed
          pure model
            { ref = coerce nullJSV
            , active = False 
            , props = f
            }
        | otherwise = do
          f <- (onStatus popup) (command Toggle) Opened
          pure model
            { active = True
            , props  = f
            }

      render Popup_ {..} Model {..} =
        case children of
          (popup:rest) -> 
            let 
              -- When active, the `props` function is applied to the tooltip
              cs | active    = (Portal (coerce body) (SetFeatures (props . WithHost (command . SetNode) $ getFeatures popup) popup))
                             : rest
                 | otherwise = rest
              -- When inactive, the `props` function is applied to the trigger
              fs | active    = someThemed variant features
                 | otherwise = props (someThemed variant features)
            in 
              as fs cs
          _ -> 
            as (someThemed variant features) children

instance Theme Popup 
instance Theme Opened where
  theme c =
    is c do
      apply do
        display =: block
instance Theme Closed where
  theme c =
    is c do
      apply do
        display =: none
