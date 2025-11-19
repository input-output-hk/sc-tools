module Convex.Aiken.BlueprintKey (
  BlueprintKey (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.String (IsString (..))
import Data.Text (Text)

newtype BlueprintKey = BlueprintKey {unBlueprintKey :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, IsString)
