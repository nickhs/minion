module PrivEsc (
    username,
    executable,
    args,
    HasUsername,
    HasExecutable,
    HasArgs,
    PrivEsc(..),
) where

import Data.Text (Text)
import Control.Lens (makeFieldsNoPrefix)

data PrivEsc = PrivEsc {
    _username :: Text,
    _executable :: FilePath,
    _args :: [Text]
}
makeFieldsNoPrefix ''PrivEsc
