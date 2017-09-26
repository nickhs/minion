module Module.Exec (
  ) where

import Module (Module(..), Modulable(..), Result(..), DidUpdate(..))

data ExecModule = ExecModule {
  exec :: FilePath -> [Text]
}
