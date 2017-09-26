module Task (
    Task(..),
    action,
    before,
    after,
    sudo,
    createSimpleTask,
    become,
    HasAction,
    HasAfter,
    HasBefore,
    HasBecome,
) where

import Control.Lens (makeFieldsNoPrefix, set)
import Data.Text (Text)

import qualified Module
import Module (Action)
import PrivEsc (PrivEsc(..))

data Task = Task {
  _action :: Action,
  _before :: [Task],
  _after :: [Task],
  _become :: Maybe PrivEsc
}
makeFieldsNoPrefix ''Task

sudo :: Task -> Task
sudo = set become (Just pe)
    where pe = PrivEsc {
        _username = "root",
        _executable = "/usr/bin/sudo",
        _args = []
    }

createSimpleTask :: Module.Actionable a => a -> Task
createSimpleTask mod = Task {
    _action = Module.toAction mod,
    _before = [],
    _after = [],
    _become = Nothing
}
