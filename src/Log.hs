module Log (
    LogMsgType(..),
) where

data LogMsgType =
  Debug |
  Info |
  Warn |
  Error |
  Cmdin |
  Stdin |
  Stdout |
  Stderr deriving (Show, Eq, Ord)
