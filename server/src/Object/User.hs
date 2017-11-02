module Object.User where

data User = User { userid :: Integer, username :: String } deriving (Eq, Show)