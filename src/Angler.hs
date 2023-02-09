module Angler where

-- Record Syntax zur Erstellung von eigenen Datentypen
data Angler = Angler {
    name :: String,
    age :: Int
} deriving (Show)