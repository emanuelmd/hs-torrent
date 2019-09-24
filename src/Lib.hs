module Lib (sayHello) where

import Prelude

sayHello :: IO ()
sayHello = putStrLn ("Hello" :: Text)
