{-# LANGUAGE RecordWildCards #-}

module Lib ( sayIntro ) where

import           Prelude

-- FIXME: Export something more meaningful
data Version = Version
             { programName  :: Text
             , majorVersion :: Integer
             , minorVersion :: Integer
             , patchVersion :: Integer
             , programTag   :: Maybe Text }

instance Show Version where
  show Version{..} = toS $ mconcat lines <> tag programTag

    where
      integer = toS . show
      lines = [ programName
              , "-"
              , integer majorVersion
              , "."
              , integer minorVersion
              , "."
              , integer patchVersion ]

      tag (Just t) = "-" <> t
      tag Nothing  = ""

sayIntro :: IO ()
sayIntro = print programVersion
  where
    programVersion  = Version "HTorrent" 0 0 1 (Just "ALPHA")
