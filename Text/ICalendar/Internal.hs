{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Text.ICalendar.Internal where

import           Control.DeepSeq      (NFData)
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.ByteString.Lazy (ByteString)
import           Data.CaseInsensitive (CI)
import           Data.String
import           Data.Text.Lazy       (Text)
import qualified Data.Time            as Ti
import           Data.Typeable        (Typeable)
import           GHC.Generics         (Generic)


type Line = Int

-- | Content lines, separated into components. 3.1.
data Content = ContentLine Line (CI Text) [(CI Text, [Text])] ByteString
             | Component Line (CI Text) [Content]
               deriving (Show, Eq, Ord, Typeable, Generic)

instance NFData Content

type ICalendarParser state = ExceptT String (RWS Line [String] state)


pErr :: (MonadReader Line m, MonadError String m) => String -> m a
pErr msg = do
    line <- ask
    throwError $ "Line " ++ show line ++ ": " ++ msg

contentLineNamed :: CI Text -> Content -> Bool
contentLineNamed x (ContentLine _ n _ _)
    | x == n         = True
contentLineNamed _ _ = False

printDateTime :: (Ti.FormatTime t, IsString s) => t -> s
printDateTime = fromString . Ti.formatTime Ti.defaultTimeLocale "%C%y%m%dT%H%M%S"
