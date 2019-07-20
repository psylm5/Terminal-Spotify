{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Haskify
import Types
import Control.Monad.Trans.Maybe as M
import Control.Monad.Trans.State.Lazy as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Extra
import Control.Monad
import Data.Aeson
import System.Exit
import Data.Char
import Data.String.Utils

clientId :: B.ByteString
clientId = B.pack "50458bd0a53d431da383bda6bfcc1b07" 

clientSecret :: B.ByteString
clientSecret = B.pack "8c7d5daaedfa43f3a489d7af7ff11590"

main = do 
    s <- readFile "refresh_token.txt"    
    M.runMaybeT $ S.runStateT (progLoop) ((Token undefined undefined), (T.pack . strip) s)

progLoop :: HaskifyAction ()
progLoop = do
        refreshToken clientId clientSecret
        current <- getCurrentPlayback
        let output = ["\n"++ (T.unpack $ current_track current) ++ " by " ++ (T.unpack $ current_name current) ++ " is currently " ++ (status $ is_playing current) ++ " on " ++ (T.unpack $ device_name current),
                      "Choose one of the following options: ",
                      "[N]ext track",
                      "[S]top",
                      "[P]lay",
                      "[Q]uit"]
        mapM_ (liftIO . putStrLn) output
        input <- liftIO $ getMenuChar
        case input of 
          'n' -> skipPlayback
          's' -> pausePlayback
          'p' -> resumePlayback
          'q' -> liftIO $ exitSuccess
        progLoop
      where
         status True = "playing"
         status False = "paused"

getMenuChar :: IO Char
getMenuChar = do
  char <- getChar
  case toLower char of
    'n' -> return 'n' 
    's' -> return 's'
    'p' -> return 'p'
    'q' -> return 'q'
    _   -> getMenuChar
