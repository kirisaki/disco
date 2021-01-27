{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Environment

import Discord
import Discord.Types
import qualified Discord.Requests as R


main :: IO ()
main = do
    token <- lookupEnv "DISCO_TOKEN" >>= \case
                Just v -> pure $ T.pack v
                Nothing -> error "'DISCO_TOKEN' is not set"
    userFacingError <- runDiscord $ def 
        { discordToken = token
        , discordOnEvent = eventHandler
        }
    putStrLn $ T.unpack userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler = \case
    MessageCreate m -> when(not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
    MessageReactionAdd r -> when (emojiName (reactionEmoji r) == "\x1f914") $ do
               liftIO . print $ T.unpack (emojiName (reactionEmoji r))
               _ <- restCall (R.CreateMessage (reactionChannelId r) ":thinking:")
               pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower
