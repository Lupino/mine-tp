{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( someFunc
    ) where


import           Control.Monad.IO.Class          (liftIO)
import           Data.Aeson                      (FromJSON (..), Value (..),
                                                  object, (.:), (.=))
import           Data.Aeson.Types                (typeMismatch)
import           Data.ByteString                 (ByteString)
import qualified Data.Text                       as T (unpack)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                  as LT (toStrict)
import qualified Data.Yaml                       as Y
import           Network.HTTP.Types              (status204, status403)
import           Network.Wai.Middleware.Cors     (CorsResourcePolicy (..), cors,
                                                  simpleCorsResourcePolicy)
import           Network.Wai.Middleware.HttpAuth (basicAuth, extractBasicAuth)
import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode (..))
import           System.Process                  (system)
import           Web.Scotty                      (ActionM, get, header, json,
                                                  middleware, param, post, raw,
                                                  scotty, status)

data User = User
  { user     :: ByteString
  , password :: ByteString
  }

data Config = Config
  { port     :: Int
  , userList :: [User]
  }

instance FromJSON User where
  parseJSON (Object v) =
    User <$> (encodeUtf8 <$> v .: "user")
         <*> (encodeUtf8 <$> v .: "password")
  parseJSON invalid    = typeMismatch "User" invalid

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "port"
           <*> v .: "users"
  parseJSON invalid    = typeMismatch "Config" invalid

requireUser :: (ByteString -> ActionM ()) -> ActionM ()
requireUser done = do
    auth <- fmap (extractBasicAuth . encodeUtf8 . LT.toStrict) <$> header "Authorization"
    case auth of
      Nothing            -> errorNoPermession
      Just Nothing       -> errorNoPermession
      Just (Just (u, _)) -> done u

  where errorNoPermession :: ActionM ()
        errorNoPermession = do
          status status403
          json $ object [ "err" .= ("No permession" :: String) ]

defaultConfig :: String
defaultConfig = "config.yml"

verifyUser :: [User] -> ByteString -> ByteString -> IO Bool
verifyUser [] _ _     = return False
verifyUser (x:xs) u p | user x == u && password x == p = return True
                      | otherwise = verifyUser xs u p

genCmd :: ByteString -> String -> String
genCmd u pos = dockerCmd ++ " sh -c '" ++ shellCmd ++ "'"

  where cmd = "tp " ++ uu ++ " " ++ pos
        uu = T.unpack (decodeUtf8 u)
        dockerCmd = "docker exec -i -t mc"
        shellCmd = "echo \"" ++ cmd ++ "\" | rcon-cli"

runTPHandler :: ByteString -> ActionM ()
runTPHandler u = do
  pos <- param "pos"
  liftIO $ putStrLn $ genCmd u pos
  r <- liftIO $ system $ genCmd u pos

  case r of
    ExitSuccess   -> json $ object [ "result" .= ("OK" :: String) ]
    ExitFailure c ->
      json $ object [ "err" .= ("system error code: " ++ show c) ]

optionsHandler :: ActionM ()
optionsHandler = status status204 >> raw ""

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let path = case args of
               []    -> defaultConfig
               (x:_) -> x

  mc <- Y.decodeFileEither path
  case mc of
    Left e -> print e
    Right Config {..} -> do
      scotty port $ do
        middleware $ cors (const $ Just policy)
        middleware $ basicAuth (verifyUser userList) ""
        post "/api/tp" $ requireUser runTPHandler
        get "/api/tp" $ requireUser runTPHandler

  where policy = simpleCorsResourcePolicy
                   { corsMethods = [ "GET", "POST", "PUT", "DELETE", "OPTIONS" ]
                   , corsRequestHeaders = [ "X-REQUEST-KEY"
                                          , "X-REQUEST-SIGNATURE"
                                          , "X-REQUEST-TIME"
                                          , "X-REQUEST-TYPE"
                                          , "X-REQUEST-NONCE"
                                          , "Content-Type"
                                          ]
                   , corsMaxAge = Just 86400
                   }
