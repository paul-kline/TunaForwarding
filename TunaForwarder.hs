{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TunaForwarder where

import Web.Scotty hiding (get,put)
import qualified Web.Scotty as Scotty
import Control.Monad.State.Strict (liftIO)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import Data.Aeson
import Network.Http.Client hiding (get)
import Data.ByteString.Lazy (toStrict)
import ByteStringJSON
import Control.Monad.State.Strict
import Control.Concurrent.MVar

import Control.Concurrent 
import Data.List (intersperse)
import System.Timeout
import System.IO.Streams (InputStream, OutputStream, stdout) 
import Data.ByteString.Lazy hiding (putStrLn, init, tail)
import qualified System.IO.Streams as Streams
import System.Timeout
mainport = 55111

main' = do
  Scotty.scotty mainport $ do
            Scotty.get "/" $ do
               
               Scotty.text "serving\n"  --a quick way to check if the CA is 
                                                --serving, client should see "foobar" 
                                                --(Note: unqualified get is ambiguous).

            Scotty.post "/" $ do
              liftIO $ putStrLn "RECEIVED POST" 
              a <- (param "request") :: ActionM LazyText.Text             
              let a' = LazyEncoding.encodeUtf8 a
              let jj' = Data.Aeson.eitherDecode a' :: Either String (Hostname, Hostname,Port,Data.Aeson.Value)
              
              case jj' of 
                Right (whoisSending, toip,toport, val) -> do 
                  liftIO $ putStrLn $  "successfully received: " ++ (show (whoisSending, toip,toport,val))
                  conn <- liftIO $ mysend toip toport val 
                  mvalback <- liftIO  $ timeout 10000 $ myReceive conn 
                  closeConnection conn
                  case mvalback of 
                    Nothing -> do 
                       Scotty.json (toJSON ( ("no responseBack","") :: (String, String)))
                    Just valback -> do 
                       Scotty.json valback
                _ -> do 
                   Scotty.json (toJSON (("Error: received something weird not in the triplet form (from,to,Value).","") :: (String, String)))
myReceive :: Connection -> IO Value                 
myReceive c = do 
   receiveResponse c (\p i -> do
                          x <- Streams.read i
                          case x of
                             (Nothing) -> return (toJSON (("",5) :: (String,Int) ) )
                             (Just something) -> do
                                 --putStrLn $ "Here is what it fails to parse: " ++ (show something) --print something
                                 let caresp = (Data.Aeson.eitherDecode (fromStrict something) :: Either String Value)
                                 case caresp of
                                        (Left err) -> return ( toJSON ("Error decoding shared thing. Error was: " :: String, err))
                                        (Right r)  -> return ( r)
                  )
                  
mysend :: Hostname -> Port -> Data.Aeson.Value -> IO Connection                  
mysend toip toport val = do 
   c <- openConnection toip toport
   putStrLn "Just opened a connection to send on http"
   q <- buildRequest $ do
      http POST "/"
      setAccept "text/html/json"
      setContentType "application/x-www-form-urlencoded"
--Prelude.putStrLn ( "Request: " ++ (show req))
   let nvs = [("request", (toStrict (Data.Aeson.encode val)))]
--Prelude.putStrLn "about to send request"
   let x = encodedFormBody nvs
--print "Made it here yaaaaaaaaaaaay"
   unit <- sendRequest c q (x)
   return c 
