{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module Main where

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

import System.Timeout
mainport = 55111

type MyState =  (MVar [(Hostname,MVar Data.Aeson.Value)])  


main = do
  mvarls <- newMVar []
  Scotty.scotty mainport $ do
            Scotty.get "/" $ do
               
               Scotty.text "serving\n"  --a quick way to check if the CA is 
                                                --serving, client should see "foobar" 
                                                --(Note: unqualified get is ambiguous).

            Scotty.post "/" $ do
              liftIO $ putStrLn "RECEIVED POST" 
              --reads in "potential" request (parsing it could fail). 
              --Note: no en/de-crypting is yet taking place.
              a <- (param "request") :: ActionM LazyText.Text
              liftIO $ putStrLn "past reading a"
             -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
             -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
             -- myprint' ("Data received on port: " ++ (show port)) 1
              
              --first converts the Text to UTF8, then then attempts to read a CARequest
              let a' = LazyEncoding.encodeUtf8 a
              liftIO $ putStrLn (show a')
              let jj' = Data.Aeson.eitherDecode a' {-(LazyEncoding.encodeUtf8 a)-} :: Either String (Hostname, Hostname,Port,Data.Aeson.Value)
              
              case jj' of 
                Right (whoisSending, toip,toport, val) -> do 
                  liftIO $ putStrLn $  "successfully received: " ++ (show (whoisSending, toip,toport,val))
                  if toport == 3000
                     then do        
                       ls <-liftIO $ takeMVar mvarls                     
                       case lookup toip ls of 
                         Nothing -> do
                           --then this is the first time we're sending to this IP.
                           --so send normally and add yourself to the waiting list.
                           
                           mv <- liftIO $ newEmptyMVar 
                           let ls' = (whoisSending,mv):ls 
                           liftIO $ putMVar mvarls ls' 
                           mysend toip toport val 
                          
                         (Just mvar) -> do 
                            --if the mvar is empty, they are waiting for it. put it instead of sending. 
                            -- if it's full... I guess send normally? not too sure.
                            b <- liftIO $ isEmptyMVar mvar 
                            if b then do
                                   liftIO $ putMVar mvar val
                                   --mysend toip toport val 
                                 else do 
                                   liftIO yield
                                   liftIO $ putMVar mvar val
                                   mysend toip toport val 
                            liftIO $  putMVar mvarls ls 
                       --end case
                                  
                                  
                                  
                       -- Now I need to add myself to wait for a response
                       ls <- liftIO $ takeMVar mvarls 
                       mymv <- case lookup whoisSending ls of 
                           Nothing -> do 
                             mv <- liftIO $ newEmptyMVar 
                             let ls' = (whoisSending,mv):ls 
                             liftIO $ putMVar mvarls ls' 
                             return mv
                           Just mv ->do 
                              liftIO $ putMVar mvarls ls 
                              return mv 
                         
                       maybeResponseval <- liftIO $ timeout 1000000 (takeMVar mymv)
                       case maybeResponseval of 
                                Nothing -> do 
                                   let str = "Timed out waiting for MVar to fill. No response to give back. Sorry buddy"
                                   liftIO $ putStrLn (show str) 
                                   Scotty.text str  
                                Just res -> do 
                                   liftIO $ putStrLn $ "MVAR HAS SOMETING IN IT YAAAAAAY: " ++ (show res)
                                   Scotty.json (res)       
                     else do 
                       mysend toip toport val
                       Scotty.text "I forwarded for you"
  return ()
mysend toip toport val = do 
   c <- liftIO $ openConnection toip toport
   liftIO $ putStrLn "Just opened a connection to send on http"
   q <- liftIO $ buildRequest $ do
      http POST "/"
      setAccept "text/html/json"
      setContentType "application/x-www-form-urlencoded"
--Prelude.putStrLn ( "Request: " ++ (show req))
   let nvs = [("request", (toStrict (Data.Aeson.encode val)))]
--Prelude.putStrLn "about to send request"
   let x = encodedFormBody nvs
--print "Made it here yaaaaaaaaaaaay"
   liftIO $ sendRequest c q (x)
   liftIO $ putStrLn "Just performed send"