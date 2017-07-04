{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where


import Control.Distributed.Process
  ( Process
  , ProcessId
  , send
  , expect
  , say
  , getSelfNode
  , getSelfPid
  , spawn
  , liftIO
  )

import Control.Distributed.Process.Closure
  ( remotable
  , mkStaticClosure
  )

import Control.Distributed.Process.Node
  ( initRemoteTable
  )

import Control.Distributed.Process.Backend.SimpleLocalnet
  ( initializeBackend
  , startMaster
  )

import Control.Distributed.Process.Extras.Time  ( TimeUnit(..) )
import Control.Distributed.Process.Extras.Timer ( sleepFor )


import Control.Monad ( replicateM_ )
import Data.Binary   ( Binary )
import Data.Typeable ( Typeable )
import GHC.Generics  ( Generic )
import System.Random ( Random(randomRIO) )



data SummerMsg
  = Add Int ProcessId
  | Value Int

  deriving (Show, Typeable, Generic)



instance Binary SummerMsg



summerProc :: Process ()
summerProc = go 0

  where
    go :: Int -> Process ()
    go s =
      do
        delay <- liftIO $ randomRIO (0, 2000) :: Process Int
        say $ "sleeping for " ++ show delay ++ " milliseconds"
        sleepFor delay Millis

        msg @ (Add num from) <- expect
        say $ "received msg: " ++ show msg
        let s' = s + num
        send from (Value s')
        go s'



remotable ['summerProc]



summerTest :: Process ()
summerTest =
  do
    node <- getSelfNode
    summerPid1 <- spawn node $(mkStaticClosure 'summerProc)
    summerPid2 <- spawn node $(mkStaticClosure 'summerProc)
    mypid <- getSelfPid

    send summerPid1 (Add 5 mypid)
    send summerPid2 (Add 3 mypid)
    send summerPid1 (Add 7 mypid)
    send summerPid2 (Add 1 mypid)

    replicateM_ 4 $
      do
        Value n <- expect
        say $ "updated value: " ++ show n



main :: IO ()
main =
  do
    backend <- initializeBackend
                 "localhost" "12345"
                 (__remoteTable initRemoteTable)
    startMaster backend $ \_ -> summerTest

