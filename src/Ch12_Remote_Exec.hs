{-# LANGUAGE
    TemplateHaskell
  , KindSignatures
#-}


module Main
  where



import Control.Distributed.Process
  ( Process
  , say
  , call
  , getSelfNode
  )

import Control.Distributed.Process.Closure
  ( remotable
  , functionTDict
  , mkClosure
  )

import Control.Distributed.Process.Node
  ( initRemoteTable
  )

import Control.Distributed.Process.Backend.SimpleLocalnet
  ( initializeBackend
  , startMaster
  )



rpc :: String -> Process Int
rpc str = return (length str)



remotable ['rpc]



foo :: Process ()
foo =
  do
    node <- getSelfNode
    str <- call $(functionTDict 'rpc) node ($(mkClosure 'rpc) "foo")
    say (show str)



main :: IO ()
main =
  do
    backend <- initializeBackend "localhost" "9001" (__remoteTable initRemoteTable)
    startMaster backend $ \ _ -> foo

