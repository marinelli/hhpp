
module Main
  where


import Control.Distributed.Process
  ( Process
  , ProcessId
  , expect
  , SendPort
  , getSelfPid
  , newChan
  , spawnLocal
  , sendChan
  , receiveChan
  , say
  , send
  )

import Control.Distributed.Process.Node
  ( initRemoteTable
  )

import Control.Distributed.Process.Backend.SimpleLocalnet
  ( initializeBackend
  , startMaster
  )



server :: Process ()
server =
  do
    pid <- getSelfPid
    (sendport', recvport) <- newChan
    _clientPid <- spawnLocal (client pid sendport')

    sendport <- expect
    sendChan sendport "ping"
    receiveChan recvport >>= say



client :: ProcessId -> SendPort String -> Process ()
client pid sendport =
  do
    (sendport', recvport) <- newChan
    send pid sendport'

    ping <- receiveChan recvport
    sendChan sendport ("pong: " ++ ping)



main :: IO ()
main = do
    backend <- initializeBackend "localhost" "9001" initRemoteTable
    startMaster backend (\ _ -> server)

