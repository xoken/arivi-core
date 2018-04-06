import           Arivi.Network.Connection

instance ConnectionState IDLE_STATE where
    -- doHandShakeInitNE     a fh = if opcode fh == HANDSHAKE_INIT
    --                              then do -- TODO send HANDHAKE_ACK response
    --                                     return ()
    --                                      -- * transition to INVITED STATE
    --                              else do
    --                                 return ()

    doSendMessage         a fh = undefined
    doHandShakeAckNE      a fh = undefined
    doInitiateHandShake   a fh = undefined
    doTerminateConnection a fh = undefined
    doOfferNE             a fh = undefined
    doCloseNE             a fh = undefined
    doErrorNE             a fh = undefined
    doPingNE              a fh = undefined
    doPongNE              a fh = undefined



