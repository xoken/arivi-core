import Network.Arivi.UdpServer  

msgHandler msg = do 
    print "Handle the message"

main = do 
    let ip      = "127.0.0.1"
        udpPort = "8000"
        tcpPort = "7000"
        
    ai <- getAriviInstance ip udpPort tcpPort 

    runAriviInstance ai 
    registerEventHandler dataGramEentHandler msgHandler 
