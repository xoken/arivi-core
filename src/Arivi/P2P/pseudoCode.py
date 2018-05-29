hash<ServiceCode, serviceContext> ServiceContext
                    |-> (minPeerCount, peerType, transport)
hash<ServiceCode, [(peer,timeOut)] > SubscriptionTable
hash<ServiceCode, [(peer,timeOut)] > NotifyTable
hash<messageHash, [serderNodeId] > messageHashToPeerList
hash<servicCode, readTChan> readMessages;

# PARAMETER -> OWN NODEID, IP, PORT
def makeP2PInstance(nodeId, IP, port, outboundPeerQuota):
    creates P2P instance which stores GLOBALLY
    [nodeId, IP, port,outboundPeerQuota, maxConnectionAllowed]

# registerService and update Service requirements.
def registerService(ariviInstance, ServiceCode, minPeerCount,
                    transport, peerType):
    ServiceContext[ServiceCode] = (minPeerCount, peerType, transport)
   if SubcriptionTable doesNot have ServiceCode:
        SubcriptionTable[ServiceCode] = []
    if NotifyTable doesNot have ServiceCode:
        NotifyTable[ServiceCode] = []
    (outboundPeerQuota,maxConn) = askAriviInstance(ariviInstance)
    async(inboundThread,outboundPeerQuota,maxConn)
    async(outboundThread, minPeerCount)

def sendMessage(serviceCode, message):
    ownNodeId = ask P2P instance
    _message = wrap message with (ownNodeId,serviceCode)
    _sendMessage(serviceCode, _message)

def readNotification(serviceCode):
    return serviceReadTChan[sericeCode].read()

def publish(publishData):
    for all nodes in SubcriptionTable and NotifyTable:
        send the publishData

def closeSession(service):
    for peers in session:
        peer.sendMessage(close connection)

#---------------------------------------------------------
def allConnectionCount(SubscriptionTable,NotifyTable):
    total=0
    for serv,peerList in SubscriptionTable:
        total+=len(peerList)
    for serv,peerList in NotifyTable:
        total+=len(peerList)
    return total

def newNotifyList(timeElapsed, peerList): # peerList = (peer,timeOut)
    for all peer,timeout in peerList:
        if timer < timeElapsed:
            remove it from peerList
        else: timeout = timeout - timeElapsed

def inboundThread(serviceCode,outboundPeerQuota,maxConn,
                  SubcriptionTable,NotifyTable):
    while True:
        ti = time()
        subscriptionLen = len(SubcriptionTable[serviceCode])
        total = subriptionLen + len(NotifyTable[serviceCode])
        if (subcriptionLen/total > outboundPeerQuota) and
            allConnectionCount(SubcriptionTable,NotifyTable) < maxConn:
                accept incoming connection
        else:
            wait for time out
            NotifyTable[serviceCode] = newNotifyList(ti-time(),
                                        NotifyTable[serviceCode])

def outboundThread(serviceCode, maxConn, minPeerCount):
    if allConnectionCount(SubcriptionTable,NotifyTable) < maxConn
       and minPeerSrv< minPeerCount:
           makeConnection()



def serNeedsPeers(serPeer,serContxt):
    for servCode, peers in serPeer:
        if(len(peers)<serContxt.count):
            return False
    return True

def findMaxPeerService(ServiceContext,serCodeInit):
    max = ServiceContext[serCodeInit]
    for serv,context in ServiceContext:
        if max < context.count :
            max = context.count
    return max

def getServicesBelowMinCount(serviceContext, SubcriptionTable,NotifyTable):
    services = listofServices (serviceContext)
    for ser in services:
        minPeerCount <- getMinPeerCount( ser, serviceContext)
        subscription_num = numOfPeers(SubcriptionTable,ser)
        notify_num       = numOfPeers(NotifyTable,ser)
        #total_peer = total_peer + (subscription_num + notify_num )
        if( (outboundsubscription_num + inboundsubscriptions_num ) < minPeerCount):
            servicesBelowMinCount.append(ser)

    return servicesBelowMinCount

def addOutboundPeer():
    while True:
        services = getServicesBelowMinCount(serviceContext, SubcriptionTable, 100, 0.33, 'o')
        if (services not empty):
            peer = ask kademlia()
            peer_services_list = sendInviteRequest(peer)
            for ser in peer_services_list:
                if ser in services:
                    addPeer(peer,ser)
        else:
            wait(5 seconds)

def addInboundPeer():
    tw = currentTime()
    while True:
        if (peers incoming):
            services = getServicesBelowMinCount(serviceContext, NotifyTable, 100, 0.33, 'i')
            peer = getNewConnection()
            peer_services_list = checkServiceProvided(peer) -- one subscribe at a time
            for ser in peer_services_list:
                if ser in services:
                    if (ratio not voilated): addPeer(peer,ser)
        else:
            tw = InbountWait(tw)

def PeerMaintenanceThread(SubcriptionTable, NotifyTable, serContxt):
    async(addOutboundPeer)
    async(addInboundPeer)

def add_a_Peer(ratio, SubcriptionTable, NotifyTable, serviceCode):
    if voilation(ratio) or rand()<0.5:
        fill_outbound(kademlia)
    else: fill_inbound(get_new_connection)

# ===============================================
_sendMessage(serviceCode, _message):
    for all peers in Nodify[serviceCode]:
        sendMessage(peer, _message) # lower layer sendMessage
# ===============================================
# independent thread to keep reading and push in the corresponding TChan
def readMessageForever():
    for all services,[peers] in SubcriptionTable:
        for all peer in [peers]:
            message = read from peer
            (nodeId,serviceCode,_message) = unwrap message
            serviceReadTChan[serviceCode].push(_message)
