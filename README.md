
Arivi Network Protocol: A secure network protocol akin to SSH/SCP that supports both TCP & UDP transports and offers the following features.
- Chunking
- Multiplexing/Demultiplexing
- End to end encryption (IES key exchange, with perfect forward secrecy)
- Authenticated messaging Poly1305-ChaCha20 (AEAD). 

P2P Protocol suite : 
- Peer Lookup/Discovery using an improved Kademlia DHT protocol. Fortified from various forms of eclipse attacks by using a novel peer verification mechanism.
- Remote Proceduce Calls; pull pattern for fetching larger payload from peers e.g. entire blocks 
- Pub-Sub messaging pattern for efficient distributed push notifications in the distributed network as an improvement over traditional gossip protocol
- Peer reputation: a comprehensive peer reputation framework to track the reputation of other peers in the network.
