# Arivi
- [P2P Layer](#sec-1)


# Arivi (P2P Layer)<a id="sec-1"></a>

![P2P Architecture](https://ipfs.io/ipfs/QmRGurUXkk7zFcHdB5QJhD6NR9yJ8ZfVVsrHCRLZc6xH3d)


To start communicating with other nodes, a node has to join the network. To do this, the node has to know some other node that already participates in the protocol; this node is called a bootstrap node.

After connecting to the bootstrap node, we receive a list of peers which we’ll use for network communication. Those peers are called neighbors. The list of neighbors should be maintained in such a way that these nodes are online and any node from the network can receive our messages. Moreover, messages should be delivered efficiently.

To achieve this, the Xoken blockchain uses the Kademlia DHT protocol simply as a method of peer discovery.

The P2P Layer provides a suite of methods to both Kademlia and *Block Sync* application like multiplexing, framing, chunking, encryption and messaging. The Arivi layer also handles the network layer protocols like UDP and TCP making sure that the Kademlia and *Block Sync* layer containing the business logic are agnostic of all the network protocols used.

## Flow of Node

1. Use Kademlia to get the nearest nodes and their TCP ports
2. Establish TCP connection to the nearest nodes to perform *Block Sync* operations
