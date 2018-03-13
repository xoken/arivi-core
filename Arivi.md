- [P2P Layer](#sec-1)
- [Overview of Kademlia Protocol](#sec-2)
  - [Technical details](#sec-2-1)
    - [Peer discovery](#sec-2-1-1)
    - [Assigning Node IDs](#sec-2-1-2)
    - [Distance metric](#sec-2-1-3)
    - [Bootstrapping](#sec-2-1-4)
    - [UDP based protocol for transmitting messages](#sec-2-1-5)
  - [Message Body](#sec-2-2)
    - [PING](#sec-2-2-1)
    - [PONG](#sec-2-2-2)
    - [FIND<sub>NODE</sub>](#sec-2-2-3)
    - [RETURN<sub>NODES</sub>](#sec-2-2-4)
- [Security](#sec-3)
  - [Possible Attacks](#sec-3-1)
    - [Eclipse Attack](#sec-3-1-1)
  - [100500 attack](#sec-3-2)
  - [Routing Data](#sec-3-3)
    - [Routing Tables Sharing](#sec-3-3-1)
    - [Implementation Notes](#sec-3-3-2)

# Arivi (P2P Layer)<a id="sec-1"></a>

![P2P Architecture](https://ipfs.io/ipfs/QmRGurUXkk7zFcHdB5QJhD6NR9yJ8ZfVVsrHCRLZc6xH3d)


To start communicating with other nodes, a node has to join the network. To do this, the node has to know some other node that already participates in the protocol; this node is called a bootstrap node.

After connecting to the bootstrap node, we receive a list of peers which we’ll use for network communication. Those peers are called neighbors. The list of neighbors should be maintained in such a way that these nodes are online and any node from the network can receive our messages. Moreover, messages should be delivered efficiently.

To achieve this, the Xoken blockchain uses the Kademlia DHT protocol simply as a method of peer discovery.

The P2P Layer provides a suite of methods to both Kademlia and *Block Sync* application like multiplexing, framing, chunking, encryption and messaging. The Arivi layer also handles the network layer protocols like UDP and TCP making sure that the Kademlia and *Block Sync* layer containing the business logic are agnostic of all the network protocols used.

## Flow of Node

1. Use Kademlia to get the nearest nodes and their TCP ports
2. Establish TCP connection to the nearest nodes to perform *Block Sync* operations

# Overview of Kademlia Protocol<a id="sec-2"></a>

## Technical details<a id="sec-2-1"></a>

### Peer discovery<a id="sec-2-1-1"></a>

We use Kademlia DHT for peer discovery. It is a general solution for distributed hash tables, based on [a whitepaper by Petar Maymounkov and David Mazières, 2002.](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)

### Assigning Node IDs<a id="sec-2-1-2"></a>

We are using ED25519 algorithm to generate a public key from a random seed and a secret key. The public key generated is used as the Node ID. 

### Distance metric<a id="sec-2-1-3"></a>

Kademlia uses the XOR metric to define the distance between nodes. Key-value pairs are stored in nodes with IDs that are "close" to the keys. This distance is also used to efficiently locate a node with the given ID.

### Bootstrapping<a id="sec-2-1-4"></a>

At start, a bootstrap node should be provided to Kademlia in order to join the network. The address of this node can be hardcoded in the implementation or chosen by the user. Later, the node will attempt to find more peers by querying its neighbors (from the initial list of peers sent by the bootstrap node). A node sends messages to its peers, which resend messages to their peers close to the needed ID/key. The list of known peers is preserved between launches.

### UDP based protocol for transmitting messages<a id="sec-2-1-5"></a>

![Kademlia Payload](https://ipfs.io/ipfs/Qmbmmmp15T2cxDNybdDUe4vnAVdUfAHJpL4HYxQx4rSrnh)

### Message Type - Contains either on of these

* __MSG01__
	
	Corresponds to the __PING__ operation
* __MSG02__

	Corresponds to the __PONG__ operation
* __MSG03__

	Corresponds to the __FIND NODE__ operation
* __MSG04__

	Corresponds to the __RETURN NODES__ operation



### Message Body <a id="sec-2-2"></a>

#### PING<a id="sec-2-2-1"></a>

Contains `nodeId:NodeId` and `fromEndPoint:NodeEndPoint`

#### PONG<a id="sec-2-2-2"></a>

Contains `nodeId:NodeId` and `fromEndPoint:NodeEndPoint`

#### FIND<sub>NODE</sub><a id="sec-2-2-3"></a>

Contains `nodeId:NodeId`, `targetNodeId:NodeId` and `fromEndPoint:NodeEndPoint`

#### RETURN<sub>NODES</sub><a id="sec-2-2-4"></a>

Contains `nodeId:NodeId`, `peerList:[(NodeId,NodeEndPoint)]` and `fromEndPoint:NodeEndPoint`


*Reference: `nodeId:NodeId` - `nodeId` is the attribute name, `NodeId` is the corresponding attribute.*

# Security<a id="sec-3"></a>

Since Kademlia is a protocol for open P2P networks, it had to be modified in several other ways to become reasonably secure.

## Possible Attacks<a id="sec-3-1"></a>

### Eclipse Attack<a id="sec-3-1-1"></a>

An eclipse attack is a situation when a node is surrounded by adversary nodes. In Kademlia, eclipse attacks (targeted at the particular participant of the network) are hard to perform, but possible. First, launch a hundred nodes with node IDs close to target node ID. These nodes would fill the node’s lowest k-buckets (which are expected to be empty, at a first sight), then perform a DDoS attack on nodes from target’s k-buckets (it’s possible to determine those nodes if network’s topology haven’t changed much since the node was started). After a successful DDoS attack, the node’s remaining neighbors would be adversary agents.

Please note that Kademlia’s structure implies that launching nodes close to the target is not enough to eclipse it. Node lists are stored by node in k-buckets (the i-th bucket contains no more than k nodes with relative distance 2<sup>i</sup>-1 < d < 2<sup>i</sup>), and new nodes are added to corresponding buckets only if these buckets are not already full. Kademlia prefers nodes that have been in lists for a long time and were recently seen alive. Without getting some nodes down, it’s impossible to eclipse a node.

This attack is tricky and unlikely to happen in practice. The Addressing modification makes it even harder.

## 100500 attack<a id="sec-3-2"></a>

A 100500 attack is an attack that launches significantly more nodes than the amount of nodes in the current P2P network, either in order to eclipse some nodes or to deny service by flooding the network. The attack wouldn’t cause any problems for old nodes (not counting possible network overhead), because old nodes preserve their routes. But when a new node joins the network, it would get eclipsed (isolated in an adversarial subnet), because old honest nodes won’t add it to their buckets (as these buckets are already filled by other nodes), and the new node would be known to adversaries only.

Defending against 100500 attacks remains an open problem. For now, we’re going to make them practically infeasible with a sophisticated ban system / adversary detection.

-   Ask Ankit: Addressing mode and avoiding this attack

## Routing Data<a id="sec-3-3"></a>

Anti-forging

In Kademlia, a node requests a list of peers from its neighbors and accepts the first message it receives. An adversary may forge those replies, providing addresses of adversary nodes as closest nodes to given ID. To overcome this issue, we make nodes wait for some period to gather as many replies as possible, and after that, the replies get merged and the node selects k closest nodes from the resulting set. This way, an adversary would have to eclipse a node in order to forge the list of peers it receives. Implementation Notes

To implement this idea, we just add k neighbors nodes closest to the destination at the beginning of each lookup (lookup is a function used by FIND<sub>NODE</sub> or FIND<sub>VALUE</sub> to find k nodes closest to the given ID) to the pending set. When we receive a RETURN<sub>NODES</sub> message, we update known list to make it contain k nodes currently known that are closest to the destination ID. This loop ends when no pending nodes are left. We do not introduce any specific period to collect neighbors replies. If any neighbors do not send us RETURN<sub>NODES</sub> reply, we receive Timeout signal and this neighbor is handled by waitForReply function.

See also continueLookup function. It is the place where pending and known fields are updated, so this is where the core logic of this enhancement is located.

### Routing Tables Sharing<a id="sec-3-3-1"></a>

When a node has just joined the network, it requests a list of neighbors (set of nodes closest to it). We have modified Kademlia to include some extra nodes into this list; specifically, now we pick some random nodes along with neighbors and return them. This gives the node additional knowledge to recover in case it’s surrounded with adversary nodes.

### Implementation Notes<a id="sec-3-3-2"></a>

-   Ask Ankit