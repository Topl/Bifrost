# P2P
Node-to-node communication is implemented using a custom protocol on top of raw TCP sockets. After some initial handshakes, the remainder of the communcation is performed over a multiplexer with dedicated channels for specific types of communication.

The handshakes consist of:
- Exchange a "Peer Identity" (a verification key)
- Exchange network protocol version

Assuming the latest network protocol version, a multiplexer is created with ports dedicated to, but not limited to:
- The exchange of blocks
- The exchange of transactions
- The notification of block and transaction adoptions
- The exchange of P2P participant information

Each of these channels operate on a request-response model. Request messages are prefixed with a `0` while response messages are prefixed with a `1`. The entire message is framed together with its port number and sent over a shared TCP socket.

Using the combination of multiplexer channels, a `BlockchainPeerClient` is exposed providing higher-level interface to information from the remote peer. The `BlockchainPeerClient` is provided to the actor-based blockchain network framework.