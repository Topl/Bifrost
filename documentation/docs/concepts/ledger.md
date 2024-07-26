# Ledger
The set of rules that blockchain users must follow.

## Core Responsibilities
- Verify correct syntax of transactions
- Maintain a set of "unconfirmed" transactions
- Prohibit spending non-existent tokens
- Ensure proper authorization when spending tokens
- Calculate transaction costs and rewards

### Transaction Syntax
Transactions must satisfy a basic set of syntax rules, such as transaction size, timestamp, and quantity validation.

The exact rules are defined in BramblSc and used directly in this protocol.

A notable distinction about the transactions of this protocol: SpentTransactionOutputs (transaction inputs) embed a copy of the `Value` of the UTxO to be spent. This results in increased data/storage/transfer but reduces validation costs.

### Mempool
Temporarily holds transactions until they are included in a block.  If a transaction is not included after a certain period of time, it is expired from the mempool.

All transactions are expected to undergo syntax validation prior to inclusion in the mempool.  Attempting to add to the mempool may perform additional checks (semantic and authorization) depending on the current size of the mempool.

Adding a transaction to the mempool will insert it into a graph-like structure based on the UTxOs that are spent. Future maintainers of this part of the system might consider improving the graph implementation.  Additionally, mempool and block production tend to be tightly coupled, so those systems could be merged into one.

### Box State
The term "box" is a bit overloaded in this protocol, but in this context, it essentially just means UTxO (Unspent Transaction Output).  BoxState tracks the state of UTxOs in the ledger. At "genesis", BoxState contains an initial set of UTxOs. A transaction will spend "spend" at least one of those UTxOs and create 0+ new UTxOs.  As transactions are included in blocks, the BoxState is updated accordingly.

If a transaction attempts to spend a Transaction Output that does not exist in BoxState, it is rejected.  Generally, this implies the token was already spent (Double Spend), but it could simply mean that the token never existed at all.  Under the current implementation, there's no distinction.

### Authorization
The blockchain ledger is generally publicly readable, but modifying it requires permission. Permissions are established by the users using "Locks".  A lock requires a very specific "key" in order to be unlocked (although in this case, "key" is much broader than the term used in general cryptography).  When creating a new UTxO on the chain, a Lock (or more specifically, a LockAddress) is provided.  When spending an existing UTxO from the chain, the corresponding Proof ("key") is provided.  Authorization validation ensures the Proof/key matches and is correct for the Lock.

The exact rules are defined in BramblSc and used directly in this protocol.

### Cost and Rewards
Validating and performing bookkeeping for ledger transactions may be CPU/memory-intensive.  The amount of work required to include each transaction is captured in a score known as "Cost".  This cost is determined based on transaction size and estimated verification complexity. The resulting number does not correspond to a token quantity; it is just a number.  The calculation is defined in BramblSc and used directly in the protocol.

Because of the work involved with operating a block-producing blockchain node, block producers are rewarded for their efforts.  These rewards are captured as any "excess" tokens from each transaction included in their block.  For example, if a transaction spends/consumes 50 tokens but only creates 40 tokens, there are 10 excess tokens.  The block producer can claim this excess as a reward.

At this time, there is no enforced minimum/maximum cost/reward.  In the future, the two numbers could be combined to impose minimum fees.
