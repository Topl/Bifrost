# Consensus
The set of rules that Block Producers must follow.

## Core Responsibilities
- Validate block headers
- Ensure eligibility of block producers
- Select between two chain branches

### Header Validation
A blockchain is a sequence of "blocks", where each block contains a Header and a Body. Headers contain the information required to perform consensus operations. Each Header must satisfy a list of validation rules, including but not limited to:
- The "height" must incrementally increase from the previous block
- The Block Producer must be eligible at the block's "slot"
- The "timestamp" must increase from the previous block and must correspond to the "slot"
- The block must be produced by a registered staker
- The block must include the correct "eta" value

#### Eta
Eta represents "epoch randomness".  It is a 32-byte array that is stamped on each header and changes at the turn of each epoch.

The updated value is calculated by taking the previous value and hashing it with the randomness (rho) values provided by each staker of the first 2/3 of the current epoch.

### Eligibility
This protocol uses a flavor of "proof-of-stake" known as Taktikos, which is derived from Ouroboros.  The protocol employs a separate "clock" which is made up of an infinite number of discrete chunks of time known as "slots" (i.e. "seconds"). Eligibility to produce the next block is determined on a slot-by-slot basis, although not all slots yield an eligibility. The protocol parameters are tuned in a way that aims for _someone_ to be eligible at a predictable frequency (i.e. once every 10 slots).

Block Producers must first register before they can be considered eligible.  During registration, the block producer picks a (really big) random number which adds a bit of variability to the system.  Additionally, the block producer must set aside some portion of tokens to be used for staking.  The number of tokens set aside, relative to the rest of the block producers, scales the chances of eligibility. By setting aside more staking tokens, the block producer will be eligible more frequently.

In this protocol, a block producer references a single UTxO containing both registration data and staking tokens.  When registering (creating the UTxO), the staker is not immediately eligible, thus preventing instability and fluctuation of the protocol. The staker must wait until the completion of the current epoch plus one additional epoch before becoming eligible.  As such, the consensus mechanism uses a separate stake tracking mechanism which tracks epoch boundaries and registrations.  

### Chain Selection
Not all slots are occupied by a block.  In most cases, the slot will be empty because no block producers were eligible at the specific time.  But on some rare occasions, _multiple_ block producers may be eligible for a given slot. While both may be eligible, only one can be accepted.  Choosing between two eligible blocks is known as "chain selection".

The two types of rulesets are:
- **Standard Order**: The primary set of rules to apply to short branches/forks.
  - The longest chain is selected (greatest height value)
  - If height is equal, the tip block with the lowest slot is selected
  - If the slot is equal, the tip block with the lowest BigInt representation of rhoNonceHash is selected
- **Density Order**: The backup set of rules to apply for long forks.
  - If the branch/fork point is more than `kLookback` number of blocks from the head of either chain, employ Density Order
  - Find the common ancestor point between the two branches
  - Count the number of blocks on each branch within `sWindow` number of slots
  - The chain with more blocks (thus greater density) is selected