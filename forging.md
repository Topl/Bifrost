``` mermaid
graph TD
    A{NodeView.restoreState}
    A --> |true| B[checkPrivateForging]
    A --> |false| C[getGenesisBlock]
    C --> D[checkPrivateForging]
    C --> E
    B --> E[generateInitialKeysIfNotEmpty]
    D --> E
```

# Startup Diagrams

The following are some startup scenarios and how the actors within Bifrost interact.

## Private Network

### No Existing State And Rewards Address In Config

``` mermaid
sequenceDiagram
    participant app as BifrostApp
    participant nvh as NodeViewHolder
    participant f as Forger
    participant k as KeyManager
    participant e as Event Stream

    app ->> k: create
    Note right of k: initialize with key file and rewards address from config
    k -->> app: return actor reference

    app ->> f: create
    Note right of f: initialize with settings, context, and key manager actor
    f -->> app: return actor reference
    f ->> e: subscribe to NodeViewReady

    app ->> nvh: create
    Note right of nvh: try to restore state, but no state exists
    
    nvh ->> f: generate genesis block
    f ->> k: generate initial keys

    Note right of k: create addresses in key ring
    k -->> f: return generated addresses

    Note right of f: create genesis block with generated addresses

    f -->> nvh: return genesis block
    Note right of nvh: initialize state with genesis block
    nvh ->> e: publish NodeViewReady message

    e ->> f: notify of NodeViewReady message
    f ->> k: generate initial keys

    Note right of k: keys already generated

    k -->> f: return existing keys
    f -->> f: send start forging signal
```

### Existing State And Rewards Address In Config

``` mermaid
sequenceDiagram
    participant app as BifrostApp
    participant nvh as NodeViewHolder
    participant f as Forger
    participant k as KeyManager
    participant e as Event Stream

    app ->> k: create
    Note right of k: initialize with key file and rewards address from config
    k -->> app: return actor reference

    app ->> f: create
    Note right of f: initialize with settings, context, and key manager actor
    f -->> app: return actor reference
    f ->> e: subscribe to NodeViewReady

    app ->> nvh: create
    Note right of nvh: restore state that contains a genesis block
    
    nvh ->> e: publish NodeViewReady message

    e ->> f: notify of NodeViewReady message
    f ->> k: generate initial keys

    Note right of k: create addresses in key ring

    k -->> f: return generated addresses
    f -->> f: send start forging signal
```
