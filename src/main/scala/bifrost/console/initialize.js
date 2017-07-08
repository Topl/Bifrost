function fromClassPath(file) {
    return Java.type("bifrost.console.BifrostConsole").class.getResource(file);
}

load(fromClassPath("global-polyfill.js"));
load(fromClassPath("timer-polyfill.js"));
load(fromClassPath("xml-http-request-polyfill.js"));
load(fromClassPath("bundle.js"));

 var help = function() {
   print("\n   Welcome to the Bifrost client console for the Topl blockchain. This is a list of basic commands to use with the console. \n\
   Many commands can be run by calling the preset global bifrost object the the function. \n\n\
   Example: bifrost.getBlock('7rZk9srzypcsuDRKmww5m9qcE62mhrCb839MoAdmvDU') \n\n\
   Every parameter must be encapsulated in double or single quotes as this is a requirement of the scripting engine. \n\n\
   getBlock(hash/id)                              returns the block associated from a given hash or ID \n\
   getTransaction(hash/id)                        returns a transaction from a given hash or ID \n\
   declareRole(publicKey, role)                   sets the rolehub, producer, investor for a particular public key \n\
   getRole(publicKey)                             returns the role associated with a given public Key \n\
   getContractSignature(publicKey)                returns the signature used to sign a contract from the given public key \n\
                                                    provided by one of the roles \n\
   createContract(contractParameters)             validates and creates a new contract from a given contract JSON template \n\
   deliver(deliverParameters)                     sends quantity of assets from the producer role to the given contractBox \n\
                                                   specified in the deliver JSON template \n\
   confirmDelivery(confirmParameters)             a hub public key is provided to validate that the delivery sent by the \n\
                                                   producer is correct \n\
   endorseCompletion(endorseParameters)           sign a contract to signify completion by each party in the contract \n\
   getCompletionSignature(comSigParameters)       returns the signing key from a given public key associated \n\
                                                    with a contractBox \n\
   completeContract(completeParameters)           uses a given JSON template to close a completed contract \n\
   createKeyFile(password)                        generates a new key file using a given password string \n\n\n\
   There are a few keywords for the console as well: \n\n\
   exit                                           terminates Bifrost\n\
   [show, hide] logs                              enables or disables logging from background activities\n\
   help                                           displays this help section\n\n\n\
   The full API can be accessed via localhost:9585. Read the documentation at https://github.com/Topl/Project-Bifrost/wiki/HTTP-API\n\
   ");
 };