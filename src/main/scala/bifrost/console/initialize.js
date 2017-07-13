function fromClassPath(file) {
    return Java.type("bifrost.console.BifrostConsole").class.getResource(file);
}

load(fromClassPath("global-polyfill.js"));
load(fromClassPath("timer-polyfill.js"));
load(fromClassPath("xml-http-request-polyfill.js"));
load(fromClassPath("bundle.js"));

//global variable to represent an agreement JSON template
var agreement = {"terms": {"share": {}, "fulfilment": {}}};

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
   createKeyFile(password)                        generates a new key file using a given password string \n\
   newAgreement()                                 creates a new agreement object with user input values to set \n\
                                                  the terms for a new agreement. The resulting agreement terms \n\
                                                  can be seen by entering 'agreement' into the console to call \n\
                                                  the agreement global variable. \n\n\n\
   There are a few keywords for the console as well: \n\n\
   exit                                           terminates Bifrost\n\
   [show, hide] logs                              enables or disables logging from background activities\n\
   help                                           displays this help section\n\n\n\
   The full API can be accessed via localhost:9585. Read the documentation at https://github.com/Topl/Project-Bifrost/wiki/HTTP-API\n\
   ");
 };

var newAgreement = function() {
  var scanner = Java.type("bifrost.console.BifrostConsole");
  var input;
  var inputArray = [];




  print("pledge:");
  input = scanner.jsScan();
  agreement.terms.pledge = input;
  print("xrate:");
  input = scanner.jsScan();
  agreement.terms.xrate = input;

  print("share:");
  print("function type:");
  input = scanner.jsScan();
  agreement.terms.share.functionType = input;
  print("leave blank to finish entering points\npoints:");
  input = scanner.jsScan();
  while(input !== "") {
    inputArray.push(input);
    input = scanner.jsScan();
  }
  agreement.terms.share.points = inputArray;
  inputArray = [];

  print("fulfilment:");
  print("function type:");
  input = scanner.jsScan();
  agreement.terms.fulfilment.functionType = input;
  print("leave blank to finish entering points\npoints:");
  input = scanner.jsScan();
  while(input !== "") {
    inputArray.push(input);
    input = scanner.jsScan();
  }
  agreement.terms.fulfilment.points = inputArray;

  print("asset code:");
  input = scanner.jsScan();
  agreement.assetCode = input;

  var effectiveDate, expirationDate;
  while(input !== "correct!") {
    print("Contract dates are in the format - Month Day Year Hour:Minute:Second GMT+0000");
    print("Example: 'Mar 25 2015 12:34:56 GMT+0100");
    print("The dates will be converted to the standard Unix epoch timestamp in the resulting JSON file");
    print("contract effective time:");
    input = scanner.jsScan();
    effectiveDate = Math.floor((new Date(input).getTime())/1000);
    print("contract effective time:");
    input = scanner.jsScan();
    expirationDate =  Math.floor((new Date(input).getTime())/1000);
    if(isNaN(effectiveDate) || isNaN(expirationDate)) {
      print("Error: The date is not in the correct format");
    }
    if(expirationDate > effectiveDate) {
      input = "correct!";
    } else {
      print("Error: The expiration date is before the effective date");
    }
  }
  agreement.contractEffectiveTime = effectiveDate;
  agreement.contractExpirationTime = expirationDate;

  print(JSON.stringify(agreement, undefined, 2));
};