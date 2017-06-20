//load("nashorn:mozilla_compat.js");
load("src/main/scala/bifrost/console/blob-polyfill.js");
load("src/main/scala/bifrost/console/global-polyfill.js");
load("src/main/scala/bifrost/console/timer-polyfill.js");
load("src/main/scala/bifrost/console/xml-http-request-polyfill.js");
load("src/main/scala/bifrost/console/bundle.js");

var test = function() {
    var output = "Success!";
    return output;
}

var getBlock = function() {
    print("Hello from getBlock()");
	var request = new Request('http://localhost:9086/nodeView/persistentModifier/2xrGdJTBtjZE2RWhBSzGoRwXn4gQ7DkGM5UkymJJk57y', {
        method: 'POST',
        mode: 'cors',
        redirect: 'follow',
        headers: new Headers({
            'Content-Type': 'application/json'
        })
    });

        // curl -X POST --data '{"jsonrpc":"2.0","method":"topl_getBlock","params":["2xrGdJTBtjZE2RWhBSzGoRwXn4gQ7DkGM5UkymJJk57y"],"id":0}'
	fetch(request).then(function(response) {
	    return response.json();
	}).then(function(jsonData) {
		console.log(jsonData);
		return jsonData;
	}).catch(function(err) {
	    print("Something went wrong!");
		//console.log("Something went wrong.", err);
	});

	//nashornEventLoop.process();
};

getBlock();

//module.exports BifrostConsole = BifrostConsole;