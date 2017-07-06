function fromClassPath(file) {
    return Java.type("bifrost.console.BifrostConsole").class.getResource(file);
}

load(fromClassPath("global-polyfill.js"));
load(fromClassPath("timer-polyfill.js"));
load(fromClassPath("xml-http-request-polyfill.js"));
load(fromClassPath("bundle.js"));


var help = function() {
    //TODO Complete basic help command
    print("")
}