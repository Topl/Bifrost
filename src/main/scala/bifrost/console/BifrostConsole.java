/*
Important: The bundle.js file must invoke the load function for each polyfill.
Make sure these functions are present at the beginning of the bundle.js file.

load("src/main/scala/bifrost/console/blob-polyfill.js");
load("src/main/scala/bifrost/console/global-polyfill.js");
load("src/main/scala/bifrost/console/timer-polyfill.js");
load("src/main/scala/bifrost/console/xml-http-request-polyfill.js");
 */



package bifrost.console;

import jdk.nashorn.api.scripting.JSObject;
import jdk.nashorn.api.scripting.NashornScriptEngine;
import jdk.nashorn.api.scripting.NashornScriptEngineFactory;
import jdk.nashorn.api.scripting.ScriptObjectMirror;
import jdk.nashorn.internal.runtime.ScriptFunction;
import jdk.nashorn.internal.runtime.ScriptObject;

import javax.script.*;
import java.io.*;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class BifrostConsole {

    //TODO check if it even needs to be created by a factory
    private static final NashornScriptEngine nashornEngine;
    private static ScriptObjectMirror json = null;


    static {
        NashornScriptEngineFactory factory = new NashornScriptEngineFactory();
        nashornEngine = (NashornScriptEngine) factory.getScriptEngine();
    }

    private static ScheduledExecutorService globalScheduledThreadPool = Executors.newScheduledThreadPool(20);

    public static void main(String[] args) throws ScriptException, FileNotFoundException {
        ScriptContext context = nashornEngine.getContext();
        context.setBindings(nashornEngine.createBindings(), ScriptContext.ENGINE_SCOPE);
        context.setAttribute("__NASHORN_POLYFILL_TIMER__", globalScheduledThreadPool, ScriptContext.ENGINE_SCOPE);

        Bindings bindings = nashornEngine.getBindings(ScriptContext.ENGINE_SCOPE);

        try {
            nashornEngine.eval("this.stringify = function(obj, prop) {" +
                    "  var placeholder = '____PLACEHOLDER____';" +
                    "  var fns = [];" +
                    "  var json = JSON.stringify(obj, function(key, value) {" +
                    "    if (typeof value === 'function') {" +
                    "      fns.push(value);" +
                    "      return placeholder;" +
                    "    }" +
                    "    return value;" +
                    "  }, 2);" +
                    "  json = json.replace(new RegExp('\"' + placeholder + '\"', 'g'), function(_) {" +
                    "    var strSig = (fns.shift()+''); " +
                    "    return strSig.substring(0, strSig.indexOf('{')) + '{...}';" +
                    "  });" +
                    "  return '' + json + ';';" +
                    "};");
            json = (ScriptObjectMirror) nashornEngine.eval("this");

        } catch (ScriptException e) {
            e.printStackTrace();
        }

        try {
            context.setAttribute("Bifrost", nashornEngine.eval(new FileReader("src/main/scala/bifrost/console/initialize.js")), ScriptContext.ENGINE_SCOPE);
            nashornEngine.eval("var bifrost = new Bifrost()");
            nashornEngine.eval("print('This is the official console for Topl. Type help() to see a list of basic commands.')");

        } catch(ScriptException|FileNotFoundException e) {
            e.printStackTrace();
        }

        Invocable invocable = nashornEngine;

        promptForInput();
    }

    private static void promptForInput() {
        String input = "";
        Scanner scan = new Scanner(System.in);

        do {
            System.out.print("Topl > ");

            // reads in a line of input, then evaluates as javascript
            input = scan.nextLine();


            if (!input.equals("exit")) {
                try {
                    ScriptObjectMirror result = (ScriptObjectMirror) nashornEngine.eval(input);

                    try {
                        System.out.println("\n" + json.callMember("stringify", checkResult(result).get()));
                    } catch (Exception ignored) { }

                } catch (Exception e) {
                    System.err.println("There was a problem with that command.");
                }
            }

        } while (!input.equals("exit"));

    }

    private static Future<JSObject> checkResult(JSObject result) {

        CompletableFuture deferred = new CompletableFuture();

        if (result != null) {

            if (result.keySet().contains("then")) {
                System.out.println("Waiting for asynchronous call...");

                new java.util.Timer().schedule(
                        new java.util.TimerTask() {
                            @Override
                            public void run() {
                                try {
                                    nashornEngine.eval("global.nashornEventLoop.process();");

                                    JSObject checkThen = (JSObject) nashornEngine.eval("function(response) { return response; }");
                                    ScriptObjectMirror scriptResult = (ScriptObjectMirror)result;
                                    ((ScriptObjectMirror)scriptResult.get("then")).call(result, checkThen, checkThen);


                                    Integer updatedState = (Integer)scriptResult.get("_state");
                                    JSObject updatedResult = (JSObject) scriptResult.get("_result");

                                    if(updatedResult != null) {
                                        deferred.complete(checkResult(updatedResult).get());
                                    } else {
                                        Thread.sleep(250);
                                        System.out.print("...");
                                        run();
                                    }

                                } catch (Exception e) {
                                    e.printStackTrace();
                                    System.err.println("Problem processing");
                                    deferred.completeExceptionally(e);
                                }
                            }
                        },
                        250
                );
            } else {
                deferred.complete(result);
            }
        } else {
            deferred.cancel(false);
        }

        return deferred;
    }

}