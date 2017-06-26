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

import javax.script.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class BifrostConsole {

    //TODO check if it even needs to be created by a factory
    private static final NashornScriptEngine nashornEngine;

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

        for(Map.Entry me : bindings.entrySet()) {
            System.out.printf("%s: %s\n",me.getKey(),String.valueOf(me.getValue()));
        }

        try {
            context.setAttribute("Bifrost", nashornEngine.eval(new FileReader("src/main/scala/bifrost/console/bundle.js")), ScriptContext.ENGINE_SCOPE);

            bindings = nashornEngine.getBindings(ScriptContext.ENGINE_SCOPE);

           for(Map.Entry me : bindings.entrySet()) {
                System.out.printf("%s: %s\n",me.getKey(),String.valueOf(me.getValue()));
           }
        } catch(ScriptException|FileNotFoundException e) {
            e.printStackTrace();
        }

        Invocable invocable = nashornEngine;

        Scanner scan = new Scanner(System.in);
        String input = "";
        do {
            System.out.print("\nTopl > ");

            // reads in a line of input, then evaluates as javascript
            input = scan.nextLine();
            try {
                Object result = nashornEngine.eval(input);

                int i = 0;

                if (result == null) {
                    nashornEngine.eval("global.nashornEventLoop.process();");
                }

            } catch(ScriptException e) {
               e.printStackTrace();
            }
        } while(!input.equals("exit"));
    }

    //var b = new Bifrost()
    //var test = b.getBlock()
    //var thisWorks = test.then(function(resolve, reject) { console.log(JSON.stringify(resolve)); console.log("In 'thisworks'"); });
}