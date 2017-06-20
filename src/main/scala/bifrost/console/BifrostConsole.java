package bifrost.console;

import jdk.nashorn.api.scripting.NashornScriptEngine;
import jdk.nashorn.api.scripting.NashornScriptEngineFactory;
import jdk.nashorn.api.scripting.ScriptObjectMirror;
import scala.collection.script.Script;

import javax.script.*;
import java.io.*;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class BifrostConsole {

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

        /* Sets standard context so we can have all the globals in the same place */

       // Compilable compilableEngine = (Compilable) testEngine;
       // CompiledScript compiled = compilableEngine.compile("src/main/scala/bifrost/console/BifrostConsole.js");


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

        //System.out.println("Engine " + testEngine.getBindings(ScriptContext.ENGINE_SCOPE).);
        //System.out.println("Global " + testEngine.getBindings(ScriptContext.GLOBAL_SCOPE).toString());

        Invocable invocable = (Invocable) nashornEngine;

        Scanner scan = new Scanner(System.in);
        String input = "";
        do {
            System.out.print("Topl > ");
            input = scan.nextLine();
            try {
                ScriptObjectMirror result = ((ScriptObjectMirror)nashornEngine.eval(input));
                for (Map.Entry<String, Object> entry : result.entrySet()) {
                    System.out.println(entry.getKey() + ", " + entry.getValue());
                }
            } catch(ScriptException e) {
               e.printStackTrace();
            } /*catch (NoSuchMethodException e) {
                e.printStackTrace();
            }*/
        } while(!input.equals("exit"));
    }

}