package bifrost.console;

import jdk.nashorn.api.scripting.NashornScriptEngine;
import jdk.nashorn.api.scripting.NashornScriptEngineFactory;

import javax.script.*;
import java.io.*;
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

    BifrostConsole() {
        //initNashornEngine();
    }

   /* synchronized private void initNashornEngine() {
        ScriptContext context = new SimpleScriptContext();

        context.setBindings(nashornEngine.createBindings(), ScriptContext.ENGINE_SCOPE);
        context.setAttribute("__NASHORN_POLYFILL_TIMER__", globalScheduledThreadPool, ScriptContext.ENGINE_SCOPE);

        Bindings initialBindings = context.getBindings(ScriptContext.ENGINE_SCOPE);

    }*/

    public ScriptEngine getEngine() {
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("nashorn");
        return engine;
    }

    public ScriptContext getContext(ScriptEngine engine) {
        ScriptContext context = new SimpleScriptContext();
        Bindings bindings = engine.createBindings();
        context.setBindings(bindings, ScriptContext.ENGINE_SCOPE);
        context.setBindings(nashornEngine.createBindings(), ScriptContext.ENGINE_SCOPE);
        context.setAttribute("__NASHORN_POLYFILL_TIMER__", globalScheduledThreadPool, ScriptContext.ENGINE_SCOPE);

        Bindings initialBindings = context.getBindings(ScriptContext.ENGINE_SCOPE);

        return context;
    }

    public static void main(String[] args) throws ScriptException, FileNotFoundException {
        BifrostConsole bifrostConsole = new BifrostConsole();
        ScriptEngine testEngine = bifrostConsole.getEngine();
        ScriptContext testContext = bifrostConsole.getContext(testEngine);
        testEngine.setContext(testContext);

       // Compilable compilableEngine = (Compilable) testEngine;
       // CompiledScript compiled = compilableEngine.compile("src/main/scala/bifrost/console/BifrostConsole.js");

        Bindings bindings = testEngine.getBindings(ScriptContext.ENGINE_SCOPE);

        for(Map.Entry me : bindings.entrySet()) {
            System.out.printf("%s: %s\n",me.getKey(),String.valueOf(me.getValue()));
        }

        try {
            testEngine.eval(new FileReader("src/main/scala/bifrost/console/bundle.js"));
        } catch(ScriptException|FileNotFoundException e) {
            e.printStackTrace();
        }

        Invocable invocable = (Invocable) testEngine;

        Scanner scan = new Scanner(System.in);
        String input = "";
        do {
            System.out.print("Topl: ");
            input = scan.nextLine();
            try {
                Map<String, Object> result = (Map<String, Object>) testEngine.eval(input);
                System.out.println(result);
            } catch(ScriptException e) {
                throw new RuntimeException(e);
            } /*catch (NoSuchMethodException e) {
                e.printStackTrace();
            }*/
        } while(!input.equals("exit"));
    }

}