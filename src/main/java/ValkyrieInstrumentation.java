import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;
import com.oracle.truffle.js.nodes.instrumentation.JSTags;
import org.graalvm.polyglot.*;

import java.io.IOException;

//@TruffleInstrument.Registration(id = "Valkyrie", services = ValkyrieInstrumentation.class)
//public final class ValkyrieInstrumentation extends TruffleInstrument {
//
//    @Override
//    protected void onCreate(TruffleInstrument.Env env) {
//        System.out.println("Entered");
//        SourceSectionFilter.Builder builder = SourceSectionFilter.newBuilder();
////        SourceSectionFilter filter = builder.tagIs(StandardTags.CallTag.class).build();
//        SourceSectionFilter filter = builder.tagIs(StandardTags.StatementTag.class).build();
//
////        SourceSectionFilter filter = builder
////                .tagIs(StandardTags.class)
////                .mimeTypeIs("x-application/js")
////                .build();
//
//
//        Instrumenter instrumenter = env.getInstrumenter();
//        instrumenter.attachExecutionEventListener(filter, new ExecutionEventListener() {
//            @Override
//            public void onEnter(EventContext context, VirtualFrame frame) {
//                CompilerDirectives.transferToInterpreter();
//                // notify the runtime that we will change the current execution flow
//                throw context.createUnwind(null);
////                System.out.println("Entered onEnter for ExecutionEventListener");
//            }
//
//            @Override
//            public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
//                CompilerDirectives.transferToInterpreter();
//                // notify the runtime that we will change the current execution flow
//                throw context.createUnwind(null);
////                System.out.println("Entered onReturnValue for ExecutionEventListener");
//
//            }
//
//            @Override
//            public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
//                System.out.println("Entered onReturnExceptional for ExecutionEventListener");
//
//            }
//
//            @Override
//            public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
//                return 42;
//            }
//        });
//
//        env.registerService(this);
//        env.getInstruments().get("Valkyrie");
//    }
//
////    public void run(Source source, Context context) {
////        System.out.println(context.getEngine().getInstruments().get("Valkyrie"));
////
////    }
//}

class Main {
    public static void main(String args[]) throws IOException {
        String testValkyrie =
                "this.assetCreated = {}; \n" +
                        "this.createAssets = function(issuer, to, amount, assetCode, fee, data) { \n" +
                        "this.issuer = issuer; \n" +
                        "this.to = to; \n" +
                        "this.amount = amount; \n" +
                        "this.assetCode = assetCode; \n" +
                        "this.fee = fee; \n" +
                        "this.data = data; \n" +
                        "return assetCreated; } \n" +
                        "function create() { \n" +
                        "a = 2 + 2; \n" +
                        "var res = createAssets('a', 'b', 10, 'testAssets', 0, ''); }";

        String test =
                "function add(){a=2+2;}" ;

//        ValkyrieInstrumentation valk = new ValkyrieInstrumentation();
//        Context context = Context.newBuilder()
//                .allowAllAccess(true)
//
//                .build();
        System.out.println("Test 1 -------------------");
        Context context = Context.create("js");
        System.out.println(context.getEngine().getInstruments().get("Valkyrie"));
        Object valk = context.getEngine().getInstruments().get("Valkyrie").lookup(Object.class);
        System.out.println(valk);
//        context.getBindings("js").putMember("Valkyrie", ValkyrieInstrumentation.class);
        context.eval("js", testValkyrie);
        context.eval("js", "create()");
        System.out.println(context.getBindings("js").getMember("res"));
//        context.eval(Source.create("js", testValkyrie));
//        valk.run(Source.create("js", testValkyrie), context);



        System.out.println("Test 2 --------------------");
        String testUniversalInstrument = "function inc(x) {\n" +
                "  return x + 1\n" +
                "}\n" +
                "\n" +
                "var a = 10\n" +
                "var b = a;\n" +
                "// Let's call inc() with normal semantics\n" +
                "while (a == b && a < 100000) {\n" +
                "  a = inc(a);\n" +
                "  b = b + 1;\n" +
                "}\n" +
                "c = a;\n" +
                "// Run inc() and alter it's return type using the instrument\n" +
                " function blah() { return inc(c)}";

        Context con = Context.create("js");
        System.out.println(con.getEngine().getInstruments().get("UniversalAnswer"));
        con.getEngine().getInstruments().get("UniversalAnswer").lookup(Object.class);
        Value v = con.eval("js", testUniversalInstrument);
        Value v2 = con.eval("js", "blah()");
        System.out.println(v);
        System.out.println(v2);
        System.out.println(con.getBindings("js").getMember("blah"));
        System.out.println(con.getBindings("js").getMemberKeys());





    }

}

