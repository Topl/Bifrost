import com.oracle.truffle.api.CompilerDirectives;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.management.ExecutionListener;

import java.io.IOException;

public class ValkyrieExecutionListener {


}


//class Main {
//    public static void main(String args[]) throws IOException {
//        String testValkyrie =
//                "this.assetCreated = {}; \n" +
//                        "this.createAssets = function(issuer, to, amount, assetCode, fee, data) { \n" +
//                        "this.issuer = issuer; \n" +
//                        "this.to = to; \n" +
//                        "this.amount = amount; \n" +
//                        "this.assetCode = assetCode; \n" +
//                        "this.fee = fee; \n" +
//                        "this.data = data; \n" +
//                        "return assetCreated; } \n" +
//                        "function create() { \n" +
//                        "a = 2 + 2; \n" +
//                        "var res = createAssets('a', 'b', 10, 'testAssets', 0, ''); }";
//
//        String test =
//                "function add(){a=2+2;}";
//
////        Context context = Context.newBuilder()
////                .allowAllAccess(true)
////
////                .build();
//        Context context = Context.create("js");
//        ExecutionListener listener = ExecutionListener.newBuilder()
//                .onEnter((e) -> {
//                    System.out.println(e.getLocation().getCharacters());
//                    if(e.getLocation().getCharacters().equals("assetCreated")) {
//                        CompilerDirectives.transferToInterpreter();
////                        throw context.createUnwind(null);
//                    }
//                })
//                .roots(true)
//                .attach(context.getEngine());
//
////        context.eval("js", test);
////        context.eval("js", "add()");
//        context.eval("js", testValkyrie);
//        System.out.println("");
//        context.eval("js", "create()");
//
//        System.out.println(context.getBindings("js").getMember("a"));
//
//    }
//}