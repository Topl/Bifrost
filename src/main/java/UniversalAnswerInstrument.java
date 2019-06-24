import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;

@TruffleInstrument.Registration(id = "UniversalAnswer", services = UniversalAnswerInstrument.class)
public class UniversalAnswerInstrument extends TruffleInstrument {

//    ExecutionEventListener myListener = new ExecutionEventListener() {
//
//        public void onEnter(EventContext context, VirtualFrame frame) {
//            System.out.println("Entered onEnter in ExecutionEventListener");
//            CompilerDirectives.transferToInterpreter();
//            // notify the runtime that we will change the current execution flow
//            throw context.createUnwind(null);
////                System.out.println("Entered onEnter for ExecutionEventListener");
//        }
//
//        //@Override
//        public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
//            String callSrc = context.getInstrumentedSourceSection().getCharacters().toString();
//            // is this the function call that we want to modify?
//            if ("inc(c)".equals(callSrc)) {
//                CompilerDirectives.transferToInterpreter();
//                // notify the runtime that we will change the current execution flow
//                throw context.createUnwind(null);
//            }
//        }
//
//        //@Override
//        public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
//            // just return 42 as the return value for this node
//            return 42;
//        }
//    };
    @Override
    protected void onCreate(Env env) {
        env.registerService(this);
        env.getInstrumenter().attachExecutionEventListener(SourceSectionFilter.newBuilder().tagIs(StandardTags.CallTag.class).build(),
                new ExecutionEventListener() {

                    public void onEnter(EventContext context, VirtualFrame frame) {
                        System.out.println("Entered onEnter in ExecutionEventListener");
                        CompilerDirectives.transferToInterpreter();
                        // notify the runtime that we will change the current execution flow
                        throw context.createUnwind(null);
//                System.out.println("Entered onEnter for ExecutionEventListener");
                    }

                    //@Override
                    public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
                        String callSrc = context.getInstrumentedSourceSection().getCharacters().toString();
                        // is this the function call that we want to modify?
                        if ("inc(c)".equals(callSrc)) {
                            CompilerDirectives.transferToInterpreter();
                            // notify the runtime that we will change the current execution flow
                            throw context.createUnwind(null);
                        }
                    }

                    public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
                        System.out.println("Entered onReturnExceptional for ExecutionEventListener");

                    }

                    //@Override
                    public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
                        // just return 42 as the return value for this node
                        return 42;
                    }
                });
    }
}

