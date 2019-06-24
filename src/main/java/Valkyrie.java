import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.*;

@TruffleInstrument.Registration(id = "Valkyrie", services = Valkyrie.class)
public final class Valkyrie extends TruffleInstrument {

    @Override
    protected void onCreate(Env env) {
        System.out.println("Entered");
        SourceSectionFilter.Builder builder = SourceSectionFilter.newBuilder();
//        SourceSectionFilter filter = builder.tagIs(StandardTags.CallTag.class).build();
        SourceSectionFilter filter = builder.tagIs(StandardTags.StatementTag.class).build();

//        SourceSectionFilter filter = builder
//                .tagIs(StandardTags.class)
//                .mimeTypeIs("x-application/js")
//                .build();


        Instrumenter instrumenter = env.getInstrumenter();
        instrumenter.attachExecutionEventListener(filter, new ExecutionEventListener() {
           // @Override
            public void onEnter(EventContext context, VirtualFrame frame) {
                System.out.println("Entered onEnter in ExecutionEventListener");
                CompilerDirectives.transferToInterpreter();
                // notify the runtime that we will change the current execution flow
                throw context.createUnwind(null);
//                System.out.println("Entered onEnter for ExecutionEventListener");
            }

           // @Override
            public void onReturnValue(EventContext context, VirtualFrame frame, Object result) {
                System.out.println("Entered onReturnValue in ExecutionEventListener");
                CompilerDirectives.transferToInterpreter();
                // notify the runtime that we will change the current execution flow
                throw context.createUnwind(null);
//                System.out.println("Entered onReturnValue for ExecutionEventListener");

            }

            //@Override
            public void onReturnExceptional(EventContext context, VirtualFrame frame, Throwable exception) {
                System.out.println("Entered onReturnExceptional for ExecutionEventListener");

            }

            //@Override
            public Object onUnwind(EventContext context, VirtualFrame frame, Object info) {
                return 42;
            }
        });

        env.registerService(Valkyrie.class);
    }
}

