package co.topl.node;

import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.c.function.CEntryPoint;

public class GraalLib {
    @CEntryPoint(name = "run_node")
    public static void runNode(IsolateThread thread) {
        try {
            NodeApp.main(new String[0]);
        } catch (Exception e) {
            System.err.println("Node failed. reason=" + e.toString());
        }
    }
}
