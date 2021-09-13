package co.topl.stakeholder.primitives;

import java.lang.management.ManagementFactory;
import com.sun.management.OperatingSystemMXBean;

/**
 *  AMS 2020:
 *  Simple monitor to limit activity when system is under load
 *  used in local simulations only
 */

public class SystemLoadMonitor {
    OperatingSystemMXBean bean = (OperatingSystemMXBean) ManagementFactory
            .getOperatingSystemMXBean();
    public double cpuLoad() {
        double value = bean.getSystemCpuLoad();
        return value;
    }
}
