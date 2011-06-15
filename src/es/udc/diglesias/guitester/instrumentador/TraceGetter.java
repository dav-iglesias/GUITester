package es.udc.diglesias.guitester.instrumentador;

import java.io.File;

public class TraceGetter {

    private static TraceHandler traceHandler = null;

    public static void addTrace(File rootDir, String mainClass, boolean positive, File tracesFile) {
        traceHandler = new TraceHandler(rootDir, mainClass, positive, tracesFile);
        traceHandler.start();
    }

    public static void endTrace(){
        try{
            traceHandler.kill();
        }catch(NullPointerException ex){
            System.out.println("No application was running");
        }
        traceHandler = null;
    }
}
