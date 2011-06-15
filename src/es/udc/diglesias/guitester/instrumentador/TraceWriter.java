package es.udc.diglesias.guitester.instrumentador;

import java.io.PrintWriter;

public class TraceWriter{
    //Follows the singleton pattern

    private TraceWriter() {
        writer = new PrintWriter(System.out, true);
    }

    public void print(String s){
	writer.println(s);
	// No need to writer.flush(); because it is a 'println'
    }

    private static TraceWriter getInstance(){
	if (instance == null){
	    instance = new TraceWriter();
	}
	return instance;
    }

    public static void write(String trace){
	getInstance().print(trace);
    }

    //------- private --------
    static private TraceWriter instance;
    private PrintWriter writer;

}