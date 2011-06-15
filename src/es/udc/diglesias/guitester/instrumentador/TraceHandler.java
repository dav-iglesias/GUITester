package es.udc.diglesias.guitester.instrumentador;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class TraceHandler extends Thread {

    private static String JAVA_COMMAND = "java";
    private final File rootDir;
    private final String mainClass;
    private final boolean positive;
    private final File tracesFile;
    private Process process;
    private BufferedReader input;

    public TraceHandler(File rootDir, String mainClass, boolean positive, File tracesFile) {
        this.rootDir = rootDir;
        this.mainClass = mainClass;
        this.positive = positive;
        this.tracesFile = tracesFile;
    }

    private static String getJavaOrderString(File rootDir, String mainClass) throws IOException {
        String classpathOption = "-cp " + rootDir.getCanonicalPath()
                + System.getProperty("path.separator") + System.getProperty("java.class.path");
        return JAVA_COMMAND + " " + classpathOption + " " + mainClass;
    }

    @Override
    public void run() {
        try {
            // First 'true' to append, second to autoflush
            PrintWriter writer = new PrintWriter(new FileOutputStream(tracesFile, true), true);
            writer.println(positive ? "+" : "-");
            String line;
            process = Runtime.getRuntime().exec(getJavaOrderString(rootDir, mainClass));
            input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            while ((line = input.readLine()) != null) {
                writer.println(line);
            }
            //Final blank line
            writer.println();
            input.close();
        } catch (FileNotFoundException ex) {
            System.out.println(ex.getMessage());
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        }
    }

    public void kill() {
        process.destroy();
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new FileOutputStream(tracesFile, true), true);
            writer.println();
            input.close();
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        } finally {
            writer.close();
        }
    }
}
