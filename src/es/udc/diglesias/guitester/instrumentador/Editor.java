package es.udc.diglesias.guitester.instrumentador;

import es.udc.diglesias.guitester.util.FilesystemOperations;
import javassist.*;
import java.io.IOException;
import java.io.File;

public class Editor{

    private static void modifyMethod (String methodName, String name, CtClass c){
	try{
	    CtMethod m = c.getDeclaredMethod(methodName);
	    m.insertBefore("es.udc.diglesias.guitester.instrumentador.TraceWriter.write(\""+name+";;"+methodName+" \");");
	}catch(NotFoundException e){
	    //System.out.println("in "+name+" ->no "+methodName);
	}catch(CannotCompileException e){
	    System.out.println(e.getMessage());
	}
    }

    private static void visitClass (String name, String simpleName, ClassPool p, String rootParent){
	if (name.equals("com.ehsunbehravesh.mypasswords.gui.MainFrame")) return; //Bug raro
	try{
	    CtClass c = p.get(name);
	    String methodNames[] = {"actionPerformed", "mouseClicked", "focusLost", "keyPressed", "keyReleased", "mousePressed", "mouseMoved", "focusGained", "windowClosing"};
	    for (String methodName : methodNames){
		modifyMethod(methodName, simpleName, c);
	    }
	    c.writeFile(rootParent);
	    c.detach(); //Avoid out of memory (http://tinyurl.com/624usrb)
	}catch(NotFoundException e){
	    System.out.println(e.getMessage());
            e.printStackTrace();
	}catch(CannotCompileException e){
	    System.out.println(e.getMessage());
            e.printStackTrace();
	}catch(IOException e){
	    System.out.println(e.getMessage());
            e.printStackTrace();
	}
    }

    // packageName includes final '.', e.g., "java.util."
    private static void visitDirectory(File dir, String packageName, ClassPool pool, String rootParent) throws Exception{
	File[] files = dir.listFiles();
	if (files != null){
	    for(File f: files){
		if (f.isDirectory()){
		    String newPackageName = packageName + f.getName() + ".";
		    visitDirectory(f, newPackageName, pool, rootParent);
		}else{
		    String name = f.getName();
		    if (name.endsWith(".class")){
			String simpleName = name.substring(0, name.length()-6); //Take ".class" out
			String className = packageName + simpleName;
			visitClass(className, simpleName, pool, rootParent);
		    }
		}
	    }
	}else{
	    throw new Exception("No se pueden listar archivos");
	}
    }

    private static void edit(File root){
        ClassPool pool = ClassPool.getDefault();
        try {
            pool.insertClassPath(root.getParent());
        } catch (NotFoundException ex) {
            System.out.println(ex.getMessage());
        }
	    try {
		visitDirectory(root, root.getName()+".", pool, root.getParent());
	    } catch (Exception e){
		System.out.println(e.getMessage());
	    }
    }




    private static File createDstFolder(File sourceLocation, File targetLocation){
        File dstLocation = new File(targetLocation.getAbsolutePath() + System.getProperty("file.separator") + sourceLocation.getName());
        dstLocation.mkdirs();
        return dstLocation;
    }

    public static void main(String srcFolder, String tmpFolder){
        try {
            File srcFolderFile = new File(srcFolder);
            File tmpFolderFile = new File(tmpFolder);
            System.out.println("Copying files to a temporary folder...");
            File dstFolderFile = createDstFolder(srcFolderFile, tmpFolderFile);
            FilesystemOperations.copyDirectory(srcFolderFile, dstFolderFile);
            System.out.println("Editing bytecodes...");
            edit(dstFolderFile);
        } catch (IOException ex) {
            System.out.println(ex.getMessage());
        }
    }
    
    public static void main(String[] args) {
	if (args.length != 1){
	    System.out.println("Use: java Editor <root_package_path> <tmp_folder_path>");
	    System.out.println("  example: java Editor com");
	}else{
            main (args[0], args[1]);
            
	}
    }

}
