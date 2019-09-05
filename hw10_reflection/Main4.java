import java.util.*;
import java.lang.Class;
import java.lang.reflect.*;

// ******************************************************************************************************* Myclass
class Myclass {

	public static boolean test1(){return true;}
	public static boolean test2(){return false;}
	public static boolean testTAMU(boolean tamu){return tamu;}
	public boolean testCSE(){return false;}
	public static int testAggie(){return 2017;}
	public static boolean fakeTestAggie(){return false;}
}

// ********************************************************************************************************* Main4
public class Main4 {
    
	public static void reflection(String className) {
        try {
            Class testClass = Class.forName(className);
            
            Field fields[] = testClass.getDeclaredFields();
            for(Field f : fields) {
                if(Modifier.isPublic(f.getModifiers())) {
                    System.out.println(f);
                }
            }
            
            Method[] methods = testClass.getDeclaredMethods();
            for(Method m : methods) {
                if(m.getParameterTypes().length == 0 && m.getReturnType().getName().startsWith("bool") &&  m.getName().startsWith("test") && Modifier.isStatic(m.getModifiers())) {
                    Object o = m.invoke(null);
                    boolean b = (boolean)o;
                    if(b == true) {
                        System.out.println(m.getName() + " succeeded");
                    }
                    else {
                        System.out.println(m.getName() + " failed");
                    }
                }
            }
            
        } catch(Throwable e) {
            System.err.println(e);
        }
	}
	
	public static void main(String args[]) {
		
		String className = args[0];
        reflection(className);
		
	}
}
/*
    public static void main(String args[]){
        
        String className = "Myclass";
        
        try {
            Class c = Class.forName(className);
            System.out.println(c + " {");
            
            int mods;
            Field fields[] = c.getDeclaredFields();

            for (Field f : fields) {
                if (!Modifier.isPrivate(f.getModifiers()) && !Modifier.isProtected(f.getModifiers())) System.out.println("\t" + f);
            }
        
            Constructor[] constructors = c.getConstructors();

            for (Constructor con : constructors) { System.out.println("\t" + con); }
                Method methods[] = c.getDeclaredMethods();
                for (Method m : methods) {
                    if (!Modifier.isPrivate(m.getModifiers())) { System.out.println("\t" + m);
                    }
                }

            System.out.println("}");
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }
}*/


/************************************************************************************************* Notes/Questions
*
* On line 31, if "Modifier.isStatic(m.getModifiers())" case is removed, howcome when this program is ran, it throws and error "java.lang.NullPointerException". The only non static method is "public boolean testCSE(){return false;}"
*/