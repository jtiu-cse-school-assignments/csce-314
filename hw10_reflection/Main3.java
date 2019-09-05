import java.util.*;
import java.lang.Class;
import java.lang.reflect.*;

// ************************************************************************************************************* A
class A {
    void foo(int T1, int T2) {  }
    int bar(int T1, int T2, int T3) { return 0; }
    static double doo() { return 0.0; }
}

// ********************************************************************************************************* Main3
class Main3 {
    static void displayMethodInfo(Object obj) {
        try {
            
            Class c = obj.getClass();
            
            Method methods[] = c.getDeclaredMethods();
            
            for(Method m : methods) {
                
                Class types[] = m.getParameterTypes();
                
                System.out.print(m.getName() + " (");
                
                for(int i = 0; i < types.length; i++) {
                    if(i == types.length-1) {
                        System.out.print(types[i]);
                    }
                    else {
                        System.out.print(types[i] + ", ");
                    }
                }
                
                System.out.println(") -> " + m.getReturnType().getName());
            }
        } catch(Throwable e) {
            System.err.println(e);
        }
    }
    
    public static void main(String args[]) {
        
        Object obj = new A();
        displayMethodInfo(obj);
    }
}