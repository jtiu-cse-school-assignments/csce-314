import java.util.*;
import java.util.concurrent.locks.ReentrantLock;

class TimerPrinter implements Runnable {
    private final int DELAY = 1000;
    protected int counter;
    
    public TimerPrinter() {
        counter = 0;
    }
    
    public void addMessageTimer(int printMessageAt)
    {
        new Thread(new Runnable()
                   {
            public void run()
            {
                printMessage(printMessageAt);
            }
        }).start();
    }
    
    public synchronized void increment() {
        counter++;
        notifyAll();
    }
    
    private synchronized void printMessage(int printMessageAt)
    {
        try
        {
            for(;;)
            {
                wait();
                if ((counter % printMessageAt) == 0)
                {
                    System.out.println();
                    System.out.println(printMessageAt + " second message");
                }
            }
        }
        catch(InterruptedException e)
        {
            e.printStackTrace();
        }
    }
    
    public void run() {
        try {
            for(;;) {
                increment();
                System.out.print(counter + " ");
                Thread.sleep(DELAY);
            }
        } catch(Throwable e) {
            System.err.println(e);
        }
    }
}

class Main2 {
    public static void main(String args[]) {
        TimerPrinter pt = new TimerPrinter();
        pt.addMessageTimer(7);
        pt.addMessageTimer(15);
        new Thread(pt).start();
    }
}






/*
class PrintTimer implements Runnable
{
    // 1 second
    private final int DELAY = 1000;
    protected int counter;

    PrintTimer()
    {   
        counter = 0;
    }   

    public void addMessageTimer(int printMessageAt)
    {   
        new Thread(new Runnable()
                {   
                    public void run()
                    {   
                        printMessage(printMessageAt);
                    }   
                }).start();
    }   

    public void run()
    {   
        try 
        {   
            for(;;)
            {   
                increment();
                System.out.print(counter + " ");
                Thread.sleep(DELAY);
            }   
        }   
        catch(InterruptedException e)
        {   
            e.printStackTrace();
        }   
    }   

    private synchronized void increment()
    {   
        ++counter;
        notifyAll();
    }   

    private synchronized void printMessage(int printMessageAt)
    {   
        try 
        {   
            for(;;)
            {   
                wait();
                if ((counter % printMessageAt) == 0)
                {   
                    System.out.println();
                    System.out.println(printMessageAt + " second message");
                }   
            }   
        }   
        catch(InterruptedException e)
        {
            e.printStackTrace();
        }
    }
}

class Main2
{
    public static void main(String[] args)
    {   
        PrintTimer pt = new PrintTimer();
        pt.addMessageTimer(7);
        pt.addMessageTimer(15);
        new Thread(pt).start();
    }   
}*/