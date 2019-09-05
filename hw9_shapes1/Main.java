import java.util.*;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Collections;
import java.util.Arrays;
import java.util.Iterator;

//********************************************************************************************************** Point
class Point {
    
    public double x;
    public double y;
    
    public Point() {
        x = 0;
        y = 0;
    }
    
    public Point(double first, double second) {
        x = first;
        y = second;
    }
}

//********************************************************************************************************* Shape
abstract class Shape implements Comparable<Shape>{
	
    abstract Point position();
    abstract double area();
    
    public boolean equalTo() {
        return true;
    }
    
    public boolean equals(Object obj) {
        if(!(obj instanceof Shape)) return false;
        return true;
    }
    public int hashCode() {
        int hash = 5;
        
        return hash;
    }
    
        public int compareTo(Shape p) {
            //Triangle tri = (Triangle) p;
            if(p.area() < this.area()) {
                return 1;
            }
            else if(p.area() == this.area()) {
                return 0;
            }
            else return -1;
        }
    
}

//******************************************************************************************************* Triangle
class Triangle extends Shape{
    
    public Point a;
    public Point b;
    public Point c;
    
    public Triangle(Point pOne, Point pTwo, Point pThree) {
        a = pOne;
        b = pTwo;
        c = pThree;
    }
    
    public Point position() {
        Point pos = new Point();
        
        pos.x = (a.x + b.x + c.x)/3;
        pos.y = (a.y + b.y + c.y)/3;
        
        return pos;
    }
    
    public double area() {
        
        //Heron's Formula
        double veclen1 = Math.sqrt(Math.pow(Math.abs(a.x - b.x),2) + Math.pow(Math.abs(a.y - b.y),2));
        double veclen2 = Math.sqrt(Math.pow(Math.abs(b.x - c.x),2) + Math.pow(Math.abs(b.y - c.y),2));
        double veclen3 = Math.sqrt(Math.pow(Math.abs(a.x - c.x),2) + Math.pow(Math.abs(a.y - c.y),2));
        double s = (veclen1 + veclen2 + veclen3) / 2;
        double area = Math.sqrt((s) * (s-veclen1) * (s-veclen2) * (s-veclen3));
        
        return area;
    }
    
    public double perimeter() {
        double veclen1 = Math.sqrt(Math.pow(Math.abs(a.x - b.x),2) + Math.pow(Math.abs(a.y - b.y),2));
        double veclen2 = Math.sqrt(Math.pow(Math.abs(b.x - c.x),2) + Math.pow(Math.abs(b.y - c.y),2));
        double veclen3 = Math.sqrt(Math.pow(Math.abs(a.x - c.x),2) + Math.pow(Math.abs(a.y - c.y),2));
        
        double perimeter = veclen1 + veclen2 + veclen3;
        
        return perimeter;
    }
    
    public boolean equals(Object obj){
        if(!(obj instanceof Triangle)) return false;
        Triangle tri = (Triangle) obj;
        if(tri.position() != this.position() || tri.position() != this.position()) return false;
        if(tri.area() != this.area()) return false;
        if(tri.perimeter() != this.perimeter()) return false;
        
        return true;
    }
    
    public int hashCode() {
        int hash = 5;
        hash = 37 * hash + (this.position() != null ? this.position().hashCode() : 0);
        hash = 37 * hash + (int)this.area();
        hash = 37 * hash + (int)this.perimeter();

        return hash;
    }
    
//    public int compareTo(Shape p) {
//        //Triangle tri = (Triangle) p;
//        if(p.area() < this.area()) {
//            return 1;
//        }
//        else if(p.area() == this.area()) {
//            return 0;
//        }
//        else return -1;
//    }
    
    public String toString() {
        return "Triangle" + " (" + this.a.x + ", " + this.a.y + ")-(" + this.b.x + ", " + this.b.y + ")-(" + this.c.x + ", " + this.c.y + ")";
    }
}

//****************************************************************************************************** Rectangle
class Rectangle extends Shape{
	
    public Point a;
    public Point b;
    
    public double side_length1;
    public double side_length2;
    
    public Rectangle(Point pOne, Point pTwo) {
        a = pOne;
        b = pTwo;
        
        double veclen1 = Math.sqrt(Math.pow(Math.abs(a.y - b.y), 2));
        double veclen2 = Math.sqrt(Math.pow(Math.abs(a.x - b.x), 2));
        
        side_length1 = veclen1;
        side_length2 = veclen2;
	}
    
    public Point position() {
        Point pos = new Point();
        
        pos.x = (a.x + b.x)/2;
        pos.y = (a.y + b.y)/2;
        
        return pos;
    }
    
    public double area() {
        
        double ans = side_length1 * side_length2;
        
        return ans;
    }
    
    public double perimeter() {
        
        return (side_length1 * 2) + (side_length2 * 2);
    }
    
    public boolean equals(Object obj){
        if(!(obj instanceof Rectangle)) return false;
        Rectangle rec = (Rectangle) obj;
        if(rec.position() != this.position() || rec.position() != this.position()) return false;
        if(rec.area() != this.area()) return false;
        if(rec.perimeter() != this.perimeter()) return false;
        
        return true;
    }
    
    public int hashCode() {
        int hash = 5;
        hash = 37 * hash + (this.position() != null ? this.position().hashCode() : 0);
        hash = 37 * hash + (int)this.area();
        hash = 37 * hash + (int)this.perimeter();
        
        return hash;
    }
    
//    public int compareTo(Shape p) {
//        //Rectangle rec = (Rectangle) p;
//        if(p.area() < this.area()) {
//            return 1;
//        }
//        else if(p.area() == this.area()) {
//            return 0;
//        }
//        else return -1;
//    }
    
    public String toString() {
        return "Rectangle" + " (" + this.a.x + ", " + this.a.y + ")-(" + this.b.x + ", " + this.b.y + ")";
    }
}

//********************************************************************************************************* Circle
class Circle extends Shape{
	
    public Point a;
    double radius;
    
    public Circle(Point pOne, double number) {
        a = pOne;
        radius = number;
    }
    
    public Point position() {
        return a;
    }
    
    public double area() {
        
        double ans = Math.PI * Math.pow(radius, 2);
        
        return ans;
    }
    
    public boolean equals(Object obj){
        if(!(obj instanceof Circle)) return false;
        Circle cir = (Circle) obj;
        if(cir.position() != this.position() || cir.position() != this.position()) return false;
        if(cir.area() != this.area()) return false;
        if(cir.perimeter() != this.perimeter()) return false;
        
        return true;
    }
    
    public double perimeter() {
        
        return Math.pow((Math.PI*radius), 2);
    }
    
    public int hashCode() {
        int hash = 5;
        hash = 37 * hash + (this.position() != null ? this.position().hashCode() : 0);
        hash = 37 * hash + (int)this.area();
        hash = 37 * hash + (int)this.perimeter();
        
        return hash;
    }
    
//    public int compareTo(Shape p) {
//        //Circle cir = (Circle) p;
//        if(p.area() < this.area()) {
//            return 1;
//        }
//        else if(p.area() == this.area()) {
//            return 0;
//        }
//        else return -1;
//    }
    
    public String toString() {
        return "Circle" + " (" + this.a.x + ", " + this.a.y + "), radius = " + radius;
    }
}

//************************************************************************************************* AreaCalculator
class AreaCalculator {

}

//******************************************************************************************************** Node<T>
final class Node<T extends Shape> implements Iterable{
    public final T v;
    public Node<T> next;
    
    public Node(T val, Node<T> link) {
        v = val;
        next = link;
    }
    
    public Iterator<T> iterator() {
        
        return this.iterator();
    }
}

//************************************************************************************************ NodeIterator<T>
class NodeIterator<T extends Shape> implements Iterator<T>{
    
    public Node<T> current;
    
    public NodeIterator(Node<T> n) {
        current = n;
    }
    
    public boolean hasNext() {
        return current != null;
    }
    
    public T next() {
        if(!hasNext()){ throw new NoSuchElementException(); }

        T Val = current.v;
        current = current.next;
        
        return Val;
    }
    
    public void remove() {
        throw new UnsupportedOperationException();
    }
}

//*********************************************************************************************************** Main
class Main {
	public static void main(String args[]) {
		Shape shape[] = new Shape[4];
        
        Point p1 = new Point(.4, 5.3);
        Point p2 = new Point(.4, 5.6);
        Point p3 = new Point(3.0, 1.2);
        Triangle tri = new Triangle(p1, p2, p3);
        //System.out.println("tri:" + tri.area());
        
        //System.out.println(" ");
        
        Point p4 = new Point (3.0, -3.4);
        Point p5 = new Point (2.3, 0.0);
        Rectangle rec = new Rectangle(p4, p5);
        //System.out.println("rec:" + rec.area());
        
        //System.out.println(" ");
        
        Point p6 = new Point(1.0, 2.0);
        double radius = 4.4;
        Circle circ = new Circle(p6, radius);
        //System.out.println("circ: " + circ.area());
        
        //System.out.println(" ");
        
        Point p7 = new Point(0.0, 0.0);
        double radius2 = 10.0;
        Circle circ2 = new Circle(p7, radius2);
        //System.out.println("circ2: "circ2.area());
        
        System.out.println("circ: " + circ.area());
        System.out.println();
        System.out.println("tri: " + tri.area());
        System.out.println();
        System.out.println("circ2: " + circ2.area());
        System.out.println();
        System.out.println("rec: " + rec.area());
        System.out.println(" ");
        System.out.println("total area: " + (tri.area() + rec.area() + circ.area() + circ2.area()));
        
        System.out.println("*************************** Unsorted\n");
        
        /*ArrayList<Shape> shapeList = new ArrayList<Shape>();
        shapeList.add(circ);
        shapeList.add(tri);
        shapeList.add(circ2);
        shapeList.add(rec);
        
        Iterator<Shape> shapeIter = shapeList.iterator();
        while(shapeIter.hasNext()) {
            Shape c = shapeIter.next();
            System.out.println(c.getClass());
        }*/
        
        shape[0] = (Shape)circ;
        shape[1] = (Shape)circ2;
        shape[2] = (Shape)circ2;
        shape[3] = (Shape)circ;
        
        int count = 0;
        for (Shape s: shape) {
            System.out.println(++count + ") "+s+"\t\t area="+s.area());
        }
        
        System.out.println("*************************** Sorted\n");
        
        //Collections.sort(shapeList);
        
        /*Iterator<Shape> shapeIter2 = shapeList.iterator();
        while(shapeIter2.hasNext()) {
            Shape c = shapeIter2.next();
            System.out.println(c.getClass());
        }*/
        
        Arrays.sort(shape);
        int count2 = 0;
        for (Shape s: shape) {
            System.out.println(++count2 + ") "+s+"\t\t area="+s.area());
        }
        
        System.out.println("*************************** Using LinkedList\n");
        Node<Shape> LLShape = new Node<Shape>(null, null);
        Node<Shape> LLShape1 = new Node<Shape>(circ, LLShape);
        Node<Shape> LLShape2 = new Node<Shape>(tri, LLShape1);
        Node<Shape> LLShape3 = new Node<Shape>(circ2, LLShape2);
        
    }
}






