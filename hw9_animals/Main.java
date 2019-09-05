import java.util.*;

abstract class Animal {
	
    String name;
    int order;
    
    public String getName() {
        return name;
    }
	public int getOrder() {
        return order;
    }
	
    abstract void cry();

}

class Cat extends Animal {
    
    public Cat(String name) {
        super.name = name;
    }
    
	public void cry() {
		System.out.println("meow");
	}
}

class Dog extends Animal {
    
    public Dog(String name) {
        super.name = name;
    }
    
	public void cry() {
		System.out.println("bark");
	}

}

class AnimalShelter {
	
    LinkedList<Animal> animalList = new LinkedList<Animal>();
	
	public void addAnimal(Animal animal) {
        animalList.addLast(animal);
        animal.order = animalList.indexOf(animal);
    }
    
	public void adopt() {
        animalList.removeFirst();
        for(int i = 0; i < animalList.size(); i++) {
            animalList.get(i).order = i-1;
        }
    }
    
	public void adoptCat() {
        
        int tempIndex = 0;
        
        for(int i = 0; i < animalList.size(); i++) {
            if(animalList.get(i).getClass() == Cat.class) {
                tempIndex = i;
                break;
            }
        }
        
        animalList.remove(tempIndex);
        for(int i = tempIndex; i < animalList.size(); i++) {
            animalList.get(i).order = i-1;
        }
    }
    
	public void adoptDog() {
        int tempIndex = 0;
        
        for(int i = 0; i < animalList.size(); i++) {
            if(animalList.get(i).getClass() == Dog.class) {
                tempIndex = i;
                break;
            }
        }
        
        animalList.remove(tempIndex);
        for(int i = tempIndex; i < animalList.size(); i++) {
            animalList.get(i).order = i-1;
        }
    }
	
	public void remainingAnimals() {
		for(int i=0; i<animalList.size(); i++) {
			System.out.println(animalList.get(i).getName());
		}
	}
    
	public void remainingCats() {
        for(int i=0; i<animalList.size(); i++) {
            if(animalList.get(i).getClass() == Cat.class) {
                System.out.println(animalList.get(i).getName());
            }
        }
    }
	public void remainingDogs() {
        for(int i=0; i<animalList.size(); i++) {
            if(animalList.get(i).getClass() == Dog.class) {
                System.out.println(animalList.get(i).getName());
            }
        }
    }
}

class Main {
	public static void main(String args[]) {
        System.out.println("1: Add new animal");
		System.out.println("2: Adopt an animal");
		System.out.println("3: Adopt a cat");
		System.out.println("4: Adopt a dog");
		System.out.println("5: Show animals in the shelter");
		System.out.println("6: Show cats in the shelter");
		System.out.println("7: Show dogs in the shelter");
		System.out.println("0: Exit");
		System.out.println("Enter a number: ");
        
        AnimalShelter animalShelter1 = new AnimalShelter();
        Dog x = new Dog("Robert");
        Cat a = new Cat("Akash");
        Dog b = new Dog("Cole");
        Cat c = new Cat("Julian");
        Dog d = new Dog("Jeff");
        
        animalShelter1.addAnimal(x);
        animalShelter1.addAnimal(a);
        animalShelter1.addAnimal(b);
        animalShelter1.addAnimal(c);
        animalShelter1.addAnimal(d);
        
        animalShelter1.remainingAnimals();
        animalShelter1.adopt();
        System.out.println(" ");
        animalShelter1.remainingDogs();
        
	}
}

