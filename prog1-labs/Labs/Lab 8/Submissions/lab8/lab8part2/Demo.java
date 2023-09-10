import java.util.ArrayList;
import java.util.Collections;

public class Demo {
    public static void main(String args[]) {
        ArrayList<Animal> animals = new ArrayList<Animal>();

        //filling the list with animals
        animals.add(new Wolf("Kit", 2));
        animals.add(new Parrot("Jeffrey", 1003));
        animals.add(new Elephant("Michael Stevens", 1000000000));
        animals.add(new Wolf("Flow", 1));
        animals.add(new Parrot("Torrap", 10));

        /*
        An interface is a collection of abstract methods
        How an interface differs from an abstract class:
        -An interface does not contain any constructors
        -All methods in an interface are abstract
         */

        //outputs the original list of animals
        for (Animal animal : animals) {
            System.out.println("Name: " + animal.getName() + " Age: " + animal.getAge());
        }

        System.out.println();

        //sorts the animals
        Collections.sort(animals);

        //outputs the sorted list of animals
        for (Animal animal : animals) {
            System.out.println("Name: " + animal.getName() + " Age: " + animal.getAge());
        }

        /*
        To sort the animals from highest age to lowest age,
        the elements being subtracted in the compareTo method should be swapped round
         */
    }
}
