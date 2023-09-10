//the parrot class inherits from the animal class
//and initialises the parrot with a name and age when the object is created
public class Parrot extends Omnivore {
    //calls the constructor of the super class and passes the given name and age
    public Parrot(String name, int age) {
        super(name, age);
    }

    //outputs the parrots noise
    public void makeNoise() {
        System.out.println("Squawk");
    }
}
