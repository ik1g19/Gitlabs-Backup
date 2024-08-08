//the parrot class inherits from the omnivore class
//the parrot is initialised with a name and age and can make a parrot noise
public class Parrot extends Omnivore {
    //calls the constructor of the super class and passes the given name and age
    public Parrot(String name, int age) {
        super(name, age);
    }

    //initialises the parrot with the name Polly and a given age
    public Parrot(Integer age) {
        this("Polly", age);
    }

    //outputs the parrots noise
    public void makeNoise() {
        System.out.println("Squawk");
    }
}
