//the wolf class inherits from the animal class
//and initialises the wolf with a name and age when the object is created
public class Wolf extends Animal{
    //calls the constructor of the superclass and passes the given values to it
    public Wolf(String name, int age) {
        super(name, age);
    }

    //outputs the wolfs noise
    public void makeNoise() {
        System.out.println("Rawr");
    }
}