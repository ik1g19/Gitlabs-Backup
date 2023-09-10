//the wolf class inherits from the carnivore class
//the wolf is initialised with a name and age and can make a wolf noise
public class Wolf extends Carnivore{
    //calls the constructor of the superclass and passes the given values to it
    public Wolf(String name, int age) {
        super(name, age);
    }

    //initialises the animal as a newborn
    public Wolf() {super();};

    //outputs the wolfs noise
    public void makeNoise() {
        System.out.println("Rawr");
    }
}