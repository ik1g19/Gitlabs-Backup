public class Elephant extends Herbivore {
    //calls the constructor of the super class and passes the given name and age
    public Elephant(String name, int age) {
        super(name, age);
    }

    //outputs the parrots noise
    public void makeNoise() {
        System.out.println("Toot");
    }
}
