public class Parrot extends Animal {
    //calls the constructor of the super class and passes the given name and age
    public Parrot(String name, int age) {
        super(name, age);
    }

    //outputs the parrots noise
    public void makeNoise() {
        System.out.println("Squawk");
    }
}
