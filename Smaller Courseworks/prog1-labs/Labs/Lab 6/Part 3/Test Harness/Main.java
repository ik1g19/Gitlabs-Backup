//the main class contains the main method
public class Main {
    public static void main(String args[]) {
        //creates a new wolf and parrot object
        Wolf wolfA = new Wolf("Krik", 13);
        Parrot parrotA = new Parrot("Carrot", 100);

        //outputs the name and age of the wolf and parrot
        System.out.println("Wolf name is " + wolfA.getName() + " and age is " + wolfA.getAge());
        System.out.println("Parrot name is " + parrotA.getName() + " and age is " + parrotA.getAge());

        //creates a new meat and plant object
        Meat meat = new Meat("Steak");
        Plant plant = new Plant("Leaves");

        wolfA.makeNoise();
        parrotA.makeNoise();

        Elephant elephantA = new Elephant("Ellie", 100);

        try {
            elephantA.eat(meat);
            wolfA.eat(plant);
        }
        catch (Exception e) {
            System.err.println(e);
        }
    }
}