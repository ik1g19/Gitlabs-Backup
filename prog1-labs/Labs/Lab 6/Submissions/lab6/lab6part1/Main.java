public class Main {
    public static void main(String args[]) {
        //creates a new wolf and parrot object
        Wolf wolfA = new Wolf("Krik", 13);
        Parrot parrotA = new Parrot("Carrot", 100);

        //outputs the name and age of the wolf and parrot
        System.out.println("Wolf name is " + wolfA.getName() + " and age is " + wolfA.getAge());
        System.out.println("Parrot name is " + parrotA.getName() + " and age is " + parrotA.getAge());

        //creates a new meat and plant object
        Meat myMeat = new Meat("Beat");
        Plant lesserMeat = new Plant("susMeat");
    }
}