public class Main {

    public static void main(String args[]) {
        Wolf wolfA = new Wolf("Krik", 13);
        Parrot parrotA = new Parrot("Carrot", 100);

        System.out.println("Wolf name is " + wolfA.getName() + " and age is " + wolfA.getAge());
        System.out.println("Parrot name is " + parrotA.getName() + " and age is " + parrotA.getAge());

        Meat myMeat = new Meat("Beat");
        Plant lesserMeat = new Plant("susMeat");
    }
}
