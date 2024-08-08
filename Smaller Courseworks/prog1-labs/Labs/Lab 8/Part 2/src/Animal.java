//the animal class is inherited from by all other animals
//holds the name and age of the animal
public abstract class Animal implements Comparable<Animal> {
    //creates variables to hold the name and age of the animals
    private String name;
    private int age;

    //initialises the animal as a newborn
    public Animal() {
        this("newborn", 0);
    }

    //initialises the name and age of the animal
    public Animal(String name, int age) {
        this.name = name;
        this.age = age;
    }

    //returns the name of the animal
    public String getName() {
        return name;
    }

    //returns the age of the animal
    public int getAge() {
        return age;
    }

    //defining an abstract method to state all animals can make noise
    public abstract void makeNoise();

    //defining an abstract method to state animals can eat, which may throw an exception
    public abstract void eat(Food food) throws Exception;

    //overloading the eat method
    public void eat(Food food, Integer n) {
        for (int i = 0; i < n; i++) {
            try {
                eat(food);
            }
            catch (Exception e) {
                System.err.println(e);
            }
        }
    }

    //compares the age of two animals
    public int compareTo(Animal animal) {
        return this.getAge() - animal.getAge();
    }
}
