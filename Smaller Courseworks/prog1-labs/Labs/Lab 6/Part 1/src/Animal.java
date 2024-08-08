public class Animal {
    //creates variables to hold the name and age of the animals
    private String name;
    private int age;

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
}
