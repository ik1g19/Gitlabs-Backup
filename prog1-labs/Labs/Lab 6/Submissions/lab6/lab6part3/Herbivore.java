//inherits from animal and is inherited herbivores
public abstract class Herbivore extends Animal {
    public Herbivore(String name, int age) {
        super(name, age);
    }

    //eats a given food, throws an exception if the food is incompatible with the type of animal
    public void eat(Food food) throws Exception {

        if (food instanceof Meat) {
            throw new Exception("Herbivores are not allowed to eat meat");
        }

        System.out.println(getName() + " is eating " + food.getName());
    }
}
