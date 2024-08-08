//inherits from animal and is inherited omnivores
public abstract class Omnivore extends Animal {
    public Omnivore(String name, int age) {
        super(name, age);
    }

    public void eat(Food food) {
        System.out.println(getName() + " is eating " + food.getName());
    }
}
