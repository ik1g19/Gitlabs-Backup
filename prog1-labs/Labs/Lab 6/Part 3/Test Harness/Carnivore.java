//inherits from animal and is inherited carnivores
public abstract class Carnivore extends Animal {
    public Carnivore(String name, int age) {
        super(name, age);
    }

    public void eat(Food food) throws Exception {
        if (food instanceof Plant) {
            throw new Exception("Carnivores are not allowed to eat plants");
        }

        System.out.println(getName() + " is eating " + food.getName());
    }
}
