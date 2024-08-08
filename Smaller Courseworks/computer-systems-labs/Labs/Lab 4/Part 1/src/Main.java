//this class prints a times table which is specified by the user
//and calculates how many consecutive integers need to be added together to total over 500
public class Main {
    public static void main(String[] args) {
        Toolbox myToolbox = new Toolbox();

        //prompting the user to choose a times table
        System.out.println("What multiplication table do you want to calculate?");

        //declaring a variable to store which times table is to be printed
        int tableKey;
        tableKey = myToolbox.readIntegerFromCmd();

        //prints out each element of the table
        //by multiplying an increasing number by the table key
        for (int i = 1; i <= 20; i++) {
            System.out.println(i * tableKey);
        }

        //creates a variable to hold the total sum so far
        int total = 0;
        //creates a variable that increases by 1 every loop
        int increment = 1;

        while (total < 500) {
            //the total is incremented by the variable "increment"
            total += increment;
            //the variable "increment" is increased by 1
            increment++;
        }

        //decrease increment by 1 to find the number of iterations that was needed
        increment--;

        //output how many iterations were needed
        System.out.println(increment);
    }
}