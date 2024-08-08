public class Main {
    public static void main(String[] args) {
        Toolbox myToolbox = new Toolbox();

        System.out.println("What multiplication table do you want to calculate?");

        int tableKey;

        tableKey = myToolbox.readIntegerFromCmd();

        for (int i = 1; i <= 20; i++) {
            System.out.println(i * tableKey);
        }

        int total = 0;
        int increment = 1;

        while (total < 500) {
            total += increment;
            increment++;
        }

        increment--;

        //System.out.println("It took "+increment+" iterations to sum the consecutive integers over 500");
        System.out.println(increment);
    }
}