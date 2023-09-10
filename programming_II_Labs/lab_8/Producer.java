import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

import java.util.Random;

/*
 * a producer is a factory worker
 * who adds numbers to the belt
 */
public class Producer extends FactoryWorker {
    /*
     * initialises the id and the belt
     */
    public Producer (int id, NumberQueue queue) {
        super("Producer", id, queue);
    }


    /*
     * outputs what the producer added to the belt
     */
    @Override
    public void message(int i) {
        System.out.println("Producer " + id + " produced " + i);
    }


    /*
     * adds a random number to the belt
     */
    @Override
    public int action() {
        Random rand = new Random();

        int num = rand.nextInt(10000);

        belt.enqueue(num);

        return num;
    }


    /*
     * adds numbers to the belt until interrupted
     */
    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                message(action());
            } catch (IndexOutOfBoundsException e) {
                messageError();
            }
        }
    }
}
