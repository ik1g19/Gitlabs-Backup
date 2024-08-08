import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

/*
 * a consumer is a factory worker
 * who takes numbers from the belt
 */
public class Consumer extends FactoryWorker {
    /*
     * intialise an id and belt
     */
    public Consumer (int id, NumberQueue queue) {
        super("Consumer", id, queue);
    }


    /*
     * output what the consumer picked
     */
    @Override
    public void message(int i) {
        System.out.println("Consumer " + id + " picked " + i);
    }


    /*
     * remove number from the belt
     */
    @Override
    public int action() {
        return belt.dequeue();
    }


    /*
     * remove numbers from the belt until interrupted
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
