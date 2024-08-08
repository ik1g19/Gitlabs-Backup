import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

public class Consumer extends FactoryWorker {
    public Consumer (int id, NumberQueue queue) {
        super("Consumer", id, queue);
    }


    @Override
    public void message(int i) {
        System.out.println("Consumer " + id + " picked " + i);
    }


    @Override
    public int action() {
        return belt.dequeue();
    }


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
