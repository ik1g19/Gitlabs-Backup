import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

import java.util.Random;

public class Producer extends FactoryWorker {
    public Producer (int id, NumberQueue queue) {
        super("Producer", id, queue);
    }


    @Override
    public void message(int i) {
        System.out.println("Producer " + id + " produced " + i);
    }


    @Override
    public int action() {
        Random rand = new Random();

        int num = rand.nextInt(10000);

        belt.enqueue(rand.nextInt(num));

        return num;
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
