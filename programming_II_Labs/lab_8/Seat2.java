import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.Seat;

import java.util.concurrent.locks.ReentrantLock;

/*
 * 1 instance of this seat is created
 */
public class Seat2 implements Seat {
    private ReentrantLock forkLock1;
    private ReentrantLock forkLock2;


    /*
     * assigns left and right forks
     */
    @Override
    public void assignForks(ReentrantLock reentrantLock, ReentrantLock reentrantLock1) {
        forkLock1 = reentrantLock;
        forkLock2 = reentrantLock1;
    }


    /*
     * requests left fork
     */
    @Override
    public void askFork1() {
        forkLock1.lock();
        askFork2();
    }


    /*
     * requests right fork
     */
    @Override
    public void askFork2() {
        forkLock2.lock();
    }
}
