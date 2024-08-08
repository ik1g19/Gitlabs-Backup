/*
 * a belt used by the number factory to store numbers
 */
public class Belt extends CyclicQueue {
    private Object enqueueLock = new Object(); /* the lock to add numbers to the belt */

    private Object dequeueLock = new Object(); /* the lock to remove numbers from the belt */


    /*
     * initialises the size of the belt
     */
    public Belt(int size) {
        super(size);
    }


    /*
     * only one number can be added to the belt at a time
     * but a number can be removed
     * blocks the thread until the number can be added
     * then wakes another thread
     */
    @Override
    public void enqueue(int i) throws IndexOutOfBoundsException {
        synchronized (enqueueLock) {
            while (size == circularQueue.length) {
                try {
                    synchronized (this) {
                        this.wait();
                    }
                } catch (InterruptedException e) {}
            }

            super.enqueue(i);

            synchronized (this) {
                this.notify();
            }
        }
    }


    /*
     * only one number can be removed from the belt at a time
     * but a number can be added
     * blocks the thread until a number can be removed
     * then wakes another thread
     */
    @Override
    public int dequeue() throws IndexOutOfBoundsException {
        synchronized (dequeueLock) {
            while (size == 0) {
                try {
                    synchronized (this) {
                        this.wait();
                    }
                } catch (InterruptedException e) {}
            }

            int num = super.dequeue();

            synchronized (this) {
                this.notify();
            }

            return num;
        }
    }
}
