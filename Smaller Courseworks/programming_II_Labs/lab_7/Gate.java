/*
 * allows guests entry and updates the counter
 */
public class Gate implements Runnable {
    Counter counter; /* counter used to count guests */
    int numberOfGuests; /* number of guests entering */


    /*
     * initialises the counter and number
     * of guests member variables
     */
    public Gate(Counter counter, int numberOfGuests) {
        this.counter = counter;
        this.numberOfGuests = numberOfGuests;
    }


    /*
     * implementation of thread run method
     * to add one to the counter for every guest
     */
    @Override
    public void run() {
        for (int i = 0; i < numberOfGuests; i++) {
            counter.addOne();
        }
    }
}
