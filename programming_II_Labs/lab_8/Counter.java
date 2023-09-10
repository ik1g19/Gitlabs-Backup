import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

/*
 * counts the number of guests
 */
public class Counter implements UnitCounter{
    private int count; /* the number of guests */


    /*
     * adds one to the counter variable
     * a thread must obtain the objects lock in order
     * to change the count
     */
    public synchronized void addOne() {
        count++;
    }


    /*
     * returns the current count
     */
    public synchronized int getCounter() {
        int count = this.count;
        return count;
    }
}
