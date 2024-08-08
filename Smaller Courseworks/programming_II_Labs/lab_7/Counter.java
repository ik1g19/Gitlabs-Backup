import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

/*
 * counts the number of guests
 */
public class Counter implements UnitCounter{
    private int count; /* the number of guest */


    /*
     * adds one to the counter variable
     */
    public void addOne() {
        count++;
    }


    /*
     * returns the current count
     */
    public int getCounter() {
        return count;
    }
}
