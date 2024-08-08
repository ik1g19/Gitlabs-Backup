import org.checkerframework.checker.units.qual.C;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/*
 * a cyclic queue to store integers
 */
public class CyclicQueue implements NumberQueue {
    protected int[] circularQueue; /* array to be used as the queue */
    protected int head; /* pointer to front of queue */
    protected int tail; /* pointer to rear of queue */
    protected int size; /* size of the queue */


    /*
     * initialises the circular queue and
     * its head and tail pointers
     * as well as the size
     */
    public CyclicQueue(int size) {
        circularQueue = new int[size];
        head = 0;
        tail = -1;
        this.size = 0;
    }


    /*
     * adds an element to the rear of the queue
     * throws an out of bounds exception if
     * the queue is full
     */
    @Override
    public void enqueue(int i) throws IndexOutOfBoundsException {
        if (size == circularQueue.length) throw new IndexOutOfBoundsException("Queue Full");

        if (tail == circularQueue.length - 1) tail = 0; /* recycle pointer to start of array */
        else tail++; /* increment pointer */

        circularQueue[tail] = i;

        size++;
    }


    /*
     * removes an element from the front of the queue
     * throws an out of bound exception if
     * the queue is full
     */
    @Override
    public int dequeue() throws IndexOutOfBoundsException {
        if (size == 0) throw new IndexOutOfBoundsException("Queue Empty");

        int element = circularQueue[head]; /* retrieving the element */

        if (head == circularQueue.length - 1) head = 0; /* recycle pointer to start of the array */
        else head++; /* increment pointer */

        size--;

        return element;
    }


    /*
     * returns true if the queue is empty
     * otherwise returns false
     */
    @Override
    public boolean isEmpty() {
        if (size == 0) return true;
        else return false;
    }


    public int[] getQueue() {
        return circularQueue;
    }
}
