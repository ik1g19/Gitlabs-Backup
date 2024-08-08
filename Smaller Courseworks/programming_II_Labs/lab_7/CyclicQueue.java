import org.checkerframework.checker.units.qual.C;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class CyclicQueue implements NumberQueue {
    int[] circularQueue; /* array to be used as the queue */
    int head; /* pointer to front of queue */
    int tail; /* pointer to rear of queue */
    int size; /* size of the queue */


    /*
     * initialises the circular queue and
     * its head and tail pointers
     * as well as the size
     */
    public CyclicQueue(int size) {
        circularQueue = new int[size];
        head = 0;
        tail = 0;
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

        circularQueue[tail] = i;

        if (tail == circularQueue.length - 1) tail = 0; /* recycle pointer to start of array */
        else tail++; /* increment pointer */
        System.out.println("tail = " + tail);

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

        if (head == circularQueue.length - 1) head = 0; /* recycle pointer to start of the array */
        else head++; /* increment pointer */

        int element = circularQueue[head]; /* retrieving the element */

        size--;

        System.out.println("head = " + head);

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


    public static void main(String args[]) throws IOException {
        BufferedReader reader =
                new BufferedReader(new InputStreamReader(System.in));

        CyclicQueue queue = new CyclicQueue(10);

        while (true) {
            System.out.println("Add or Remove?");

            String answer = reader.readLine();

            if (answer.equals("a")) {
                System.out.println("To add: ");
                int enqueue = Integer.parseInt(reader.readLine());
                try {
                    queue.enqueue(enqueue);
                } catch (IndexOutOfBoundsException e) {
                    System.out.println(e);
                }
            }
            if (answer.equals("r")) {
                try {
                    int dequeue = queue.dequeue();
                } catch (IndexOutOfBoundsException e) {
                    System.out.println(e);
                }
            }

            for (int i = 0; i < queue.getQueue().length; i++) {
                System.out.print(queue.getQueue()[i] + " ");
            }
            System.out.println();
        }
    }
}
