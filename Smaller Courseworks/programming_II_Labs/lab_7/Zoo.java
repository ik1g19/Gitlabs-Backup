public class Zoo {
    public Zoo() {

    }


    public static void main(String args[]) {
        Counter counter = new Counter();

        Thread gate1 = new Thread(new Gate(counter, 3010));
        Thread gate2 = new Thread(new Gate(counter, 6000));
        Thread gate3 = new Thread(new Gate(counter, 206));
        Thread gate4 = new Thread(new Gate(counter, 1));
        Thread gate5 = new Thread(new Gate(counter, 1000));
        Thread gate6 = new Thread(new Gate(counter, 10000));

        gate1.start();
        gate2.start();
        gate3.start();
        gate4.start();
        gate5.start();
        gate6.start();

        try {
            gate6.join();
        } catch (InterruptedException e) {
            System.out.println("Thread Interrupted");
        }

        System.out.println(counter.getCounter());
    }
}
