public class WinningAnimationThread implements Runnable {
    private GridSquare tile;

    private int r;
    private int g;
    private int b;

    private long wait = 10;
    private int increment = 2;

    boolean increasing = true;

    public WinningAnimationThread(GridSquare tile, int r, int g, int b) {
        this.tile = tile;

        this.r = r;
        this.g = g;
        this.b = b;
    }

    public void run(){
        /*while (tile.shouldContinueAnimation()) {
            //pause
            try {Thread.sleep(wait);}
            catch (InterruptedException e) {}

            if (increasing) b += increment;
            else b -= increment;

            if (b >= 255) {
                increasing = !increasing;
                b = 255;
            }
            if (b <= 0) {
                increasing = !increasing;
                b = 0;
            }

            tile.drawRGB(r, g, b);
        }*/
    }
}
