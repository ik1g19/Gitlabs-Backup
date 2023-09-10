package uk.ac.ecs.soton.ik1g19.ch2;

import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.processing.edges.CannyEdgeDetector;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.math.geometry.shape.Ellipse;

import javax.swing.*;
import java.net.URL;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh2 {
    public static void main( String[] args ) throws Exception {
    	//Create an image
        MBFImage image = ImageUtilities.readMBF(new URL("http://static.openimaj.org/media/tutorial/sinaface.jpg"));

        //exercise 1
        //Create a window
        JFrame window = DisplayUtilities.createNamedWindow("window");

        //Displaying first image
        DisplayUtilities.display(image,window);
        Thread.sleep(1000);

        //Displaying second image
        DisplayUtilities.display(image.getBand(0),window);
        Thread.sleep(1000);

        MBFImage clone = image.clone();
        clone.getBand(1).fill(0f);
        clone.getBand(2).fill(0f);
        //Displaying fourth image
        DisplayUtilities.display(clone,window);
        Thread.sleep(1000);

        image.processInplace(new CannyEdgeDetector());
        //Displaying fifth image
        DisplayUtilities.display(image,window);
        Thread.sleep(1000);

        //exercise 2
        image.drawShapeFilled(new Ellipse(700f, 450f, 23f, 13f, 0f), RGBColour.RED);
        image.drawShapeFilled(new Ellipse(700f, 450f, 20f, 10f, 0f), RGBColour.WHITE);
        image.drawShapeFilled(new Ellipse(650f, 425f, 28f, 15f, 0f), RGBColour.RED);
        image.drawShapeFilled(new Ellipse(650f, 425f, 25f, 12f, 0f), RGBColour.WHITE);
        image.drawShapeFilled(new Ellipse(600f, 380f, 33f, 18f, 0f), RGBColour.RED);
        image.drawShapeFilled(new Ellipse(600f, 380f, 30f, 15f, 0f), RGBColour.WHITE);
        image.drawShapeFilled(new Ellipse(500f, 300f, 103f, 73f, 0f), RGBColour.RED);
        image.drawShapeFilled(new Ellipse(500f, 300f, 100f, 70f, 0f), RGBColour.WHITE);
        image.drawText("OpenIMAJ is", 425, 300, HersheyFont.ASTROLOGY, 20, RGBColour.BLACK);
        image.drawText("Awesome", 425, 330, HersheyFont.ASTROLOGY, 20, RGBColour.BLACK);
        //Display sixth image
        DisplayUtilities.display(image,window);
    }
}
