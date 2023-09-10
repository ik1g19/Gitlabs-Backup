package uk.ac.ecs.soton.ik1g19.ch1;

import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh1 {
    public static void main( String[] args ) {
    	//Create an image
        MBFImage image = new MBFImage(700,70, ColourSpace.RGB);

        //Fill the image with white
        image.fill(RGBColour.WHITE);
        		        
        //Render some test into the image
        //Modified text, font and colour
        image.drawText("I do not understand template convolution", 10, 60, HersheyFont.GOTHIC_ENGLISH, 35, RGBColour.RED);

        //Apply a Gaussian blur
        image.processInplace(new FGaussianConvolve(2f));
        
        //Display the image
        DisplayUtilities.display(image);
    }
}
