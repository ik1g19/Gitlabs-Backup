package uk.ac.soton.ecs.ik1g19.hybridimages;

import jAudioFeatureExtractor.AudioFeatures.FeatureExtractor;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.FImage;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.processing.resize.ResizeProcessor;
import org.openimaj.image.typography.hershey.HersheyFont;

import javax.swing.*;
import java.io.File;
import java.net.URL;


public class App {
    public static void main( String[] args ) throws Exception {
    	//Create images
        MBFImage img1 = ImageUtilities.readMBF(new File("C:\\Users\\isaac\\Documents\\repos\\visioncw2\\Vision-CW2\\media\\wonka.png"));
        MBFImage img2 = ImageUtilities.readMBF(new File("C:\\Users\\isaac\\Documents\\repos\\visioncw2\\Vision-CW2\\media\\mark.png"));

        MBFImage hybrid = MyHybridImages.makeHybrid(img1, 5, img2, 6);
        int w = hybrid.getWidth();
        int h = hybrid.getHeight();

        MBFImage scaledHybrids = new MBFImage(w*2, h,ColourSpace.RGB);
        scaledHybrids.drawImage(hybrid,0,0);
        scaledHybrids.drawImage(hybrid.process(new ResizeProcessor(0.5f)),w,0);
        scaledHybrids.drawImage(hybrid.process(new ResizeProcessor(0.25f)),w + w/2,0);
        scaledHybrids.drawImage(hybrid.process(new ResizeProcessor(0.125f)),w + w/2 + w/4,0);
        
        //Display the image
        DisplayUtilities.display(scaledHybrids);
    }
}
