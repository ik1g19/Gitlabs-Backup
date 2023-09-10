package uk.ac.ecs.soton.ik1g19.ch4;

import org.openimaj.feature.DoubleFVComparison;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.pixel.statistics.HistogramModel;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.math.statistics.distribution.MultidimensionalHistogram;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh4 {
    public static void main( String[] args ) throws Exception {

        URL[] imageURLs = new URL[] {
                new URL( "http://openimaj.org/tutorial/figs/hist1.jpg" ),
                new URL( "http://openimaj.org/tutorial/figs/hist2.jpg" ),
                new URL( "http://openimaj.org/tutorial/figs/hist3.jpg" )
        };

        List<MultidimensionalHistogram> histograms = new ArrayList<MultidimensionalHistogram>();
        HistogramModel model = new HistogramModel(4, 4, 4);

        for( URL u : imageURLs ) {
            model.estimateModel(ImageUtilities.readMBF(u));
            histograms.add( model.histogram.clone() );
        }


        //exercise 1 & 2
        //displaying the two most similar non-trivial images
        double minDistance = Double.MAX_VALUE; int minI = 0; int minJ = 0;
        for( int i = 0; i < histograms.size(); i++ ) {
            for( int j = i; j < histograms.size(); j++ ) {

                //comparison for exercise 1
                //double distance = histograms.get(i).compare( histograms.get(j), DoubleFVComparison.EUCLIDEAN );

                //comparison for exercise 2
                //the new comparison returns a different pair of images
                //which have less similar colours than the pair obtained
                //by taking euclidean distance
                double distance = histograms.get(i).compare( histograms.get(j), DoubleFVComparison.INTERSECTION );
                if (distance < minDistance && i != j) {minDistance = distance; minI = i; minJ = j;}

            }
        }

        List<MBFImage> similar = Arrays.asList(
                ImageUtilities.readMBF(imageURLs[minI]),ImageUtilities.readMBF(imageURLs[minJ])
        );

        DisplayUtilities.display("similar images", similar);

    }
}
