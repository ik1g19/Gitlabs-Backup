package uk.ac.ecs.soton.ik1g19.ch3;

import org.apache.commons.lang.ArrayUtils;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.connectedcomponent.GreyscaleConnectedComponentLabeler;
import org.openimaj.image.pixel.ConnectedComponent;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.processor.PixelProcessor;
import org.openimaj.image.segmentation.FelzenszwalbHuttenlocherSegmenter;
import org.openimaj.image.segmentation.SegmentationUtilities;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.ml.clustering.FloatCentroidsResult;
import org.openimaj.ml.clustering.assignment.HardAssigner;
import org.openimaj.ml.clustering.kmeans.FloatKMeans;

import javax.swing.*;
import java.net.URL;
import java.util.Arrays;
import java.util.List;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh3 {
    public static void main( String[] args ) throws Exception {
        //create an image
        MBFImage input = ImageUtilities.readMBF(new URL("https://images.roughtrade.com/product/images/files/000/205/613/original/850ed345cf921d216bb97b65ab6e765e.999x999x1.jpg?1604600739"));


        //convert to LAB colour space
        input = ColourSpace.convert(input, ColourSpace.CIE_Lab);


        //construct k-means algorithm for k=2
        FloatKMeans cluster = FloatKMeans.createExact(2);


        //extract pixel array for use by k-means
        float[][] imageData = input.getPixelVectorNative(new float[input.getWidth() * input.getHeight()][3]);
        FloatCentroidsResult result = cluster.cluster(imageData);


        final float[][] centroids = result.centroids;
        final HardAssigner<float[],?,?> assigner = result.defaultHardAssigner();
        //original loop
        /*
        for (int y=0; y<input.getHeight(); y++) {
            for (int x=0; x<input.getWidth(); x++) {
                float[] pixel = input.getPixelNative(x, y);
                int centroid = assigner.assign(pixel);
                input.setPixelNative(x, y, centroids[centroid]);
            }
        }
         */

        //exercise 1
        /*
        pixel processor improves efficiency as you don't have
        to use a nested for loop
         */
        MBFImage kInput = input.process(new PixelProcessor<Float[]>() {
            public Float[] processPixel(Float[] pixel) {
                int centroid = assigner.assign(ArrayUtils.toPrimitive(pixel));
                return ArrayUtils.toObject(centroids[centroid]);
            }
        });


        input = ColourSpace.convert(input, ColourSpace.RGB);


        GreyscaleConnectedComponentLabeler labeler = new GreyscaleConnectedComponentLabeler();
        List<ConnectedComponent> components = labeler.findComponents(input.flatten());


        int i = 0;
        for (ConnectedComponent comp : components) {
            if (comp.calculateArea() < 50)
                continue;
            input.drawText("Point:" + (i++), comp.calculateCentroidPixel(), HersheyFont.TIMES_MEDIUM, 20);
        }


        JFrame kmeans = DisplayUtilities.createNamedWindow("k-means");
        DisplayUtilities.display(input,kmeans);


        //exercise 2
        FelzenszwalbHuttenlocherSegmenter<MBFImage> segmenter = new FelzenszwalbHuttenlocherSegmenter();
        List<ConnectedComponent> comps = segmenter.segment(input);
        SegmentationUtilities.renderSegments(input,comps);

        JFrame felzen = DisplayUtilities.createNamedWindow("felzen");
        DisplayUtilities.display(input,felzen);
    }
}
