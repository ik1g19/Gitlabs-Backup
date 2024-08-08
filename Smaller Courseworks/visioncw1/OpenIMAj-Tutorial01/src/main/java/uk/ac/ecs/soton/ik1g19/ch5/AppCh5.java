package uk.ac.ecs.soton.ik1g19.ch5;

import org.openimaj.feature.local.list.LocalFeatureList;
import org.openimaj.feature.local.matcher.*;
import org.openimaj.feature.local.matcher.consistent.ConsistentLocalFeatureMatcher2d;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.feature.local.engine.DoGSIFTEngine;
import org.openimaj.image.feature.local.keypoints.Keypoint;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.math.geometry.transforms.HomographyRefinement;
import org.openimaj.math.geometry.transforms.estimation.RobustAffineTransformEstimator;
import org.openimaj.math.geometry.transforms.estimation.RobustHomographyEstimator;
import org.openimaj.math.model.fit.RANSAC;

import java.net.URL;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh5 {
    public static void main( String[] args ) throws Exception {
        MBFImage query = ImageUtilities.readMBF(new URL("http://static.openimaj.org/media/tutorial/query.jpg"));
        MBFImage target = ImageUtilities.readMBF(new URL("http://static.openimaj.org/media/tutorial/target.jpg"));

        DoGSIFTEngine engine = new DoGSIFTEngine();
        LocalFeatureList<Keypoint> queryKeypoints = engine.findFeatures(query.flatten());
        LocalFeatureList<Keypoint> targetKeypoints = engine.findFeatures(target.flatten());


        /*
        RobustAffineTransformEstimator modelFitter = new RobustAffineTransformEstimator(50.0, 1500,
                new RANSAC.PercentageInliersStoppingCondition(0.5));
         */


        //exercise 2
        RobustHomographyEstimator modelFitter = new RobustHomographyEstimator(50.0, HomographyRefinement.NONE);

        LocalFeatureMatcher<Keypoint> matcher = new ConsistentLocalFeatureMatcher2d<Keypoint>(
                new FastBasicKeypointMatcher<Keypoint>(8), modelFitter);


        //exercise 1
        //BasicTwoWayMatcher<Keypoint> matcher = new BasicTwoWayMatcher<Keypoint>();


        matcher.setModelFeatures(queryKeypoints);
        matcher.findMatches(targetKeypoints);


        MBFImage consistentMatches = MatchingUtilities.drawMatches(query, target, matcher.getMatches(),
                RGBColour.RED);
        DisplayUtilities.display(consistentMatches);
    }
}
