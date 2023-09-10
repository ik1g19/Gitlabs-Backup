package uk.ac.ecs.soton.ik1g19.ch13;

import org.openimaj.data.RandomData;
import org.openimaj.data.dataset.GroupedDataset;
import org.openimaj.data.dataset.ListDataset;
import org.openimaj.data.dataset.VFSGroupDataset;
import org.openimaj.experiment.dataset.sampling.GroupedUniformRandomisedSampler;
import org.openimaj.experiment.dataset.split.GroupedRandomSplitter;
import org.openimaj.experiment.dataset.util.DatasetAdaptors;
import org.openimaj.feature.DoubleFV;
import org.openimaj.feature.DoubleFVComparison;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.FImage;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.model.EigenImages;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;

import javax.swing.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh13 {
    public static void main( String[] args ) throws Exception {

        VFSGroupDataset<FImage> dataset =
                new VFSGroupDataset<FImage>("zip:http://datasets.openimaj.org/att_faces.zip", ImageUtilities.FIMAGE_READER);


        //exercise 2
        //as the number of training images decrease,
        //the accuracy of the recogniser decreases
        int nTraining = 3;
        int nTesting = 5;
        GroupedRandomSplitter<String, FImage> splits =
                new GroupedRandomSplitter<String, FImage>(dataset, nTraining, 0, nTesting);
        GroupedDataset<String, ListDataset<FImage>, FImage> training = splits.getTrainingDataset();
        GroupedDataset<String, ListDataset<FImage>, FImage> testing = splits.getTestDataset();

        List<FImage> basisImages = DatasetAdaptors.asList(training);
        int nEigenvectors = 100;
        EigenImages eigen = new EigenImages(nEigenvectors);
        eigen.train(basisImages);


        //threshold for exercise 3
        final double threshold = 100.0;

        Map<String, DoubleFV[]> features = new HashMap<String, DoubleFV[]>();
        for (final String person : training.getGroups()) {
            final DoubleFV[] fvs = new DoubleFV[nTraining];

            for (int i = 0; i < nTraining; i++) {
                final FImage face = training.get(person).get(i);
                fvs[i] = eigen.extractFeature(face);
            }
            features.put(person, fvs);
        }

        double correct = 0, incorrect = 0;
        for (String truePerson : testing.getGroups()) {
            for (FImage face : testing.get(truePerson)) {
                DoubleFV testFeature = eigen.extractFeature(face);

                String bestPerson = null;
                double minDistance = Double.MAX_VALUE;
                for (final String person : features.keySet()) {
                    for (final DoubleFV fv : features.get(person)) {
                        double distance = fv.compare(testFeature, DoubleFVComparison.EUCLIDEAN);

                        if (distance < minDistance) {
                            minDistance = distance;
                            bestPerson = person;
                        }
                    }
                }

                //exercise 3
                //an unknown value is returned if the min distance is
                //above a certain threshold
                if (minDistance > threshold) {
                    System.out.println("Actual: " + truePerson + "\tguess: unknown");
                    incorrect++;
                }
                else {
                    System.out.println("Actual: " + truePerson + "\tguess: " + bestPerson);

                    if (truePerson.equals(bestPerson))
                        correct++;
                    else
                        incorrect++;
                }
            }
        }

        System.out.println("Accuracy: " + (correct / (correct + incorrect)));


        //exercise 1
        FImage face = testing.getRandomInstance();
        DoubleFV feature = eigen.extractFeature(face);
        FImage reconstructed = eigen.reconstruct(feature).normalise();

        List<FImage> comparison = new ArrayList<FImage>();
        comparison.add(face); comparison.add(reconstructed);
        DisplayUtilities.display("comparison", comparison);

    }
}
