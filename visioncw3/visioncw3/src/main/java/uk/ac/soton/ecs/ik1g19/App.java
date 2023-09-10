package uk.ac.soton.ecs.ik1g19;

import de.bwaldvogel.liblinear.SolverType;
import org.openimaj.data.DataSource;
import org.openimaj.data.dataset.Dataset;
import org.openimaj.data.dataset.GroupedDataset;
import org.openimaj.data.dataset.VFSGroupDataset;
import org.openimaj.data.dataset.VFSListDataset;
import org.openimaj.experiment.dataset.sampling.GroupedUniformRandomisedSampler;
import org.openimaj.experiment.dataset.split.GroupedRandomSplitter;
import org.openimaj.experiment.evaluation.classification.ClassificationEvaluator;
import org.openimaj.experiment.evaluation.classification.ClassificationResult;
import org.openimaj.experiment.evaluation.classification.Classifier;
import org.openimaj.experiment.evaluation.classification.analysers.confusionmatrix.CMAnalyser;
import org.openimaj.experiment.evaluation.classification.analysers.confusionmatrix.CMResult;
import org.openimaj.feature.FeatureExtractor;
import org.openimaj.feature.FloatFV;
import org.openimaj.feature.SparseIntFV;
import org.openimaj.feature.local.LocalFeatureImpl;
import org.openimaj.feature.local.Location;
import org.openimaj.feature.local.SpatialLocation;
import org.openimaj.feature.local.data.LocalFeatureListDataSource;
import org.openimaj.feature.local.list.LocalFeatureList;
import org.openimaj.feature.local.list.MemoryLocalFeatureList;
import org.openimaj.image.FImage;
import org.openimaj.image.Image;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.analyser.ImageAnalyser;
import org.openimaj.image.feature.local.aggregate.BagOfVisualWords;
import org.openimaj.image.pixel.sampling.RectangleSampler;
import org.openimaj.image.processor.SinglebandImageProcessor;
import org.openimaj.math.geometry.shape.Rectangle;
import org.openimaj.ml.annotation.linear.LiblinearAnnotator;
import org.openimaj.ml.clustering.FloatCentroidsResult;
import org.openimaj.ml.clustering.assignment.HardAssigner;
import org.openimaj.ml.clustering.kmeans.FloatKMeans;
import org.openimaj.util.pair.IntFloatPair;

import java.io.File;
import java.io.FileWriter;
import java.util.*;

/**
 * Run #2
 * Image classification using 15 one-vs-all classifiers
 * Trained using densely sampled patches with k-means clustering and visual words
 */
public class App {
    public static void main( String[] args ) {
        //creating datasets

        GroupedDataset<String, VFSListDataset<FImage>, FImage> trainingData;
        try {
            trainingData = new VFSGroupDataset<FImage>(
                    "zip:http://comp3204.ecs.soton.ac.uk/cw/training.zip", ImageUtilities.FIMAGE_READER);
        } catch (Exception e) {
            System.err.println("Training & testing data failed to load");
            System.err.println(e);
            return;
        }
        trainingData.remove("training");


        GroupedRandomSplitter<String, FImage> splits =
                new GroupedRandomSplitter<String, FImage>(trainingData, 80, 0, 20);


        //analyse patches with provided sizes and steps
        PatchFeature patch = new PatchFeature(6f,2f, 2f);


        //train quantiser on a sample of the data set
        HardAssigner<float[], float[], IntFloatPair> assigner =
                trainQuantiser(trainingData, patch);


        //create a feature extractor to densely sample patches
        FeatureExtractor<SparseIntFV, FImage> extractor = new PatchExtractor(patch, assigner);


        //train classification
        LiblinearAnnotator<FImage, String> ann = new LiblinearAnnotator<FImage, String>(
                extractor, LiblinearAnnotator.Mode.MULTICLASS, SolverType.L2R_L2LOSS_SVC, 1.0, 0.00001);
        ann.train(trainingData);


        //evaluation
//        ClassificationEvaluator<CMResult<String>, String, FImage> eval =
//                new ClassificationEvaluator<CMResult<String>, String, FImage>(
//                        ann, splits.getTestDataset(), new CMAnalyser<FImage, String>(CMAnalyser.Strategy.SINGLE));
//
//        Map<FImage, ClassificationResult<String>> guesses = eval.evaluate();
//        CMResult<String> result = eval.analyse(guesses);
//
//        System.out.println(result.getDetailReport());


        try {
            Evaluator eval = new Evaluator(ann, "run2");
            eval.getResults();
        } catch (Exception e) {System.out.println(e.getMessage());}
    }



    /**
     * @desc trains the quantiser using a sample of data and densely sampled patches
     * @param sample sample of image data
     * @param patch patch sampler
     * @return trained assigner
     */
    static HardAssigner<float[], float[], IntFloatPair> trainQuantiser(
            Dataset<FImage> sample, PatchFeature<FImage> patch)
    {
        List<LocalFeatureList<LocalFeatureImpl<Location, FloatFV>>> allkeys = new ArrayList<>();


        for (FImage rec : sample) {
            FImage img = rec;

            //densely sample patches and store
            patch.analyseImage(img);
            allkeys.add(patch.getPatchKeypoints());
        }


        //take first 1000 samples
        if (allkeys.size() > 10000)
            allkeys = allkeys.subList(0, 10000);


        //k-means clustering
        FloatKMeans km = FloatKMeans.createExact(700);
        DataSource<float[]> datasource =
                new LocalFeatureListDataSource<LocalFeatureImpl<Location, FloatFV>, float[]>(allkeys);
        FloatCentroidsResult result = km.cluster(datasource);


        return result.defaultHardAssigner();
    }
}





/**
 * @desc feature extractor to extract visual words from images
 */
class PatchExtractor implements FeatureExtractor<SparseIntFV, FImage> {
    PatchFeature<FImage> patch;
    HardAssigner<float[], float[], IntFloatPair> assigner;



    /**
     * @desc construct an extractor using a patch sampler and assigner
     * @param patch a dense patch sampler
     * @param assigner the trained quantiser
     */
    public PatchExtractor(PatchFeature<FImage> patch, HardAssigner<float[], float[], IntFloatPair> assigner)
    {
        this.patch = patch;
        this.assigner = assigner;
    }



    /**
     * @desc extract visual words from image
     * @param object FImage to extract from
     * @return extracted visual words as a SpareIntFV
     */
    public SparseIntFV extractFeature(FImage object) {
        FImage image = object.getImage();
        patch.analyseImage(image);

        BagOfVisualWords<float[]> bovw = new BagOfVisualWords<>(assigner);

        return bovw.aggregate(patch.getPatchKeypoints());
    }
}






/**
 * @desc image analyser for densely sampling patches across an image
 */
class PatchFeature <IMAGE extends Image<Float, IMAGE> & SinglebandImageProcessor.Processable<Float, FImage, IMAGE>> implements ImageAnalyser<IMAGE> {
    float size;
    float stepx;
    float stepy;
    private LocalFeatureList<LocalFeatureImpl<Location, FloatFV>> patchList;



    /**
     * @desc construct a patch sampler with given size and step
     * @param size the size of the patch (a x a)
     * @param stepx size of horizontal step
     * @param stepy size of vertical step
     */
    public PatchFeature(float size, float stepx, float stepy) {
        this.size = size;
        this.stepx = stepx;
        this.stepy = stepy;
        patchList = new MemoryLocalFeatureList<>();
    }



    public void analyseImage(IMAGE image) {
        //clear patches of previous analysis
        patchList.clear();


        //sample patches using sliding window provided by RectangleSampler
        RectangleSampler sampler = new RectangleSampler(image, stepx, stepy, size, size);


        Iterator<Rectangle> rects = sampler.allRectangles().iterator();
        while (rects.hasNext()) {
            Rectangle rect = rects.next();

            IMAGE rImg = image.extractROI(rect).normalise();

            Float[] pvF = new Float[rImg.getWidth()* rImg.getHeight()];
            rImg.getPixelVector(pvF);

            float[] pvf = new float[rImg.getWidth()* rImg.getHeight()];
            for (int i = 0; i < pvF.length; i++) { pvf[i] = pvF[i].floatValue(); }

            FloatFV fv = new FloatFV(pvf);

            patchList.add(new LocalFeatureImpl<Location, FloatFV>(new SpatialLocation(rect.x, rect.y),fv));
        }
    }


    /**
     * @desc returns densely sampled patches of analysed image
     * @return densely sampled patches as LocalFeatureList
     */
    public LocalFeatureList<LocalFeatureImpl<Location, FloatFV>> getPatchKeypoints() {
        return patchList;
    }
}




class Evaluator {

    Classifier<String, FImage> classifier;
    String runName;
    VFSListDataset<FImage> testDataset;

    // Call constructor with your Classifier and the string "runX". Where X is the number of your run.
    // Call the constructor and getResults() in your main method.
    public Evaluator(Classifier<String, FImage> classifier, String runName) throws Exception {
        this.classifier = classifier;
        this.runName = runName;
        this.testDataset = new VFSListDataset<FImage>("zip:http://comp3204.ecs.soton.ac.uk/cw/testing.zip", ImageUtilities.FIMAGE_READER);
    }

    public void getResults() throws Exception {
        String fileName = runName.concat(".txt");
        File file = new File(fileName);
        file.createNewFile();
        FileWriter fileWriter = new FileWriter(fileName);
        for(int i = 0; i < testDataset.numInstances(); i++) {
            FImage image = testDataset.getInstance(i);
            String imageFileName = testDataset.getFileObject(i).getName().getBaseName();
            ClassificationResult<String> result = classifier.classify(image);
            Set<String> setOfPredictedClasses = result.getPredictedClasses();
            String predictedClass = setOfPredictedClasses.iterator().next();
            fileWriter.write(imageFileName);
            fileWriter.write(" ");
            fileWriter.write(predictedClass);
            fileWriter.write("\n");
        }
        fileWriter.close();
    }
}