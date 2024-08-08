package uk.ac.ecs.soton.ik1g19.ch12;

import de.bwaldvogel.liblinear.SolverType;
import net.didion.jwnl.data.Exc;
import org.openimaj.data.DataSource;
import org.openimaj.data.dataset.Dataset;
import org.openimaj.data.dataset.GroupedDataset;
import org.openimaj.data.dataset.ListDataset;
import org.openimaj.data.dataset.VFSListDataset;
import org.openimaj.experiment.dataset.sampling.GroupSampler;
import org.openimaj.experiment.dataset.sampling.GroupedUniformRandomisedSampler;
import org.openimaj.experiment.dataset.split.GroupedRandomSplitter;
import org.openimaj.experiment.evaluation.classification.ClassificationEvaluator;
import org.openimaj.experiment.evaluation.classification.ClassificationResult;
import org.openimaj.experiment.evaluation.classification.analysers.confusionmatrix.CMAnalyser;
import org.openimaj.experiment.evaluation.classification.analysers.confusionmatrix.CMResult;
import org.openimaj.feature.DiskCachingFeatureExtractor;
import org.openimaj.feature.DoubleFV;
import org.openimaj.feature.FeatureExtractor;
import org.openimaj.feature.SparseIntFV;
import org.openimaj.feature.local.data.LocalFeatureListDataSource;
import org.openimaj.feature.local.list.LocalFeatureList;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.FImage;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.annotation.evaluation.datasets.Caltech101;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.feature.dense.gradient.dsift.ByteDSIFTKeypoint;
import org.openimaj.image.feature.dense.gradient.dsift.DenseSIFT;
import org.openimaj.image.feature.dense.gradient.dsift.PyramidDenseSIFT;
import org.openimaj.image.feature.local.aggregate.BagOfVisualWords;
import org.openimaj.image.feature.local.aggregate.BlockSpatialAggregator;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.ml.annotation.linear.LiblinearAnnotator;
import org.openimaj.ml.clustering.ByteCentroidsResult;
import org.openimaj.ml.clustering.assignment.HardAssigner;
import org.openimaj.ml.clustering.kmeans.ByteKMeans;
import org.openimaj.ml.kernel.HomogeneousKernelMap;
import org.openimaj.util.pair.IntFloatPair;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh12 {
    public static void main( String[] args ) throws Exception {
        //retrieving caltech101 dataset
        GroupedDataset<String, VFSListDataset<Caltech101.Record<FImage>>, Caltech101.Record<FImage>> allData =
                Caltech101.getData(ImageUtilities.FIMAGE_READER);


        //sampling dataset for performance
        GroupedDataset<String, ListDataset<Caltech101.Record<FImage>>, Caltech101.Record<FImage>> data =
                GroupSampler.sample(allData, 5, false);


        //dividing into training and testing data sets
        GroupedRandomSplitter<String, Caltech101.Record<FImage>> splits =
                new GroupedRandomSplitter<String, Caltech101.Record<FImage>>(data, 15, 0, 15);


        //constructing sift
        DenseSIFT dsift = new DenseSIFT(5, 7);
        PyramidDenseSIFT<FImage> pdsift = new PyramidDenseSIFT<FImage>(dsift, 6f, 7);


        //constructing assigner
        HardAssigner<byte[], float[], IntFloatPair> assigner =
                trainQuantiser(GroupedUniformRandomisedSampler.sample(splits.getTrainingDataset(), 30), pdsift);


        //exercise 1 & 2
        //using homgeneous and disk caching wrapper
        HomogeneousKernelMap homo =
                new HomogeneousKernelMap(HomogeneousKernelMap.KernelType.Chi2, HomogeneousKernelMap.WindowType.Rectangular);
        FeatureExtractor<DoubleFV, Caltech101.Record<FImage>> phow = new PHOWExtractor(pdsift, assigner);
        FeatureExtractor<DoubleFV, Caltech101.Record<FImage>> homoExtr = homo.createWrappedExtractor(phow);
        DiskCachingFeatureExtractor extractor = new DiskCachingFeatureExtractor(new File("C:\\Users\\isaac\\Documents\\repos\\visioncw1\\OpenIMAj-Tutorial01\\src\\main\\java\\uk\\ac\\ecs\\soton\\ik1g19\\ch12\\cache"), homoExtr);


        LiblinearAnnotator<Caltech101.Record<FImage>, String> ann = new LiblinearAnnotator<Caltech101.Record<FImage>, String>(
                extractor, LiblinearAnnotator.Mode.MULTICLASS, SolverType.L2R_L2LOSS_SVC, 1.0, 0.00001);
        ann.train(splits.getTrainingDataset());


        ClassificationEvaluator<CMResult<String>, String, Caltech101.Record<FImage>> eval =
                new ClassificationEvaluator<CMResult<String>, String, Caltech101.Record<FImage>>(
                        ann, splits.getTestDataset(), new CMAnalyser<Caltech101.Record<FImage>, String>(CMAnalyser.Strategy.SINGLE));

        Map<Caltech101.Record<FImage>, ClassificationResult<String>> guesses = eval.evaluate();
        CMResult<String> result = eval.analyse(guesses);

        //initially ran with an accuracy of 0.72
        System.out.println(result.toString());
    }



    static HardAssigner<byte[], float[], IntFloatPair> trainQuantiser(
            Dataset<Caltech101.Record<FImage>> sample, PyramidDenseSIFT<FImage> pdsift)
    {
        List<LocalFeatureList<ByteDSIFTKeypoint>> allkeys = new ArrayList<LocalFeatureList<ByteDSIFTKeypoint>>();

        for (Caltech101.Record<FImage> rec : sample) {
            FImage img = rec.getImage();

            pdsift.analyseImage(img);
            allkeys.add(pdsift.getByteKeypoints(0.005f));
        }

        if (allkeys.size() > 10000)
            allkeys = allkeys.subList(0, 10000);

        ByteKMeans km = ByteKMeans.createKDTreeEnsemble(300);
        DataSource<byte[]> datasource = new LocalFeatureListDataSource<ByteDSIFTKeypoint, byte[]>(allkeys);
        ByteCentroidsResult result = km.cluster(datasource);

        return result.defaultHardAssigner();
    }



    static class PHOWExtractor implements FeatureExtractor<DoubleFV, Caltech101.Record<FImage>> {
        PyramidDenseSIFT<FImage> pdsift;
        HardAssigner<byte[], float[], IntFloatPair> assigner;

        public PHOWExtractor(PyramidDenseSIFT<FImage> pdsift, HardAssigner<byte[], float[], IntFloatPair> assigner)
        {
            this.pdsift = pdsift;
            this.assigner = assigner;
        }

        public DoubleFV extractFeature(Caltech101.Record<FImage> object) {
            FImage image = object.getImage();
            pdsift.analyseImage(image);

            BagOfVisualWords<byte[]> bovw = new BagOfVisualWords<byte[]>(assigner);

            BlockSpatialAggregator<byte[], SparseIntFV> spatial = new BlockSpatialAggregator<byte[], SparseIntFV>(
                    bovw, 2, 2);

            return spatial.aggregate(pdsift.getByteKeypoints(0.015f), image.getBounds()).normaliseFV();
        }
    }
}
