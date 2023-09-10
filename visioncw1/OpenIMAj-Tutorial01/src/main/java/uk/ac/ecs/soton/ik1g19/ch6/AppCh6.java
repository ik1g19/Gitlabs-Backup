package uk.ac.ecs.soton.ik1g19.ch6;

import jogamp.opengl.glu.nurbs.Bin;
import org.openimaj.data.dataset.MapBackedDataset;
import org.openimaj.data.dataset.VFSGroupDataset;
import org.openimaj.data.dataset.VFSListDataset;
import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.FImage;
import org.openimaj.image.ImageUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.colour.ColourSpace;
import org.openimaj.image.colour.RGBColour;
import org.openimaj.image.dataset.BingImageDataset;
import org.openimaj.image.processing.convolution.FGaussianConvolve;
import org.openimaj.image.typography.hershey.HersheyFont;
import org.openimaj.util.api.auth.DefaultTokenFactory;
import org.openimaj.util.api.auth.common.BingAPIToken;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh6 {
    public static void main( String[] args ) throws Exception {
        //VFSListDataset<FImage> images =
        //        new VFSListDataset<FImage>("C:\\Users\\isaac\\Pictures\\ForVisionTesting", ImageUtilities.FIMAGE_READER);

        //System.out.println(images.size());

        //DisplayUtilities.display(images.getRandomInstance(), "A random image from the dataset");
        //DisplayUtilities.display("My images", images);


        //retrieving a set of non-local images
        /*
        VFSListDataset<FImage> faces =
                new VFSListDataset<FImage>("zip:http://datasets.openimaj.org/att_faces.zip", ImageUtilities.FIMAGE_READER);
        DisplayUtilities.display("ATT faces", faces);
         */

        //maintains directory structure
        /*
        VFSGroupDataset<FImage> groupedFaces =
                new VFSGroupDataset<FImage>( "zip:http://datasets.openimaj.org/att_faces.zip", ImageUtilities.FIMAGE_READER);

         */

        //iterating through a grouped dataset
        /*
        for (final Entry<String, VFSListDataset<FImage>> entry : groupedFaces.entrySet()) {
	        DisplayUtilities.display(entry.getKey(), entry.getValue());
        }
         */

        //creating a dataset of images using the flickr api
        /*
        FlickrAPIToken flickrToken = DefaultTokenFactory.get(FlickrAPIToken.class);
        FlickrImageDataset<FImage> cats =
		    FlickrImageDataset.create(ImageUtilities.FIMAGE_READER, flickrToken, "cat", 10);
        DisplayUtilities.display("Cats", cats);
         */


        //exercise 1
        VFSGroupDataset<FImage> groupedFaces =
                new VFSGroupDataset<FImage>( "zip:http://datasets.openimaj.org/att_faces.zip", ImageUtilities.FIMAGE_READER);

        List<FImage> faces = new ArrayList<>();
        for (final Map.Entry<String, VFSListDataset<FImage>> entry : groupedFaces.entrySet()) {
            faces.add(entry.getValue().getRandomInstance());
        }
        DisplayUtilities.display("faces", faces);


        //exercise 2
        //https://commons.apache.org/proper/commons-vfs/filesystems.html
        //above is a link to a list of supported file systems for common vfs


        //exercise 3
        BingAPIToken bingToken = DefaultTokenFactory.get(BingAPIToken.class);
        BingImageDataset<FImage> dogs =
                BingImageDataset.create(ImageUtilities.FIMAGE_READER, bingToken, "dog", 10);
        DisplayUtilities.display("Dogs", dogs);


        //exercise 4
        BingImageDataset<FImage> jDepp =
                BingImageDataset.create(ImageUtilities.FIMAGE_READER, bingToken, "johnny depp", 3);
        BingImageDataset<FImage> mNixon =
                BingImageDataset.create(ImageUtilities.FIMAGE_READER, bingToken, "mark nixon", 3);
        BingImageDataset<FImage> lDicaprio =
                BingImageDataset.create(ImageUtilities.FIMAGE_READER, bingToken, "leonardo dicaprio", 3);

        MapBackedDataset handsomeMen = MapBackedDataset.of(jDepp, mNixon, lDicaprio);
    }
}
