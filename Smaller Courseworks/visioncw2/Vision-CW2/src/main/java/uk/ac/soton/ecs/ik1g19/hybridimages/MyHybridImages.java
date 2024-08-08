package uk.ac.soton.ecs.ik1g19.hybridimages;

import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.MBFImage;
import org.openimaj.image.processing.convolution.Gaussian2D;


/**
 * @desc Combines low and high frequencies of two images to form a hybrid image
 */
public class MyHybridImages {
    /**
     * @desc Compute a hybrid image combining low-pass and high-pass filtered images
     *
     * @param lowImage
     *            the image to which apply the low pass filter
     * @param lowSigma
     *            the standard deviation of the low-pass filter
     * @param highImage
     *            the image to which apply the high pass filter
     * @param highSigma
     *            the standard deviation of the low-pass component of computing the
     *            high-pass filtered image
     *
     * @return the computed hybrid image
     */
    public static MBFImage makeHybrid(MBFImage lowImage, float lowSigma, MBFImage highImage, float highSigma) {
        //kernels for gaussian averaging
        float[][] lowGausK = Gaussian2D.createKernelImage(gSigmaToSize(lowSigma), lowSigma).pixels;
        float[][] highGausK = Gaussian2D.createKernelImage(gSigmaToSize(highSigma), highSigma).pixels;

        //convolution operators for gaussian averaging
        MyConvolution gAvgLow = new MyConvolution(lowGausK);
        MyConvolution gAvgHigh = new MyConvolution(highGausK);

        //lowpass on first image
        lowImage.processInplace(gAvgLow);

        //high pass on second image
        MBFImage tmpHigh = highImage.process(gAvgHigh); //lowpass...
        highImage.subtractInplace(tmpHigh); //subtracted from original image

        //combine frequencies
        return highImage.add(lowImage);
    }


    /**
     * @desc Helper function, find kernel size for given sigma
     * @param sigma Given sigma value
     * @return Appropriate kernel size
     */
    private static int gSigmaToSize(float sigma) {int size = (int)(8.0f*sigma+1.0f); return size%2==0 ? size++ : size;}
}