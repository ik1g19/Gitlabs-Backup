package uk.ac.soton.ecs.ik1g19.hybridimages;

import org.openimaj.image.DisplayUtilities;
import org.openimaj.image.FImage;
import org.openimaj.image.processor.SinglebandImageProcessor;

public class MyConvolution implements SinglebandImageProcessor<Float, FImage> {

    //templated to be used during convolution
    private float[][] kernel;



    public MyConvolution(float[][] kernel) {
        this.kernel = kernel;
    }


    /**
     * @desc Perform template convolution on given image
     * @param image Image to be transformed
     */
    @Override
    public void processImage(FImage image) {
        //temp image for writing values to during convolution
        FImage tmpImg = image.clone();

        //border size on left and right is half kernel width
        int bLR = (int) Math.floor(kernel[0].length/2);
        //border size on top and bottom is half kernel height
        int bTB = (int) Math.floor(kernel.length/2);

        //image width
        int imgW = image.getWidth();
        //image height
        int imgH = image.getHeight();

        //kernel width
        int kW = kernel[0].length;
        //kernel height
        int kH = kernel.length;


        //convolve template lol
        for (int x = 0 - bLR; x < imgW - 1 - bLR; x++) { //traverse columns, start from zero padding
            for (int y = 0 - bTB; y < imgH - 1 - bTB; y++) { //traverse rows, start from zero padding
                float sum = 0f;
                for (int kX = 0; kX < kW; kX++) { //all points in the kernel
                    for (int kY = 0; kY < kH; kY++) {
                        int cX = x + kX;
                        int cY = y + kY;

                        //implicit zero-padding
                        boolean outbounds = (cX < 0) || (cX > imgW - 1) || (cY < 0) || (cY > imgH - 1);

                        //zero padding will not affect sum
                        if (!outbounds) {
                            sum += image.getPixel(cX, cY) * //pixel value...
                                    kernel[kH - kY - 1][kW - kX - 1]; //multiplied with kernel value
                        }
                    }
                }
                tmpImg.setPixel(x+bLR,y+bTB,sum); //store as new point
            }
        }

        //replace original image with convolved image
        image.internalAssign(tmpImg);
    }

}