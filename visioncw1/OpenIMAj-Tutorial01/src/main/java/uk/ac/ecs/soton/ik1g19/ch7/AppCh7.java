package uk.ac.ecs.soton.ik1g19.ch7;


import org.openimaj.image.MBFImage;
import org.openimaj.image.processing.edges.CannyEdgeDetector;
import org.openimaj.video.Video;
import org.openimaj.video.VideoDisplay;
import org.openimaj.video.VideoDisplayListener;
import org.openimaj.video.capture.VideoCapture;
import org.openimaj.video.capture.VideoCaptureException;

/**
 * OpenIMAJ Hello world!
 *
 */
public class AppCh7 {
    public static void main( String[] args ) throws VideoCaptureException {
    	Video<MBFImage> video;
        //video = new XuggleVideo(new File("C:\\Users\\isaac\\Documents\\repos\\visioncw1\\OpenIMAj-Tutorial01\\media\\keyboardcat.flv"));
        video = new VideoCapture(320, 240);
        VideoDisplay<MBFImage> display = VideoDisplay.createVideoDisplay(video);
        display.addVideoListener(
                new VideoDisplayListener<MBFImage>() {
                    public void beforeUpdate(MBFImage frame) {
                        frame.processInplace(new CannyEdgeDetector());

                        //exercise 1
                        //setting green and blue bands to black
                        //frame.getBand(1).fill(0f);
                        //frame.getBand(2).fill(0f);
                    }

                    public void afterUpdate(VideoDisplay<MBFImage> display) {
                    }
                });
    }
}
