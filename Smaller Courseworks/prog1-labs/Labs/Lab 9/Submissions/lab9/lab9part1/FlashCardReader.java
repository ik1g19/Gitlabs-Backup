import java.io.BufferedReader;
import java.io.FileReader;

//reads the information from a flash card file
public class FlashCardReader {
    //member variables
    private BufferedReader reader;


    //constructor
    public FlashCardReader(String filename) {
        //creates a new buffered reader for the given filepath
        try {
            reader = new BufferedReader(new FileReader(filename));
        }
        catch (Exception e) {
            System.out.println(e);
        }
    }


    //returns the next lines of the file
    public String getLine() {
        String line = null;

        try {
            line = reader.readLine();
        }
        catch (Exception e) {
            System.err.println(e);
        }

        return line;
    }


    //returns true if the file is ready to be read
    //otherwise false
    public Boolean fileIsReady() {
        Boolean isReady = false;

        try {
            isReady = reader.ready();
        }
        catch (Exception e) {
            System.err.println(e);
        }

        if (isReady && reader != null) {
            return true;
        }
        else{
            return false;
        }
    }
}
