import java.io.BufferedReader;
import java.io.FileReader;

public class FlashCardReader {
    //member variables
    private BufferedReader reader;


    public FlashCardReader(String filename) {
        //creates a new buffered reader object using the given filepath
        try {
            reader = new BufferedReader(new FileReader(filename));
        }
        catch (Exception e) {
            System.out.println(e);
        }
    }


    //returns the next line of the file
    public String getLine() {
        String line = null;

        try {
            line = reader.readLine();
        }
        catch (Exception e) {
            System.err.println("SHE SHOULD'VE DIED");
        }

        return line;
    }

    public Boolean fileIsReady() {
        Boolean isReady = false;

        try {
            isReady = reader.ready();
        }
        catch (Exception e) {
            System.err.println("87 YEARS AGO");
        }

        if (isReady && reader != null) {
            return true;
        }
        else{
            return false;
        }
    }
}
