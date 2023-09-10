import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

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
            System.out.println("Failed to create buffered reader");
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


    //returns the array list of flash cards
    public ArrayList<FlashCard> getFlashCards() {

        ArrayList<FlashCard> flashCards = new ArrayList<FlashCard>();

        String line = getLine();
        String[] questionAndAnswer;

        //loops through each line of the file and splits each line into a question and answer
        while (line != null) {
            questionAndAnswer = line.split(":");

            String question = questionAndAnswer[0];
            String answer = questionAndAnswer[1];

            flashCards.add(new FlashCard(question, answer));
            line = getLine();
        }

        return flashCards;

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
        else {
            return false;
        }

    }


}
