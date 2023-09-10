import java.util.ArrayList;

//the class that represents the quiz
public class Quiz {


    //member variables
    private ArrayList<FlashCard> flashCards;


    //constructor
    public Quiz(String filepath) {
        FlashCardReader flashCardReader = new FlashCardReader(filepath);

        //populating the array list of flashcards
        flashCards = flashCardReader.getFlashCards();

        System.out.println(flashCards.size());

        //plays the quiz
        play();
    }


    //the main method
    public static void main(String args[]) {
        Quiz quiz = new Quiz("Questions.txt");
    }


    //plays the quiz game
    public void play() {

        //creates a new toolbox object for input
        Toolbox toolbox = new Toolbox();

        //loops through each flashcard
        int questionNumber = 0;
        for (FlashCard flashCard : flashCards) {

            questionNumber++;

            //outputs the question
            System.out.println("Question Number " + questionNumber + ": \n" + flashCard.getQuestion());

            //reads user input
            String answer = toolbox.readStringFromCmd();

            //if the user is correct
            if (answer.equals(flashCard.getAnswer())) {
                System.out.println("right");
            }

            //if the user is wrong
            else {
                System.out.println("wrong \nThe correct answer was: " + flashCard.getAnswer());
            }

        }

    }

}
