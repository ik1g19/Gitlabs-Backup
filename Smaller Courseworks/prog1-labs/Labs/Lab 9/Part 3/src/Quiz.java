import java.io.PrintStream;
import java.util.ArrayList;

//the class that represents the quiz
public class Quiz {


    //member variables
    private PrintStream printStream;
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

        //asking if the user wishes to save their answers
        Boolean saveAnswers;
        System.out.println("Would you like to save your results? (Y/N)");
        String response = toolbox.readStringFromCmd();
        if (response.equals("Y"))
            saveAnswers = true;
        else
            saveAnswers = false;

        //initialising variables to store users results
        int numberOfQuestions = flashCards.size();
        int amountCorrect = 0;
        String[] answers = new String[numberOfQuestions];

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
                amountCorrect++;
                //stores users answer at appropriate index
                answers[questionNumber-1] = answer + "," + "right";
            }
            //if the user is wrong
            else {
                System.out.println("wrong \nThe correct answer was: " + flashCard.getAnswer());
                //stores users answer at appropriate index
                answers[questionNumber-1] = answer + "," + "wrong";
            }

        }
        System.out.println(saveAnswers);
        //if the user choose to save their answers write them to the file
        if (saveAnswers)
            save(amountCorrect, numberOfQuestions, answers);

    }


    public void save(int score, int numberOfQuestions, String[] answers) {

        //creating print stream to write to the file
        try {
            printStream = new PrintStream("save.txt");
        }
        catch (Exception e) {
            System.err.println("Failed to create PrintStream");
            System.err.println(e);
        }

        //writing each answer and result to the file
        for (int i = 0; i < numberOfQuestions; i++) {
            printStream.println(flashCards.get(i).getQuestion() +"," + answers[i]);
        }

        //calculating and outputting the users result
        float percentage = ((float)score / numberOfQuestions) * 100;
        String result = score + "," + numberOfQuestions + "," + percentage;
        printStream.println(result);

    }


}
