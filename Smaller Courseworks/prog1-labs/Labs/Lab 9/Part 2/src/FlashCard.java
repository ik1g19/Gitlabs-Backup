//the class that represents a flash card
public class FlashCard {
    //member variables
    private String question;
    private String answer;


    //constructor
    public FlashCard(String question, String answer) {
        this.question = question;
        this.answer = answer;
    }


    //getters
    public String getQuestion() {
        return question;
    }
    public String getAnswer() {
        return answer;
    }
}
