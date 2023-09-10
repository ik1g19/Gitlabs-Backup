import java.util.HashSet;

//holds a string, creates an array of its separated words
//and a set of a combination of its words and another word groups
public class WordGroup {
    //creates a string to hold the string passed to the parameter
    private String words;

    //constructor takes a string as a parameter and converts it to lowercase
    //before storing in a string
    public WordGroup(String inputStr) {
        words = inputStr.toLowerCase();
    }

    //separates the string passed to the constructor by its whitespaces
    //and returns the result as an array of strings
    public String[] getWordArray() {
        return words.split(" ");
    }

    //returns the set which ia a combination of the words of this object
    //and the words of the object passed as a parameter
    public HashSet getWordSet(WordGroup tempWordGroup) {
        //creating a new set of strings
        HashSet<String> set = new HashSet<String>();

        //creating an array of words
        String[] wordsArray = getWordArray();

        System.out.println();

        for (int i = 0; i < wordsArray.length; i++) {
            set.add(wordsArray[i]);
        }

        for (int i = 0; i < tempWordGroup.getWordArray().length; i++) {
            set.add(tempWordGroup.getWordArray()[i]);
        }

        return set;
    }
}