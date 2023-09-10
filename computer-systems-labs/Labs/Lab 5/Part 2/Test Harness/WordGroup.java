import java.util.HashSet;

//holds a string and returns an array of its separated words
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

    public HashSet getWordSet(WordGroup tempWordGroup) {
        HashSet<String> set = new HashSet<String>();

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
