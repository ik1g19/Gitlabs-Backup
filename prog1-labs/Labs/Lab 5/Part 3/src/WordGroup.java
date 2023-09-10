import java.util.HashMap;
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

        //populating the set with words from the array
        for (int i = 0; i < wordsArray.length; i++) {
            set.add(wordsArray[i]);
        }

        //populating the set with words from the other group
        for (int i = 0; i < tempWordGroup.getWordArray().length; i++) {
            set.add(tempWordGroup.getWordArray()[i]);
        }

        //returning the now populated set
        return set;
    }

    //counts the number of occurrences of each word in the array
    //and stores the result as a hashmap
    public HashMap<String, Integer> getWordCounts() {
        //creating a new hashmap
        HashMap<String, Integer> wordCounts = new HashMap<String, Integer>();

        //creating an array to hold the list of words
        String[] wordsArray = getWordArray();

        //loops through every word in the array
        for (String word : wordsArray) {
            //if the word has already been counted,
            //then increase the number of occurrences by one
            if (wordCounts.containsKey(word)) {
                wordCounts.replace(word, wordCounts.get(word) + 1);
            }
            //otherwise if the word has not yet been counted,
            //add it to the hashmap
            else {
                wordCounts.put(word, 1);
            }
        }

        //return the hashmap
        return wordCounts;
    }
}