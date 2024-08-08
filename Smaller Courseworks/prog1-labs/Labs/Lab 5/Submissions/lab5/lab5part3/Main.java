import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

//the main method creates two word groups
//separates each word, stores them in an array
//and outputs them using a for loop
//it also creates a set of words from both group
//and outputs it
public class Main {
    public static void main (String[] args) {
        //creating two new WordGroup objects and initialising them with the test strings
        WordGroup plato = new WordGroup("You can discover more about a person in an hour of play than in a year of conversation");
        WordGroup roosevelt = new WordGroup("When you play play hard when you work dont play at all");

        //creating two new string arrays to hold the split up test strings
        String[] platoArray = plato.getWordArray();
        String[] rooseveltArray = roosevelt.getWordArray();

        //looping through the array containing the plato quote
        for (int i = 0; i < platoArray.length; i++) {
            System.out.println(platoArray[i]);
        }

        //looping through the array containing the roosevelt quote
        for (int i = 0; i < rooseveltArray.length; i++) {
            System.out.println(rooseveltArray[i]);
        }

        //creating a new set which is a combination of words
        //from the two groups
        HashSet<String> combinedSet = plato.getWordSet(roosevelt);

        //creating an iterator to look through the new set
        Iterator<String> setIt = combinedSet.iterator();

        //outputting each element in the set
        while (setIt.hasNext()) {
            String currentWord = setIt.next();

            System.out.println(currentWord);
        }

        //creating hashmaps to store the occurences of each word
        //in the plato quote and the roosevelt quote
        HashMap<String, Integer> platoWordCount = plato.getWordCounts();
        HashMap<String, Integer> rooseveltWordCount = roosevelt.getWordCounts();

        //creating sets of the keys from the plato hashmap
        //and the roosevelt hashmap
        Set<String> platoSet = platoWordCount.keySet();
        Set<String> rooseveltSet = rooseveltWordCount.keySet();

        //for each element in the plato set out the key
        //and its corresponding value
        for (String key : platoSet) {
            System.out.println(key + ": " + platoWordCount.get(key));
        }

        //for each element in the roosevelt set out the key
        //and its corresponding value
        for (String key : rooseveltSet) {
            System.out.println(key + ": " + rooseveltWordCount.get(key));
        }

        //counts the combined occurrences of each word
        //from both the plato set and the roosevelt set
        for (String key : combinedSet) {
            //initialise the total to 0
            int totalCount = 0;

            //if the word is in the plato set count its occurrences
            if (platoWordCount.containsKey(key))
                totalCount += platoWordCount.get(key);
            //if the word is in the roosevelt set count its occurrences
            if (rooseveltWordCount.containsKey(key))
                totalCount += rooseveltWordCount.get(key);

            //output the total occurrences of each word
            System.out.println(key + ": " + totalCount);
        }

    }
}