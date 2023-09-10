import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.PalindromeChecker;

//determines if a string is a k-palindrome
public class KPalindrome implements PalindromeChecker {
    //returns true if a string is a k-palindrome
    public boolean isKPalindrome (String str, int k) {
        //a string is always a k palindrome if it has less than 2 characters
        if (str.length() < 2) {
            return true;
        }

        //if the first character is the same as the character at the end of the string
        if (str.charAt(0) == str.charAt(str.length()-1)) {
            //then check if also true for the second and penultimate character
            return isKPalindrome(str.substring(1, str.length()-1), k);
        }

        else {
            //otherwise if checking for a normal palindrome then return false
            if (k == 0) {
                return false;
            }

            else {
                //otherwise remove a character from the end and check if the new string is a k-1-palindrome
                if (isKPalindrome(str.substring(0, str.length() - 1), k-1)) {
                    //if so then return true
                    return true;
                }

                else {
                    //otherwise remove a character from the start and check if the new string is a k-1-palindrome
                    return isKPalindrome(str.substring(1, str.length()), k-1);
                }
            }
        }
    }
}
