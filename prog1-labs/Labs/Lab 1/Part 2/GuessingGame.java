public class GuessingGame
{
	public static void main(String[] args){
		
		//declaring the number to guess and the guessed number
		Integer numberToGuess;
		Integer guessedNumber;
		Toolbox myToolbox = new Toolbox();

		//welcoming the user, generating a random number and reading user input
		System.out.println("Welcome to the number guessing game!");
		numberToGuess = myToolbox.getRandomInteger(10);
		guessedNumber = myToolbox.readIntegerFromCmd();

		//checking if the users guess is correct
		if (guessedNumber > numberToGuess) {
			System.out.println("too high");
		}
		else if (guessedNumber < numberToGuess) {
			System.out.println("too low");
		}
		else if (numberToGuess.equals(guessedNumber)) {
			System.out.println("right");
		}
	}
}