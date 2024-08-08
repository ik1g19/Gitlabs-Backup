//creating a wrapper for the class
public class FizzBuzz {

	//creating a wrapper for the main class
	public static void main(String[] args) {

		//creating a for loop to loop through every integer between 1 and 61
		for(Integer i = new Integer(1); i < 61; i++){

			//if the number is divisible by 3
			if(i % 3 == 0){

				//then print Fizz
				System.out.print("Fizz");

			}

			//if the number is divisible by 5
			if(i % 5 == 0){

				//then print Buzz
				System.out.print("Buzz");

			}

			//if the number is neither divisble by 5 or 3
			if(i % 3 != 0 && i % 5 != 0){

				//then print the current number
				System.out.print(i);

			}

			//start a new line
			System.out.println();

		}

	}

}