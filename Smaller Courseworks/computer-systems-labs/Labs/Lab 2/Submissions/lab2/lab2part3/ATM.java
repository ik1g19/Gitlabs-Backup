public class ATM {
	//creating the toolbox
	Toolbox myToolbox = new Toolbox();

	//creating a variable to hold the balance, negative initally so while loop will run
	Integer balance = -1;

	//the main method creates a new ATM object and calls the go method
	public static void main(String[] args){
		ATM myATM = new ATM();
		myATM.go();
	}

	public void withdraw(){
		//prints a welcome message for the withdraw method
		System.out.println("******************************************");
		System.out.println("		 Withdrawal");
		System.out.println("******************************************");
		System.out.println("How much would you like to withdraw?");
		
		//initialises withdrawal as negative so the loop will start
		Integer withdrawal = -1;

		//while loop will run until the user enters a valid withdrawal
		while (withdrawal < 0 || ((balance - withdrawal) < 0) ) {

			//creates a variable to hold how much the user wants to withdraw
			withdrawal = myToolbox.readIntegerFromCmd();

			//informs the user if they enter an invalid value
			if (withdrawal < 0) {
				System.out.println("Negative withdrawals are not allowed");
			}
			if (balance - withdrawal < 0) {
				System.out.print("Negative balances are not allowed");
			}

		}

		//decreases the users balance by the amount they specified
		balance = balance - withdrawal;

		//prints the users new balance
		System.out.println("******************************************");
		System.out.println("	  Your new balance is "+balance);
		System.out.println("******************************************");
	}

	public void deposit(){
		//prints a welcome message for the deposit methid
		System.out.println("******************************************");
		System.out.println("		Deposit");
		System.out.println("******************************************");
		System.out.println("How much would you like to deposit?");

		//initialises the value to deposit as negative so the loop will start
		Integer toDeposit = -1;

		//continually asks the user for values to deposit until they enter a valid value
		while (toDeposit < 0) {

			//creates a variable to hold how much the user wants to deposit
			toDeposit = myToolbox.readIntegerFromCmd();

			//informs the user if they enter an invalid value
			if (toDeposit < 0) {
				System.out.println("Negative deposits are not allowed");
			}

		}

		//increases the users balance by the amount they specified
		balance = balance + toDeposit;

		//prints the users new balance
		System.out.println("******************************************");
		System.out.println("	  Your new balance is "+balance);
		System.out.println("******************************************");
	}

	public void inquire(){
		//prints the users current balance
		System.out.println("******************************************");
		System.out.println("	   Your balance is "+balance);
		System.out.println("******************************************");
	}

	public void quit(){
		//prints a goodbye message
		System.out.println("******************************************");
		System.out.println("         	 GoodBye!");
		System.out.println("******************************************");

		//quit the program
		System.exit(0);
	}

	public void go(){
		//prints a welcome message
		System.out.println("Welcome to online ATM banking");
		System.out.println("How much do you want in your account?");

		//loop will run until the user enters a valid balance
		while (balance < 0) {

			//sets the balance to the user input
			balance = myToolbox.readIntegerFromCmd();

		}

		//outputs the users balance
		System.out.println("Your balance is "+balance);

		//runs the main loop until the user chooses to quit
		while (true) {

			//prompts the user to choose an option
			System.out.println("What do you want to do?");
			System.out.println("1 : Withdraw");
			System.out.println("2 : Deposit");
			System.out.println("3 : Inquire");
			System.out.println("4 : Quit");

			//stores the users choice in a variable
			Integer choice = myToolbox.readIntegerFromCmd();

			//if the user chose the first option then withdraw
			if (choice.equals(1)) {
				withdraw();
			}
			//if the user chose the second option then deposit
			else if (choice.equals(2)) {
				deposit();
			}
			//if the user chose the third option then display their current balance
			else if (choice.equals(3)) {
				inquire();
			}
			//if the user chose the fourth option then quit
			else if (choice.equals(4)) {
				quit();
			}

		}

	}
}