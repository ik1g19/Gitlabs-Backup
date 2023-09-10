public class ATM {
	//creating the toolbox
	Toolbox myToolbox = new Toolbox();

	//creating a variable to hold the balance
	Integer balance;

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

		Integer withdrawal;

		//creates a variable to hold how much the user wants to withdraw
		withdrawal = myToolbox.readIntegerFromCmd();

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

		Integer toDeposit;

		//creates a variable to hold how much the user wants to deposit
		toDeposit = myToolbox.readIntegerFromCmd();

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

		//sets the balance to the user input
		balance = myToolbox.readIntegerFromCmd();

		//outputs the users balance
		System.out.println("Your balance is "+balance);

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