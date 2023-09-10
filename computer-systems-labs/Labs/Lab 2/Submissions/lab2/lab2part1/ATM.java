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

	public void go(){
		//prints a welcome message
		System.out.println("Welcome to online ATM banking");
		System.out.println("How much do you want in your account?");

		//sets the balance to the user input
		balance = myToolbox.readIntegerFromCmd();

		//outputs the users balance
		System.out.println(balance);
	}
}