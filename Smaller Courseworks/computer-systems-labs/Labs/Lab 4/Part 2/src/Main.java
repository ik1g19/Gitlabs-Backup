//the Main class creates a new user group,
//populates it with sample data
//and outputs the the group
public class Main {
    public static void main(String[] args) {
        //creating a new group
        UserGroup groupA = new UserGroup();

        //populating with sample data
        groupA.addSampleData();

        //outputting users in the new group
        groupA.printUsernames();
    }
}
