import java.util.Iterator;

//the main class contains the main method
//which creates two new groups and manipulates them
public class Main {
    public static void main(String[] args) {
        //creates a new user group
        UserGroup groupA = new UserGroup();

        //populating with sample data
        groupA.addSampleData();

        //outputting users in the new group
        groupA.printUsernames();

        //creates another user group
        UserGroup administrators = new UserGroup();

        //creates a new iterator, which points to the iterator
        //from group A's user group
        Iterator<User> aIt = groupA.getUserIterator();

        //loops through the iterator and adds any users
        //with the user type of admin to the administrator group
        while (aIt.hasNext()) {
            //creates a User object that points to the current user
            User currentUser = aIt.next();

            //if the current user has a user type of admin
            //then add them to the administrator group
            if (currentUser.getUserType() == "admin")
                administrators.getUsers().add(currentUser);
        }

        //outputs the users in the administrator user group
        administrators.printUsernames();

        //changes the user type of the last user in administrators to user
        administrators.getUser(administrators.getUsers().size() - 1).setUserType("user");

        //outputs the users in group As user group
        groupA.printUsernames();

        //outputs the users in the administrator user group
        administrators.printUsernames();
    }
}