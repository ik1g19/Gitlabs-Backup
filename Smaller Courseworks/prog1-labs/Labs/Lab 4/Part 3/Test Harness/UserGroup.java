import java.util.ArrayList;
import java.util.Iterator;

//the class UserGroup contains an array list to hold users
//and methods to manipulate the array lists contents,
//as well as an iterator for the array list
public class UserGroup {
    //creates an array list that can only hold objects of the type User
    private ArrayList<User> users;

    //creates an iterator that can iterate through User objects
    private Iterator<User> userIt;

    //initialises the array list "users"
    public UserGroup() {
        users = new ArrayList<User>();
    }

    //returns the array list "users"
    public ArrayList<User> getUsers() {
        return users;
    }

    //populates the array list "users" with 10 pieces of sample data
    public void addSampleData() {
        users.add(new User("idk","user","isaac"));
        users.add(new User("sad","editor","tom"));
        users.add(new User("vsd","admin","alice"));
        users.add(new User("d21","user","henry"));
        users.add(new User("4fe","editor","ben"));
        users.add(new User("dw2","admin","adam"));
        users.add(new User("5tg","user","max"));
        users.add(new User("sr6","editor","matt"));
        users.add(new User("hy3","admin","kirk"));
        users.add(new User("dw1","user","meghan"));
    }

    //returns the user at a given index
    public User getUser(int index) {
        return users.get(index);
    }

    //outputs the username and user type of each user in the array list
    public void printUsernames() {
        for (User user : users) {
            System.out.println(user.getUsername() + " " + user.getUserType());
        }
        System.out.println("");
    }

    //removes the first user from the array list
    public void removeFirstUser() {
        users.remove(0);
    }

    //removes the last user from the array list
    public void removeLastUser() {
        users.remove(users.size() - 1 );
    }

    //removes a specific user from the array list, given their username
    public void removeUser(String username) {
        //creating a new instance of the iterator
        userIt = users.iterator();

        //creating a list of users to be removed
        ArrayList<User> found = new ArrayList<User>();

        //iterates through array list using an iterator
        while (userIt.hasNext()) {
            User currentUser = userIt.next();

            //if the current user has the username of the given parameter
            //it will be added to the list of users to be removed
            if (currentUser.getUsername().equals(username))
                found.add(currentUser);
        }

        //removes all users from the list created during the loop
        users.removeAll(found);
    }

    //initialises the iterator to the array list and returns the iterator
    public Iterator<User> getUserIterator() {
        userIt = users.iterator();
        return userIt;
    }
}