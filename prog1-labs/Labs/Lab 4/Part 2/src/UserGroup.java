import java.util.ArrayList;

//the class UserGroup contains an array list to hold users,
//and methods to populate the array and output its contents
public class UserGroup {
    //creates an array list that can only hold objects of the type User
    private ArrayList<User> users;

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
    }
}
