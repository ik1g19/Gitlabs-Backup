//the class User is designed to hold information about different users
public class User {
    //creates a variable to hold the users username
    private String username;

    //creates a variable to hold the users type
    private String userType;

    //creates a variable to hold the users name
    private String name;

    //the constructor initialises the users username, type and name
    public User(String username, String userType, String name) {
        this.username = username;
        this.userType = userType;
        this.name = name;
    }

    //returns the users username
    public String getUsername() {
        return username;
    }

    //returns the users type
    public String getUserType() {
        return userType;
    }

    //returns the users name
    public String getName() {
        return name;
    }

    //sets the users type to the given value
    public void setUserType(String userType) {
        this.userType = userType;
    }
}
