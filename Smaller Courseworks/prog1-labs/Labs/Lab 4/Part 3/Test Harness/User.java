public class User {
    private String username;
    private String userType;
    private String name;

    public User(String username, String userType, String name) {
        this.username = username;
        this.userType = userType;
        this.name = name;
    }

    public String getUsername() {
        return username;
    }

    public String getUserType() {
        return userType;
    }

    public String getName() {
        return name;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }
}