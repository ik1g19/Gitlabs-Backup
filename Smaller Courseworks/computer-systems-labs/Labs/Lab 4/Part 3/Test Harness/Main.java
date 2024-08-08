import java.util.Iterator;

public class Main {
    public static void main(String[] args) {
        UserGroup groupA = new UserGroup();

        groupA.addSampleData();

        groupA.printUsernames();

        UserGroup administrators = new UserGroup();

        Iterator<User> aIt = groupA.getUserIterator();

        while (aIt.hasNext()) {
            User currentUser = aIt.next();

            if (currentUser.getUserType() == "admin")
                administrators.getUsers().add(currentUser);
        }

        administrators.printUsernames();

        administrators.getUser(administrators.getUsers().size()).setUserType("user");

        groupA.printUsernames();

        administrators.printUsernames();
    }
}