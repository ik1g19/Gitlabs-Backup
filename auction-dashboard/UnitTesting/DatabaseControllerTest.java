package UnitTesting;

import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import src.Database;
import src.DatabaseController;

import java.io.*;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class DatabaseControllerTest {

    private static final String PATH = "./test.db";
    private static DatabaseController db_ctrl;
    private static Connection conn;

    private static final String IMP_FILE = "./TestingFiles/dbImpressionLogTestFile.csv";
    private static final String CLICK_FILE = "./TestingFiles/dbClickLogTestFile.csv";
    private static final String SERVER_FILE = "./TestingFiles/dbServerLogTestFile.csv";
    private static boolean impStore = false;
    private static boolean clickStore = false;
    private static boolean serverStore = false;

    @BeforeAll
    static void setUp() {
        // creates new database & controller for the database
        Database db = new Database(PATH);
        db_ctrl = new DatabaseController(db);
        conn = db.getConnection();
    }

    @AfterAll
    static void tearDown() throws SQLException {
        // closes the db connection
        conn.close();
        // deletes the database that was setup for the test
        File file = new File(PATH);
        boolean deleteSuccess = file.delete();
        if (!deleteSuccess) {
            System.out.println("The test database file failed to delete.");
        }
        // resets the database controller & database
        db_ctrl = null;
        conn = null;

        impStore = false;
        clickStore = false;
        serverStore = false;
    }

    @Test
    @Order(1)
    void storeImpressionLog() {
        impStore = db_ctrl.storeImpressionLog(IMP_FILE);
        assertTrue(impStore);

    }

    @Test
    @Order(2)
    void storeClickLog() {
        clickStore = db_ctrl.storeClickLog(CLICK_FILE);
        assertTrue(clickStore);

    }

    @Test
    @Order(3)
    void storeServerLog() {
        serverStore = db_ctrl.storeServerLog(SERVER_FILE);
        assertTrue(serverStore);

    }

    @Test
    @Order(4)
    void queryImpressionLog() {
        int missMatches = dbInsertAndQueryTest(IMP_FILE, impStore, "SELECT user_id, date, gender, age, income, context, impression_cost FROM impression_log",7);
        assertEquals(0, missMatches);
    }

    @Test
    @Order(5)
    void queryClickLog() {
        int missMatches = dbInsertAndQueryTest(CLICK_FILE, clickStore, "SELECT user_id, date, click_cost FROM click_log",3);
        assertEquals(0, missMatches);
    }

    @Test
    @Order(6)
    void queryServerLog() {
        int missMatches = dbInsertAndQueryTest(SERVER_FILE, serverStore, "SELECT user_id, entry_date, exit_date, viewed, conversion FROM server_log", 5);
        assertEquals(0, missMatches);}

    /*
    Function to test inserting into the DB Impression_Log Table & then querying the data
    @param db_ctrl - the controller for the Database
     */
    public int dbInsertAndQueryTest(String fileName, boolean storeAttempt, String query, int columnCount) {
        try {
            // attempts to insert the data in the click_log table

            if (storeAttempt) {
                // the result from the query, selecting all the data in the table (excluding the auto incrementing id column)
                List<String[]> queryResult = db_ctrl.dbQuery(query, columnCount);

                BufferedReader br = null;
                try {
                    br = new BufferedReader(new FileReader(fileName));
                    br.readLine(); // Discard headings
                } catch (FileNotFoundException e1) {
                    e1.printStackTrace();
                } catch (IOException e2) {
                    e2.printStackTrace();
                }

                int dataMismatches = 0;

                // looping for all of the records found in the query one at a time
                for (int i = 0; i < queryResult.size(); i++) {
                    String[] queryData = queryResult.get(i);

                    // checks that all of the data returned from the query is the same as the data in the input file
                    // this is done by counting the number of data mismatches
                    String[] expected = br.readLine().split(",");
                    dataMismatches = expected[0].equals(queryData[1]) ? dataMismatches++ : dataMismatches;
                    dataMismatches = expected[1].equals(queryData[0]) ? dataMismatches++ : dataMismatches;
                    for (int j = 2; j < columnCount; j++) {
                        dataMismatches = expected[j].equals(queryData[j]) ? dataMismatches++ : dataMismatches;
                    }
                    return dataMismatches;
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
        return -1;
    }
}