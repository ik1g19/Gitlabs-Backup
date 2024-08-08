package UnitTesting;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import src.Database;
import src.DatabaseController;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class FileInputTests {

    Database db;
    DatabaseController db_ctrl;

    @BeforeEach
    void setUp() {
        db = new Database("./test.db");
        db_ctrl = new DatabaseController(db);
    }

    @AfterEach
    void tearDown() throws SQLException {
        File dbFile = new File ("./test.db");
        dbFile.delete();
        db.getConnection().close();
        db = null;
        db_ctrl = null;
    }

    @Test
    @DisplayName("Server Log Error")
    public void serverLogInputErrorTest() {
        String filePath = "./TestingFiles/dbClickLogTestFile.csv";
        db_ctrl.storeServerLog(filePath);
        assertTrue(db_ctrl.getServerError());
    }

    @Test
    @DisplayName("Click Log Error")
    public void clickLogInputErrorTest() {
        String filePath = "./TestingFiles/dbServerLogTestFile.csv";
        db_ctrl.storeClickLog(filePath);
        assertTrue(db_ctrl.getClickError());
    }

    @Test
    @DisplayName("Impression Log Error")
    public void impressionLogInputErrorTest() {
        String filePath = "./TestingFiles/dbServerLogTestFile.csv";
        db_ctrl.storeImpressionLog(filePath);
        assertTrue(db_ctrl.getImpressionError());
    }

    @Test
    @DisplayName("Server Log Saving")
    public void serverLogSavesToDatabaseTest() {
        String filePath = "./TestingFiles/dbServerLogTestFile.csv";
        db_ctrl.storeServerLog(filePath);
        boolean isSame = true;
        List<String[]> result = db_ctrl.dbQuery("SELECT user_id, entry_date, exit_date, viewed, conversion FROM server_log", 5);

        try {
            BufferedReader br = new BufferedReader(new FileReader(filePath));
            br.readLine(); // Remove headings
            String line = "";
            int queryIndex = 0;

            while ((line = br.readLine()) != null) {

                // File values
                String[] entry = line.split(",");
                String user_id = entry[1];
                String entry_date = entry[0];
                String exit_date = entry[2];
                String viewed  = entry[3];
                String conversion = entry[4];

                // Database values
                String[] record = result.get(queryIndex);
                String user_idR = record[0];
                String entry_dateR = record[1];
                String exit_dateR = record[2];
                String viewedR  = record[3];
                String conversionR = record[4];

                isSame = (
                            user_id.equals(user_idR) &&
                            entry_date.equals(entry_dateR) &&
                            exit_date.equals(exit_dateR) &&
                            viewed.equals(viewedR) &&
                            conversion.equals(fromBool(Integer.valueOf(conversionR)))
                );

                queryIndex++;
            }

            assertTrue(isSame);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Test
    @DisplayName("Click Log Saving")
    public void clickLogSavesToDatabaseTest() {
        String filePath = "./TestingFiles/dbClickLogTestFile.csv";
        db_ctrl.storeClickLog(filePath);
        boolean isSame = true;
        List<String[]> result = db_ctrl.dbQuery("SELECT user_id, date, click_cost FROM click_log", 3);

        try {
            BufferedReader br = new BufferedReader(new FileReader(filePath));
            br.readLine(); // Remove headings
            String line = "";
            int queryIndex = 0;

            while ((line = br.readLine()) != null) {

                // File values
                String[] entry = line.split(",");
                String user_id = entry[1];
                String date = entry[0];
                String click_cost = entry[2];

                // Database values
                String[] record = result.get(queryIndex);
                String user_idR = record[0];
                String dateR = record[1];
                String click_costR = record[2];

                isSame = (
                                user_id.equals(user_idR) &&
                                date.equals(dateR) &&
                                click_cost.equals(click_costR)
                );

                queryIndex++;
            }

            assertTrue(isSame);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Test
    @DisplayName("Impression Log Saving")
    public void impressionLogSavesToDatabaseTest() {
        String filePath = "./TestingFiles/dbImpressionLogTestFile.csv";
        db_ctrl.storeImpressionLog(filePath);
        boolean isSame = true;
        List<String[]> result = db_ctrl.dbQuery("SELECT user_id, date, gender, age, income, context, impression_cost FROM impression_log", 7);

        try {
            BufferedReader br = new BufferedReader(new FileReader(filePath));
            br.readLine(); // Remove headings
            String line = "";
            int queryIndex = 0;

            while ((line = br.readLine()) != null) {

                // File values
                String[] entry = line.split(",");
                String user_id = entry[1];
                String date = entry[0];
                String gender = entry[2];
                String age  = entry[3];
                String income = entry[4];
                String context = entry[5];
                String impression_cost = entry[6];

                // Database values
                String[] record = result.get(queryIndex);
                String user_idR = record[0];
                String dateR = record[1];
                String genderR = record[2];
                String ageR  = record[3];
                String incomeR = record[4];
                String contextR = record[5];
                String impression_costR = record[6];

                isSame = (
                        user_id.equals(user_idR) &&
                                date.equals(dateR) &&
                                gender.equals(genderR) &&
                                age.equals(ageR) &&
                                income.equals(incomeR) &&
                                context.equals(contextR) &&
                                impression_cost.equals(impression_costR)
                );

                queryIndex++;
            }

            assertTrue(isSame);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private String fromBool(int con) {
        if (con == 0)
            return "No";
        else
            return "Yes";
    }

}
