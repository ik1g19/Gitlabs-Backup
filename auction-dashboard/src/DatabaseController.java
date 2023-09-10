package src;

import javafx.scene.control.Alert;
import src.Database;

import java.io.BufferedReader;
import java.io.FileReader;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class DatabaseController {

    private Database db;
    private Connection conn;
    private boolean serverError = false;
    private boolean impressionError = false;
    private boolean clickError = false;

    public DatabaseController (Database db) {
        this.db = db;
        this.conn = db.getConnection();
    }

    /*
    Insert Server Log into Database
    @param - File Path to CSV
    @return - True - Successful, False - Not Successful
     */
    public boolean storeServerLog (String filePath) {
        try {
            String line = "";

            BufferedReader br = new BufferedReader(new FileReader(filePath));
            String[] headings = br.readLine().split(","); // Check file
            if (checkHeadings(headings, 1)) {
                PreparedStatement statement = conn.prepareStatement("insert into server_log (user_id, entry_date, exit_date, viewed, conversion) values (?,?,?,?,?)");
                conn.setAutoCommit(false);
                while ((line = br.readLine()) != null) {
                    String[] entry = line.split(",");
                    statement.setString(1, entry[1]);
                    statement.setString(2, entry[0]);
                    statement.setString(3, entry[2]);
                    statement.setString(4, entry[3]);
                    statement.setString(5, String.valueOf(toBool(entry[4])));
                    statement.addBatch();
                }
                statement.executeBatch();
                conn.commit();
                return true;
            }
            else {
                serverError = true;
            }
        } catch (Exception e){
            serverError = true;
            System.err.println("Server log storage error: " + e);
        }
        return false;
    }

    /*
   Insert Click Log into Database
   @param - File Path to CSV
   @return - True - Successful, False - Not Successful
    */
    public boolean storeClickLog (String filePath) {
        try {

            String line = "";

            BufferedReader br = new BufferedReader(new FileReader(filePath));
            String[] headings = br.readLine().split(","); // Check file
            if (checkHeadings(headings, 0)){
                PreparedStatement statement = conn.prepareStatement("insert into click_log (user_id, date, click_cost) values (?,?,?)");
                conn.setAutoCommit(false);
                while ((line = br.readLine()) != null) {
                    String[] entry = line.split(",");
                    statement.setString(1, entry[1]);
                    statement.setString(2, entry[0]);
                    statement.setString(3, entry[2]);
                    statement.addBatch();
                }
                statement.executeBatch();
                conn.commit();
                return true;
            } else {
                clickError = true;
            }
        } catch (Exception e){
            clickError = true;
            System.err.println("Click log storage error: " + e);
        }
        return false;
    }


    /*
   Insert Impression Log into Database
   @param - File Path to CSV
   @return - True - Successful, False - Not Successful
    */
    public boolean storeImpressionLog (String filePath) {
        try {

            String line = "";

            BufferedReader br = new BufferedReader(new FileReader(filePath));
            String[] headings = br.readLine().split(","); // Check file
            if (checkHeadings(headings, 2)) {
                PreparedStatement statement = conn.prepareStatement("insert into impression_log (user_id, date, gender, age, income, context, impression_cost) values (?,?,?,?,?,?,?)");
                conn.setAutoCommit(false);
                while ((line = br.readLine()) != null) {
                    String[] entry = line.split(",");
                    statement.setString(1, entry[1]);
                    statement.setString(2, entry[0]);
                    statement.setString(3, entry[2]);
                    statement.setString(4, entry[3]);
                    statement.setString(5, entry[4]);
                    statement.setString(6, entry[5]);
                    statement.setString(7, entry[6]);
                    statement.addBatch();
                }
                statement.executeBatch();
                conn.commit();
                return true;
            } else {
                impressionError = true;
            }
        } catch (Exception e){
            impressionError = true;
            System.err.println("Impression log storage error: " + e);
        }
        return false;
    }

    private boolean checkHeadings(String[] headings, int i) {
        boolean correctFile = false;
        if (i == 0){
            correctFile = (
                    headings[0].equals("Date") &&
                    headings[1].equals("ID") &&
                    headings[2].equals("Click Cost")
            );
        }
        else if (i == 1){
            correctFile = (
                    headings[0].equals("Entry Date") &&
                    headings[1].equals("ID") &&
                    headings[2].equals("Exit Date") &&
                    headings[3].equals("Pages Viewed") &&
                    headings[4].equals("Conversion")
            );
        }
        else if (i == 2){
            correctFile = (
                    headings[0].equals("Date") &&
                    headings[1].equals("ID") &&
                    headings[2].equals("Gender") &&
                    headings[3].equals("Age") &&
                    headings[4].equals("Income") &&
                    headings[5].equals("Context") &&
                    headings[6].equals("Impression Cost")
            );
        }

        return correctFile;
    }

    /*
    @param - query - the SQLite query to be executed e.g. "Select * From [table]"
    @param - returnColumnCount - the number of columns the query will return e.g. "Select a, b, c From [table]" should be 3
    @return - an ArrayList where each item is an Array, unless the query fails where null is returned
    */
    public List<String[]> dbQuery(String query, int returnColumnCount) {
        Statement statement = null;
        try {
            statement = conn.createStatement();
        } catch (SQLException e) {
            System.err.println("Query error: "+e);
        }
        // attempts to executes the query
        try (ResultSet rs = statement.executeQuery(query)) {
            // creates an ArrayList of Arrays
            List<String[]> data = new ArrayList<String[]>();
            // loops for all of the records returned by the query
            while (rs.next()) {
                String[] line = new String[returnColumnCount];
                // adds all of the data from that record into an array
                for (int i = 0; i < returnColumnCount; i++) {
                    line[i] = rs.getString(i + 1); // ResultSet starts index count at 1 => i + 1
                }
                data.add(line);
            }
            //showAllQueryResult(data, returnColumnCount);

            return data;
        }
        catch (SQLException e) {
            System.out.println(e.getMessage());
        }
        return null;
    }

    public void tearDown() {
        try {
            db.rebuild();
        } catch (SQLException e){
            System.err.println("Tear down error: "+e);
        }

    }

    /*
     Convert Yes or No into integer boolean value
     @param - String of 'Yes' or 'No'
     @param - Boolean equivalent, -1 if invalid string
     */
    private int toBool (String val){
        if (val.equals("Yes"))
            return 1;
        else if (val.equals("No"))
            return 0;
        else
            return -1;
    }

    public Alert showAnyErrors (){
        if(clickError || serverError || impressionError) {
            Alert error = new Alert(Alert.AlertType.ERROR);
            if(impressionError)
                error.setContentText(error.getContentText()+"Impression Log incorrect\n\n");
            if(serverError)
                error.setContentText(error.getContentText()+"Server Log incorrect\n\n");
            if(clickError)
                error.setContentText(error.getContentText()+"Click Log incorrect\n\n");
            impressionError = false;
            serverError = false;
            clickError = false;
            return error;
        }
        return null;
    }

    public boolean getServerError(){return serverError;}
    public boolean getClickError(){return clickError;}
    public boolean getImpressionError(){return impressionError;}
}
