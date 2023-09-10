package src;

import java.sql.*;

public class Database {

    private Statement statement;
    private Connection conn;

    public Database(String dbPath){
        this.statement = init(dbPath); // Connect/Create database
        try{buildDatabase();} catch (SQLException e){System.err.println("Database Build Error: "+e);}
    }

    /*
        Initialise connection to database
        @param - Path to create database file
    */
    private Statement init(String path){
        try {
            Connection conn = DriverManager.getConnection("jdbc:sqlite:"+path);
            this.conn = conn;
            if (conn != null) {
                System.out.println("Database Initialised. (Database class)");
            }
            Statement statement = conn.createStatement();
            statement.setQueryTimeout(30);  // set timeout to 30 sec.

            return statement;
        } catch (SQLException e){
            System.err.println("Database Initialisation Error: "+e);
        }
        return null;
    }

    // Build database tables
    private void buildDatabase() throws SQLException{
        conn.setAutoCommit(false);
        statement.addBatch("drop table if exists server_log");
        statement.addBatch("drop table if exists click_log");
        statement.addBatch("drop table if exists impression_log");
        statement.addBatch(
                                "create table impression_log (" +
                                    "id integer primary key AUTOINCREMENT," +
                                    "user_id integer,"+
                                    "date DATETIME,"+
                                    "gender varchar(6)," +
                                    "age integer," +
                                    "income varchar(6)," +
                                    "context varchar(8)," +
                                    "impression_cost double)"
                                );

        statement.addBatch(
                                "create table click_log (" +
                                    "id integer primary key AUTOINCREMENT,"+
                                    "user_id integer references impression_log (user_id) on update cascade on delete cascade," +
                                    "date DATETIME,"+
                                    "click_cost double)"
                                );

        statement.addBatch(
                                "create table server_log (" +
                                    "id integer primary key AUTOINCREMENT,"+
                                    "user_id integer references impression_log (user_id) on update cascade on delete cascade," +
                                    "entry_date DATETIME,"+
                                    "exit_date DATETIME,"+
                                    "viewed integer,"+
                                    "conversion integer)" // 0 - No, 1 - Yes
                                );
        statement.addBatch("CREATE INDEX click_user_id ON click_log(user_id)");
        statement.addBatch("CREATE INDEX server_user_id ON server_log(user_id)");
        statement.addBatch("CREATE INDEX impression_user_id ON impression_log(user_id)");
        statement.addBatch("CREATE INDEX impression_date ON impression_log(date)");
        statement.executeBatch();
        conn.commit();
        conn.setAutoCommit(true);
    }

    public void rebuild () throws SQLException{
        buildDatabase();
    }

    public Statement getStatement(){
        return statement;
    }

    public Connection getConnection(){
        return conn;
    }

}
