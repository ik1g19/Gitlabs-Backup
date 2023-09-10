package src;

import javafx.application.Application;
import javafx.stage.Stage;

public class Main extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        Database db = new Database("./logs.db");
        DatabaseController db_ctrl = new DatabaseController(db); // Use to interface with database
        FileSelectScreen screen = new FileSelectScreen(primaryStage, db_ctrl);

        //db_ctrl.dbQuery("SELECT COUNT(click_log.id) / COUNT(impression_log.id) FROM click_log, impression_log",1);
    }
}