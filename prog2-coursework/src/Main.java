import javafx.event.EventHandler;
import javafx.scene.control.*;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.stage.Stage;

public class Main extends Application {
    //the grid object
    Grid grid;

    public void start(Stage stage) {
        String cssLayout = "-fx-border-color: red;\n" +
                "-fx-border-insets: 5;\n" +
                "-fx-border-width: 3;\n" +
                "-fx-border-style: dashed;\n";


        Buttons buttons = new Buttons(grid);


        //creating the text area used to load a grid
        TextArea loadTextArea = new TextArea();
        loadTextArea.setWrapText(true);


        BorderPane uiDivider = new BorderPane();
        uiDivider.setBottom(buttons.getAllButtons());

        uiDivider.setCenter(grid);


        //creating a scene from the pane
        Scene scene = new Scene(uiDivider);


        //creating a stage from the scene
        stage.setScene(scene);


        //setting the default width and height of the stage
        stage.setWidth(700);
        stage.setHeight(750);

        //setting the minimum width and height
        stage.setMinWidth(700);
        stage.setMinHeight(750);


        //setting button functionality
        buttons.setUndoFunctionality();
        buttons.setRedoFunctionality();
        buttons.setClearFunctionality();
        buttons.setMistakeButtonFunctionality();
        buttons.setLoadFileFunctionality(uiDivider, loadTextArea, stage, scene);
        buttons.setLoadTextFunctionality(uiDivider, scene, loadTextArea);
        buttons.setCreateGridFunctionality(uiDivider, loadTextArea, scene);
        buttons.setGenerateGridFunctionality(uiDivider);
        buttons.setSolverFunctionality();
        buttons.setHintFunctionality();
        buttons.setGenerateFunctionality(uiDivider, loadTextArea, scene);


        //displaying the stage
        stage.show();

    }

    public static void main(String[] args) {
        launch(args);
    }
}