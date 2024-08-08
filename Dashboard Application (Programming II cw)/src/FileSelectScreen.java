package src;

import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FileSelectScreen {

    private final Stage stage;
    private final DatabaseController dbc;

    private final List<Label> allLabels;
    private final List<Button> allButtons;

    private final MenuBarHandler menuBarHandler;

    private int fileCount = 0;
    private final boolean[] logsGiven;
    private final String[] logsPath;

    /*
    @desc - constructs the file select window
    @param - stage: stage to use as the window
    @param - db: database being used to store the data
    @param - dbc: controller for the database
    */
    public FileSelectScreen(Stage stage, DatabaseController dbc) {
        this.stage = stage;
        this.dbc = dbc;

        allLabels = new ArrayList<>();
        allButtons = new ArrayList<>();
        menuBarHandler = new MenuBarHandler(stage,new ArrayList<>());
        logsGiven = new boolean[3];
        logsPath = new String[3];

        setupInterface();
    }

    // sets up the GUI
    // configures the stage - width, height, title & scene
    private void setupInterface() {
        configureStage(setupScene(setupVBox()));
    }

    // sets up the scene from the StackPane
    private Scene setupScene(VBox vbox) {
        StackPane root = new StackPane();
        root.getChildren().add(vbox);
        return new Scene(root);
    }

    // configures the stage to have property values that are desirable and more user friendly
    // e.g. changes the height to an intuitive value
    private void configureStage(Scene scene) {
        stage.setMinWidth(550);
        stage.setMinHeight(300);
        stage.setTitle("File Selection");
        stage.setScene(scene);
        stage.show();
    }

    private VBox setupVBox() {
        VBox verticalVBox = new VBox(10);

        Label fileCountShowLabel = createNewLabel("0 files selected");
        fileCountShowLabel.setFont(new Font("Arial", 24));
        Button continueButton = setupContinueButton();

        Label[] selectLabels = new Label[]{
                createNewLabel("No File Selected"),
                createNewLabel("No File Selected"),
                createNewLabel("No File Selected"),};

        Button[] removeButtons = new Button[] {
                setupRemoveButton(0, continueButton, selectLabels[0], fileCountShowLabel),
                setupRemoveButton(1, continueButton, selectLabels[1], fileCountShowLabel),
                setupRemoveButton(2, continueButton, selectLabels[2], fileCountShowLabel) };
        Button[] selectButtons = new Button[]{
                setupSelectButton(0, selectLabels[0], fileCountShowLabel, removeButtons[0], continueButton),
                setupSelectButton(1, selectLabels[1], fileCountShowLabel, removeButtons[1], continueButton),
                setupSelectButton(2, selectLabels[2], fileCountShowLabel, removeButtons[2], continueButton)};

        configureContinueButton(continueButton, selectButtons, removeButtons);
        HBox horizontalHBox = setupHorizontalHBox(continueButton, fileCountShowLabel);
        GridPane gridPane = setupGridPane(selectButtons, removeButtons, selectLabels);

        verticalVBox.getChildren().addAll(
                menuBarHandler.fileSelectScreen(allLabels, allButtons),
                gridPane,
                horizontalHBox,
                setupCheatButton(continueButton));
        menuBarHandler.updateFontSize("File Select Screen");

        return verticalVBox;
    }

    private Button setupCheatButton(Button continueButton) {
        // autoload files button
        Button cheatButton = new Button("Auto load files (GUI not updated, testing purposes)");
        allButtons.add(cheatButton);
        cheatButton.setOnAction(e -> cheatButtonEvent(continueButton));
        return cheatButton;
    }

    private void cheatButtonEvent(Button continueButton) {
        logsPath[0] = ".//impression_log.csv";
        logsPath[1] = ".//server_log.csv";
        logsPath[2] = ".//click_log.csv";
        continueButton.setDisable(false);
        logsGiven[0] = true;
        logsGiven[1] = true;
        logsGiven[2] = true;
        continueButton.fire();
    }

    private HBox setupHorizontalHBox(Button continueButton, Label fileCountShowLabel) {
        HBox horizontalHBox = new HBox(10);
        horizontalHBox.getChildren().addAll(fileCountShowLabel, continueButton);
        horizontalHBox.setAlignment(Pos.BASELINE_RIGHT);
        horizontalHBox.setPadding(new Insets(10));
        return horizontalHBox;
    }

    private Button setupContinueButton() {
        Button continueButton = new Button();
        allButtons.add(continueButton);
        return continueButton;
    }

    private void configureContinueButton(Button continueButton, Button[] selectLogButtons, Button[] removeLogButtons) {
        continueButton.setText("Continue");
        continueButton.setDisable(true);
        continueButton.setOnAction(e -> continueButtonEvent(continueButton, selectLogButtons, removeLogButtons));
    }

    private void continueButtonEvent(Button continueButton, Button[] selectLogButtons, Button[] removeLogButtons) {
        // store logs in database
        Thread t = new Thread(
                () -> {
                    QueryExecutor queryExecutor = new QueryExecutor();
                    Platform.runLater(() -> disableLogButtons(continueButton, selectLogButtons, removeLogButtons));

                    ArrayList<Button> buttonsList = getLogButtons(continueButton, selectLogButtons, removeLogButtons);
                    Platform.runLater(() -> attemptGoDataViewScreen(continueButton, buttonsList, queryExecutor));
                }
        );
        t.start();
    }

    private void attemptGoDataViewScreen(Button continueButton, ArrayList<Button> buttonsList, QueryExecutor queryExecutor) {
        Alert error = dbc.showAnyErrors();
        if (error != null) {
            error.show();
            setDisable(false, buttonsList);
            continueButton.setText("Continue");
        }
        else {
            goToDataViewScreen(buttonsList, continueButton, queryExecutor);
        }
    }

    private void goToDataViewScreen(ArrayList<Button> buttons, Button continueButton, QueryExecutor qry_exec) {
        setDisable(false, buttons);
        continueButton.setText("Continue");

        // open the next screen
        stage.setMinWidth(1200);
        stage.setMinHeight(700);
        new DataViewScreen(stage, dbc, qry_exec, menuBarHandler, stage.getScene());
    }

    private void disableLogButtons(Button continueButton, Button[] selectLogButtons, Button[] removeLogButtons) {
        ArrayList<Button> buttons = new ArrayList<>(Arrays.asList(
                continueButton, selectLogButtons[0], selectLogButtons[1], selectLogButtons[2],
                removeLogButtons[0], removeLogButtons[1], removeLogButtons[2]));
        setDisable(true, buttons);
        continueButton.setText("Loading...");
    }

    private ArrayList<Button> getLogButtons(Button continueButton, Button[] selectLogButtons, Button[] removeLogButtons) {
        ArrayList<Button> buttonsList = new ArrayList<>();

        if (logsGiven[0]) {
            dbc.storeImpressionLog(logsPath[0]);
            buttonsList.add(removeLogButtons[0]);
        }
        if (logsGiven[1]) {
            dbc.storeServerLog(logsPath[1]);
            buttonsList.add(removeLogButtons[1]);
        }
        if (logsGiven[2]) {
            dbc.storeClickLog(logsPath[2]);
            buttonsList.add(removeLogButtons[2]);
        }
        buttonsList.addAll(Arrays.asList(continueButton, selectLogButtons[0], selectLogButtons[1], selectLogButtons[2]));

        return buttonsList;
    }

    // sets up the GridPane with valid properties
    private GridPane setupGridPane(Button[] selectButtons, Button[] removeButtons, Label[] selectLabels) {
        GridPane selectionGridPane = new GridPane();
        selectionGridPane.setPadding(new Insets(20));
        selectionGridPane.setHgap(20);
        selectionGridPane.setVgap(20);

        // labels showing which log is which
        selectionGridPane.add(createNewLabel("Impression Log"), 0, 0);
        selectionGridPane.add(createNewLabel("Server Log"), 0, 1);
        selectionGridPane.add(createNewLabel("Click Log"), 0, 2);
        // buttons which allow the user to select the logs
        selectionGridPane.add(selectButtons[0], 1, 0);
        selectionGridPane.add(selectButtons[1], 1, 1);
        selectionGridPane.add(selectButtons[2], 1, 2);
        // buttons which allow the user to remove the selected logs
        selectionGridPane.add(removeButtons[0], 2, 0);
        selectionGridPane.add(removeButtons[1], 2, 1);
        selectionGridPane.add(removeButtons[2], 2, 2);
        // labels which say whether a file has been selected, and if one has then it shows the name of this file
        selectionGridPane.add(selectLabels[0], 3, 0);
        selectionGridPane.add(selectLabels[1], 3, 1);
        selectionGridPane.add(selectLabels[2], 3, 2);

        return selectionGridPane;
    }

    private Label createNewLabel(String labelText) {
        Label label = new Label(labelText);
        allLabels.add(label);
        return label;
    }

    ////////////////////
    private Button setupRemoveButton(int log, Button continueButton, Label selectedLabel, Label fileCountShowerLabel) {
        Button removeButton = new Button("Remove File");
        removeButton.setDisable(true);
        removeButton.setOnAction(e -> removeLogEvent(log,removeButton, continueButton, selectedLabel, fileCountShowerLabel));
        allButtons.add(removeButton);
        return removeButton;
    }

    ////////////////////
    private void removeLogEvent(int log, Button removeImpressionLogButton, Button continueButton, Label impressionSelectedLabel, Label fileCountShowerLabel) {
        removeImpressionLogButton.setDisable(true);
        logsGiven[log] = false;
        logsPath[log] = null;
        impressionSelectedLabel.setText("No File Selected");
        fileCount--;
        fileCountShowerLabel.setText(fileCount + " file(s) selected");
        buttonEnabler(continueButton);
    }

    ////////////////////
    private Button setupSelectButton(int log, Label selectLabel, Label fileCountShowerLabel, Button removeButton, Button continueButton) {
        Button button = new Button("Select File");
        button.setOnAction(e -> selectLogEvent(log,selectLabel, fileCountShowerLabel, removeButton, continueButton));
        allButtons.add(button);
        return button;
    }

    private void selectLogEvent(int log, Label selectLabel, Label fileCountShowerLabel, Button removeButton, Button continueButton) {
        File f;
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Open File to Load");

        String currentPath = Paths.get(".").toAbsolutePath().normalize().toString();
        fileChooser.setInitialDirectory(new File(currentPath));

        FileChooser.ExtensionFilter csvFilter = new FileChooser.ExtensionFilter("Text files", "*.csv");
        fileChooser.getExtensionFilters().add(csvFilter);
        f = fileChooser.showOpenDialog(stage);
        logsGiven[log] = true;
        if (selectLabel.getText().equals("No File Selected")) {
            selectLabel.setText(f.getName());
            fileCount++;
            fileCountShowerLabel.setText(fileCount + " file(s) selected");
        }
        else {
            selectLabel.setText(f.getName());
        }
        removeButton.setDisable(false);
        buttonEnabler(continueButton);
        //store click log path
        logsPath[log] = f.getAbsolutePath();
    }
    ////////////////////

    /*
    @desc - Disables/Enables list of buttons
    @param - set - Boolean value to set disable to
    @param - buttons - ArrayList of buttons to set to disabled/not disabled
    */
    private void setDisable(boolean set, ArrayList<Button> buttons) {
        buttons.stream().parallel().forEach(b -> b.setDisable(set));
    }

    private void buttonEnabler(Button buttonEnabler) {
        if (logsGiven[0]) {
            buttonEnabler.setDisable(!logsGiven[2] && logsGiven[1]);
        }
        else {
            buttonEnabler.setDisable(true);
        }
    }
}