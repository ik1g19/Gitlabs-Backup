import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.nio.file.Paths;
import java.util.Optional;

public class Buttons {
    private Button loadFile = new Button("Load From File");
    private Button loadText = new Button("Load From Text Input");
    private Button generateGrid = new Button("Create Random Grid");

    //these buttons should not be displayed initially
    private CheckBox mistakes = new CheckBox("Show Mistakes");
    private Button undo = new Button("Undo");
    private Button redo = new Button("Redo");
    private Button clear = new Button("Clear");
    private Button createGrid = new Button("Create Grid");
    private Button solveGrid = new Button("Solve!");
    private Button hint = new Button("Hint");
    private Button generate = new Button("Generate");


    //creating the hbox for loading buttons
    HBox loadingButtons = new HBox();
    //creating a hbox for buttons used during the game
    HBox mathdukoButtons = new HBox();
    //creating the overall button layout
    VBox buttons = new VBox();


    VBox sliderBox = new VBox();
    Label size;
    Slider slider;


    //defining an event filter for grid input
    EventHandler gridInput = new EventHandler<KeyEvent>() {

        public void handle(KeyEvent event) {
            switch (event.getCode()) {
                //enter number into grid if user presseds digit
                case DIGIT1: grid.numEntered(1); break;
                case DIGIT2: grid.numEntered(2); break;
                case DIGIT3: grid.numEntered(3); break;
                case DIGIT4: grid.numEntered(4); break;
                case DIGIT5: grid.numEntered(5); break;
                case DIGIT6: grid.numEntered(6); break;
                case DIGIT7: grid.numEntered(7); break;
                case DIGIT8: grid.numEntered(8); break;
                case DIGIT9: grid.numEntered(9); break;
                //if the user presses backspace clear the selected tile, if there is one
                case BACK_SPACE: if (grid.isATileSelected()) grid.clearSelected(); break;
                //if the user presses escape unselect the cell
                case ESCAPE: grid.unselectCurrentTile(); break;
            }

            event.consume();
        }

    };


    Grid grid;


    public Buttons(Grid grid) {
        this.grid = grid;

        buttons.getChildren().addAll(generate, createGrid, loadingButtons, mathdukoButtons);

        mathdukoButtons.getChildren().addAll(undo, redo, clear, mistakes, solveGrid, hint);
        loadingButtons.getChildren().addAll(loadFile, loadText, generateGrid);


        //initially only the loading buttons should be visible
        mathdukoButtons.setVisible(false);
        createGrid.setVisible(false);
        generate.setVisible(false);


        //spacing and alignment
        loadingButtons.setSpacing(10);
        mathdukoButtons.setSpacing(10);

        buttons.setSpacing(20);

        mathdukoButtons.setAlignment(Pos.CENTER);
        loadingButtons.setAlignment(Pos.CENTER);
        buttons.setAlignment(Pos.CENTER);

        BorderPane.setMargin(buttons, new Insets(0, 0, 20, 0));


        // Creates a slider
        slider = new Slider(2, 8, 0.5);
        slider.setShowTickMarks(true);
        slider.setShowTickLabels(true);
        slider.setMajorTickUnit(1f);
        slider.setMinorTickCount(0);
        slider.setSnapToTicks(true);

        size = new Label();

        size.setFont(new Font("Arial", 30));

        slider.valueProperty().addListener(new ChangeListener<Number>() {
            public void changed(ObservableValue<? extends Number> ov,
                                Number old_val, Number new_val) {
                size.setText(Integer.toString(new_val.intValue()));
            }
        });

        sliderBox.getChildren().addAll(slider, size);
    }


    public VBox getAllButtons() {
        return buttons;
    }


    public void setUndoFunctionality() {
        //undo button
        undo.setOnMouseClicked(e -> {
            //if the stack is not empty undo changes and check for new mistakes
            if (!grid.getUndoStack().empty()) {
                grid.lastChangeToRedo();

                //retrieving the change
                Integer numOriginal = grid.getUndoStack().pop();
                GridSquare tile = grid.getTileByLocation(grid.getUndoStack().pop());

                tile.setNum(numOriginal);

                //checking for new mistakes
                grid.updateGrid(tile);

                //changing selection back to the tile from the undo stack
                grid.changeSelection(tile, false);

                //pushing to the redo stack
                grid.getRedoStack().push(tile.getLocation());
                grid.getRedoStack().push(numOriginal);
            }
        });
    }


    public void setRedoFunctionality() {
        //redo button
        redo.setOnMouseClicked(e -> {
            //if the stack is not empty redo changes and check for new mistakes
            if (!grid.getRedoStack().empty()) {
                //retrieving the change
                Integer newNum = grid.getRedoStack().pop();
                GridSquare tile = grid.getTileByLocation(grid.getRedoStack().pop());

                tile.setNum(newNum);

                //checking for new mistakes
                grid.updateGrid(tile);

                //changing selection back to the tile from the redo stack
                grid.changeSelection(tile, false);

                //pushing to the undo stack
                grid.getUndoStack().push(tile.getLocation());
                grid.getUndoStack().push(newNum);
            }
        });
    }


    public void setClearFunctionality() {
        //clear button
        clear.setOnMouseClicked(e -> {
            //shows the user an alert before allowing them to confirm clearing the board
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION, "Board will be cleared, are you sure?");

            //only clear the board if the user chooses ok
            Optional<ButtonType> warning = alert.showAndWait();
            if (warning.isPresent() && warning.get() == ButtonType.OK) {
                grid.clearBoard();
                grid.unselectCurrentTile();
                if (grid.isCompleted()) grid.stopWinAnimation();

                grid.getUndoStack().clear();
                grid.getRedoStack().clear();
            }
        });
    }


    public void setMistakeButtonFunctionality() {
        mistakes.selectedProperty().addListener(new ChangeListener<Boolean>() {
            //change listener for the show mistakes button
            //when selected or unselected the grid is checked for mistakes and they are shown and hid
            //appropriately
            @Override
            public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                GridSquare[] tiles = grid.getAllTiles();

                for (int i = 0; i < tiles.length; i++) {
                    GridSquare tile = tiles[i];

                    //if show mistakes was turned on show mistakes
                    if (newValue) {
                        tile.drawMistakes();
                    }
                    //otherwise hide any mistakes
                    else tile.drawNormal();

                    //if the tile was selected, draw it as selected
                    if (tile.isSelected()) {
                        tile.drawSelected();
                    }
                }
            }
        });
    }


    public void setLoadFileFunctionality(BorderPane uiDivider, TextArea loadTextArea, Stage stage, Scene scene) {
        //load from file button
        loadFile.setOnMouseClicked(e -> {
            //makes sure the load text button is visible
            loadText.setVisible(true);
            createGrid.setVisible(false);
            generate.setVisible(false);
            //removes the load text area if it is in the border pane
            uiDivider.getChildren().remove(loadTextArea);
            uiDivider.getChildren().remove(sliderBox);

            FileChooser fileChooser = new FileChooser();
            fileChooser.setTitle("Select Grid to Load");
            String currentPath = Paths.get(".").toAbsolutePath().normalize().toString();
            fileChooser.setInitialDirectory(new File(currentPath));

            FileChooser.ExtensionFilter txtFilter = new FileChooser.ExtensionFilter("Text files", "*.txt");
            fileChooser.getExtensionFilters().add(txtFilter);

            File file = fileChooser.showOpenDialog(stage);

            //if the user selected a file create the grid
            if (file != null) {

                //creating the new grid
                grid = null;
                grid = new TextGrid(file.getAbsolutePath(), true, mistakes);

                //if the created grid is not valid
                if (!grid.isValid()) {
                    showWarning(grid.getInvalidMessage());
                }
                //otherwise the created grid was valid
                else {
                    alignGrid(grid, uiDivider);

                    mathdukoButtons.setVisible(true);

                    //removing any previous event filters
                    scene.removeEventFilter(KeyEvent.KEY_PRESSED, gridInput);

                    //adding an event handler to handle key events
                    scene.addEventFilter(KeyEvent.KEY_PRESSED, gridInput);
                }

            }
            //otherwise tell the user they did not select a file
            else {
                showWarning("No File Selected");
            }
        });
    }


    public void setLoadTextFunctionality(BorderPane uiDivider, Scene scene, TextArea loadTextArea) {
        //load from text button
        loadText.setOnMouseClicked(e -> {
            //removing any previous event filters
            scene.removeEventFilter(KeyEvent.KEY_PRESSED, gridInput);

            //hide the load from text button
            loadText.setVisible(false);
            mathdukoButtons.setVisible(false);
            createGrid.setVisible(true);
            generate.setVisible(false);

            //positioning the text area
            uiDivider.setCenter(loadTextArea);
            BorderPane.setMargin(loadTextArea, new Insets(30,30,30,30));
        });
    }


    public void setCreateGridFunctionality(BorderPane uiDivider, TextArea loadTextArea, Scene scene) {
        //creates the grid from the given text when clicked
        createGrid.setOnMouseClicked(e -> {
            //retrieving the text from the text area
            String text = loadTextArea.getText();

            grid = null;
            grid = new TextGrid(text, false, mistakes);

            //if grid did not create successfully
            if (!grid.isValid()) {
                showWarning(grid.getInvalidMessage());
            }
            //otherwise grid was created suceesfully
            else {
                alignGrid(grid, uiDivider);
                //showing and hiding appropriate buttons
                createGrid.setVisible(false);
                mathdukoButtons.setVisible(true);
                loadText.setVisible(true);

                //removing the text area
                uiDivider.getChildren().remove(loadTextArea);

                //removing any previous event filters
                scene.removeEventFilter(KeyEvent.KEY_PRESSED, gridInput);

                //adding an event handler to handle key events
                scene.addEventFilter(KeyEvent.KEY_PRESSED, gridInput);
            }
        });
    }


    public void setGenerateGridFunctionality(BorderPane uiDivider) {
        //generate grid button
        generateGrid.setOnMouseClicked(e -> {
            //showing and hiding appropriate buttons
            generate.setVisible(true);
            createGrid.setVisible(false);
            mathdukoButtons.setVisible(false);

            uiDivider.setCenter(sliderBox);

            uiDivider.setCenter(sliderBox);

            BorderPane.setMargin(sliderBox, new Insets(20, 0, 0, 0));

            sliderBox.setAlignment(Pos.CENTER);
        });
    }

    public void setGenerateFunctionality(BorderPane uiDivider, TextArea loadTextArea, Scene scene) {
        //generate grid button
        generate.setOnMouseClicked(e -> {
            createGrid.setVisible(false);
            generate.setVisible(false);
            mathdukoButtons.setVisible(true);
            loadText.setVisible(true);

            grid = null;
            grid = new RandomGrid(mistakes, (int)slider.getValue());

            alignGrid(grid, uiDivider);

            //removing the text area
            uiDivider.getChildren().remove(loadTextArea);

            //removing any previous event filters
            scene.removeEventFilter(KeyEvent.KEY_PRESSED, gridInput);

            //adding an event handler to handle key events
            scene.addEventFilter(KeyEvent.KEY_PRESSED, gridInput);
        });
    }


    public void setSolverFunctionality() {
        //solve grid button
        solveGrid.setOnMouseClicked(e -> {
            grid.getSolver().setSolution();
            grid.displayWinAnimation();
        });
    }


    public void setHintFunctionality() {
        //hint button
        hint.setOnMouseClicked(e -> {
            grid.getSolver().showHint();
        });
    }


    private void alignGrid(Grid grid, BorderPane uiDivider) {
        uiDivider.setCenter(grid);

        BorderPane.setMargin(grid, new Insets(20, 0, 0, 0));

        grid.setAlignment(Pos.CENTER);
    }

    private void showWarning(String text) {
        Alert warning = new Alert(Alert.AlertType.WARNING, text);
        warning.show();
    }
}
