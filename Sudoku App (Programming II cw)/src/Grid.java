import javafx.animation.*;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.layout.*;
import javafx.util.Duration;

import java.util.*;

//represents the grid, contains all the tiles in the grid
//class is abstract as it provides methods for other types of grid
public abstract class Grid extends GridPane {

    protected int dimensions;

    protected GridSquare[] gridRepresentation;

    protected TextField grid;

    protected GridSquare currentlySelected;

    protected ArrayList<Cage> cages;
    protected Column[] columns;
    protected Row[] rows;


    protected Stack<Integer> undoStack;
    protected GridSquare tileToCommitToUndo;
    protected Integer numToCommitToUndo;
    protected Stack<Integer> redoStack;

    protected CheckBox mistakeToggle;

    protected Solver solver;


    //flag that stores whether the grid is valid
    //assumes grid is valid until proven otherwise
    protected boolean valid = true;
    //holds a message explaining why the grid is invalid
    protected String invalidMessage;

    private boolean completed = false;


    public Grid() {

    }

    public Grid(CheckBox mistakeToggle) {
        //storing a reference to the show mistakes toggle
        this.mistakeToggle = mistakeToggle;

        solver = new Solver(this);
    }


    public Solver getSolver() {
        return solver;
    }


    public void numEntered(Integer num) {
        //if the user has selected a tile and the number is valid
        if (currentlySelected != null && num <= dimensions) {
            //entering the number into the tile
            currentlySelected.setNum(num);

            //updates the grid based on changes made to the tile
            updateGrid(currentlySelected);

            //getting changes ready to be pushed onto the undo stack
            commitChangesToUndo();
            tileToCommitToUndo = currentlySelected;
            numToCommitToUndo = num;

            if (checkIfWon()) displayWinAnimation();
        }
    }


    public void clearSelected() {
        //clear the selected tile
        currentlySelected.clearNum();

        //updates the grid based on changes made to the tile
        updateGrid(currentlySelected);

        commitChangesToUndo();
        tileToCommitToUndo = currentlySelected;
        numToCommitToUndo = currentlySelected.getNumber();
    }


    //unselects the currently selected tile
    public void unselectCurrentTile() {
        if (currentlySelected != null) {
            currentlySelected.unselect(mistakeToggle.isSelected());
            currentlySelected = null;
        }
    }

    //selects a new tile
    public void selectTile(GridSquare tile) {
        //if there is already a tile selected, unselect it
        if (currentlySelected != null) unselectCurrentTile();

        tile.select();
        currentlySelected = tile;
    }


    public void commitChangesToUndo() {
        if (tileToCommitToUndo != null && numToCommitToUndo != null) {
            undoStack.push(tileToCommitToUndo.getLocation());
            undoStack.push(numToCommitToUndo);
        }
    }

    //pushes the last change to the redo stack
    public void lastChangeToRedo() {
        if (tileToCommitToUndo != null && numToCommitToUndo != null) {
            redoStack.push(tileToCommitToUndo.getLocation());
            redoStack.push(numToCommitToUndo);
            tileToCommitToUndo = null;
            numToCommitToUndo = null;
        }
    }


    public Stack<Integer> getUndoStack() {
        return undoStack;
    }

    public Stack<Integer> getRedoStack() {return redoStack;}


    public GridSquare getTileByLocation(Integer location) {
        return gridRepresentation[location-1];
    }

    //1 based coordinates
    public GridSquare getTileBy2DLocation(int x, int y) {
        int location = (y - 1) * dimensions;
        location += x;
        return gridRepresentation[location - 1];
    }


    public void changeSelection(GridSquare tile, boolean userChange) {
        //if clicked tile is already selected and change was made by a user, increase number
        if (tile.isSelected() && userChange) {
            commitChangesToUndo();
            tile.incrementNum();

            //updates the grid based on changes made to the tile
            updateGrid(tile);

            tileToCommitToUndo = tile;
            numToCommitToUndo = tile.getNumber();

            if (checkIfWon()) displayWinAnimation();
        }
        //otherwise select the new tile
        else {
            selectTile(tile);

            //only commit changes to undo stack if user changed selection
            if (userChange) {
                commitChangesToUndo();
                tileToCommitToUndo = tile;
                numToCommitToUndo = tile.getNumber();
            }
        }
    }


    public void clearBoard() {
        for (int i = 0; i < dimensions * dimensions; i++) {
            GridSquare tile = gridRepresentation[i];

            //removing the value from each tile
            tile.clearNum();
            //drawing each tile normally
            tile.drawNormal();
            //flagging each tile as causing no mistakes
            tile.flagNoMistakes();
        }
    }


    public Integer getDimensions() {
        return dimensions;
    }


    public ArrayList<Cage> getCages() {
        return cages;
    }

    public Container[] getRows() {return rows;}

    public Container[] getColumns() {return columns;}


    public GridSquare[] getAllTiles() {
        return gridRepresentation;
    }


    //updates tiles flags and returns true if a mistake was detected
    private boolean mistakeDetection(GridSquare tile) {
        Cage tileCage = tile.getCage();
        Column tileColumn = tile.getColumn();
        Row tileRow = tile.getRow();

        boolean mistakeDetected = false;

        //checking the tiles cage to see if there are any mistakes and if show mistakes is on
        //if so mark mistakes and unselect currently selected tile
        //otherwise update the tiles colouring
        if (tile.checkCageForMistakes()) {
            tileCage.flagMistakes(true);
            mistakeDetected = true;
        }
        else {
            tileCage.flagMistakes(false);
        }

        //checking the tiles column to see if there are any mistakes and if show mistakes is on
        //if so mark mistakes and unselect currently selected tile
        //otherwise hide any previous markings
        if (tile.checkColumnForMistakes()) {
            tileColumn.flagMistakes(true);
            mistakeDetected = true;
        }
        else {
            tileColumn.flagMistakes(false);
        }

        //checking the tiles row to see if there are any mistakes and if show mistakes is on
        //if so mark mistakes and unselect currently selected tile
        //otherwise hide any previous markings
        if (tile.checkRowForMistakes()) {
            tileRow.flagMistakes(true);
            mistakeDetected = true;
        }
        else {
            tileRow.flagMistakes(false);
        }
        
        return mistakeDetected;
    }

    //updates the grid based on a tile that has changed
    public void updateGrid(GridSquare tile) {
        //updating the mistake flags of all tiles
        boolean mistakeDetected = mistakeDetection(tile);

        //draw tiles with updated flags if show mistakes is selected
        if (mistakeToggle.isSelected()) {
            tile.getCage().colourMistakes();
            tile.getRow().colourMistakes();
            tile.getColumn().colourMistakes();
        }
        //otherwise hide mistakes
        else {
            tile.getCage().hideMistakes();
            tile.getRow().hideMistakes();
            tile.getColumn().hideMistakes();
        }

        //if a mistake was detected, unselect the current tile
        //otherwise no mistakes were detected, so hide all row, column and cage mistakes
        if (mistakeDetected) selectTile(tile);
    }


    //returns true if there is a tile selected
    public boolean isATileSelected() {
        if (currentlySelected != null) return true;
        else return false;
    }


    //returns whether the grid created is valid
    public boolean isValid() {
        return valid;
    }


    public String getInvalidMessage() {
        return invalidMessage;
    }


    //adds a tile to the correct row and column
    protected void addTileToRowAndColumn(GridSquare tile, int x, int y) {
        //making new row and column containers for error detection
        if (columns[x] == null) {
            columns[x] = new Column(dimensions, mistakeToggle);
        }
        columns[x].addTile(tile);
        tile.setColumn(columns[x]);
        if (rows[y] == null) {
            rows[y] = new Row(dimensions, mistakeToggle);
        }
        rows[y].addTile(tile);
        tile.setRow(rows[y]);
    }


    //sets the width and height of each tile
    protected void setDimensionsOfTiles(int widthHeight) {
        for (int x = 0; x < dimensions * dimensions; x++) {
            //getting the current tile
            GridSquare currentTile = gridRepresentation[x];

            currentTile.setWidth(widthHeight);
            currentTile.setHeight(widthHeight);
        }
    }


    //draws all the tiles in the grid
    protected void drawTiles() {
        //drawing all the tiles in the grid with the appropriate thickness
        for (int x = 0; x < dimensions * dimensions; x++) {
            GridSquare currentTile = gridRepresentation[x];

            currentTile.deduceThickness();

            currentTile.drawRect();
        }
    }


    //sets the row and column restraints
    protected void setRowAndColumnRestraints() {
        for (int i = 0; i < dimensions; i++) {
            RowConstraints row = new RowConstraints();
            row.setVgrow(Priority.NEVER);

            getRowConstraints().add(row);

            ColumnConstraints column = new ColumnConstraints();
            column.setHgrow(Priority.NEVER);

            getColumnConstraints().add(column);
        }
    }


    //returns true if the grid has been completed correctly
    protected boolean checkIfWon() {
        Iterator<Cage> cageIterator = cages.iterator();
        while (cageIterator.hasNext()) {
            Cage cage = cageIterator.next();

            if (!cage.isFull()) return false;
            if (!cage.doesCageMeetTarget()) return false;
        }

        return true;
    }



    //displays the win animation
    public void displayWinAnimation() {
        completed = true;
        //animates the colour of each diagonal one at a time
        for (int i = 1; i <= (2 * dimensions) - 1; i++) {
            ArrayList<GridSquare> tilesToAnimate = new ArrayList<>();
            GridSquare tile;
            //moving along the left handside of the grid
            if (i <= dimensions) {
                tile = getTileBy2DLocation(1, i);
            }
            //moving along the bottom row of the grid
            else {
                tile = getTileBy2DLocation(i - dimensions + 1, dimensions);
            }
            tilesToAnimate.add(tile);
            boolean diagonalTileExists = true;
            while (diagonalTileExists) {
                if (!tile.doesRightTileExist()) diagonalTileExists = false;
                else if (!tile.doesAboveTileExist()) diagonalTileExists = false;
                else {
                    tile = tile.getRightTile().getAboveTile();
                    tilesToAnimate.add(tile);
                }
            }

            Iterator<GridSquare> tileIterator = tilesToAnimate.iterator();
            while (tileIterator.hasNext()) {
                GridSquare tileToAnimate = tileIterator.next();

                tileToAnimate.startWinAnimation();
            }


            try{Thread.sleep(500);}
            catch (InterruptedException e) {}


        }
    }

    //displays the win animation
    public void stopWinAnimation() {
        completed = false;
        //animates the colour of each diagonal one at a time
        for (int i = 0; i < dimensions * dimensions; i++) {
            gridRepresentation[i].stopWinAnimation();
        }
    }


    public void prettyPrintGrid() {
        for (int i = 1; i <= dimensions * dimensions; i++) {
            if (i % dimensions == 0) System.out.println();
        }
    }

    public boolean isCompleted() {
        return completed;
    }
}
