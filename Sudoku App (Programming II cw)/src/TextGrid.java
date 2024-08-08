import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.text.Text;

import java.util.*;

public class TextGrid extends Grid {
    //a list of locations maintained to check if a tile is repeated
    protected ArrayList<Integer> locations;


    public TextGrid(String text, boolean loadedFromFile, CheckBox mistakeToggle) {
        super(mistakeToggle);

        createGrid(text, loadedFromFile);

        checkIfGridIsValid();
    }

    private void createGrid(String text, boolean loadedFromFile) {
        //keeps track of the largest location in the grid to calculate the grid dimension
        int highest = 0;

        //array list of integer array lists, containing all cage locations
        cages = new ArrayList<>();

        //list of all the locations in the grid
        locations = new ArrayList<>();

        //creating an interpreter to read the given text
        TextInterpreter interpreter = new TextInterpreter(text, loadedFromFile);
        interpreter.processNextLine();

        //loops through setup file extracting location of cages
        while (interpreter.hasNext()) {
            interpreter.resetLocationCount();

            Cage newCage = new Cage(mistakeToggle, this);

            //adding the new cage to the list of cages
            cages.add(newCage);

            //creating an array list to store current cages values
            ArrayList<Integer> cageValues = new ArrayList<>();

            interpreter.processNextCage();

            //for every location in this cage
            while (interpreter.hasLocation()) {
                //fetch current location in cage
                Integer cageLocation = interpreter.getCurrentCageLocation();

                //adding the new location to the list of locations
                locations.add(cageLocation);

                //creating a new tile
                GridSquare tile = new GridSquare(cageLocation, newCage, this);

                //adding current tile to current cage
                newCage.addTile(tile);

                //giving the tile a pointer to its cage
                tile.setCage(newCage);

                //if its the new highest location store it
                if (cageLocation > highest) highest = cageLocation;

                //adding event handlers for when the tile is clicked
                tile.setOnMouseClicked(e -> {
                    changeSelection(tile, true);
                });

                //processing the text describing the next cage
                interpreter.processNextCage();
            }

            //sets the target and operator of the new cage
            String target = interpreter.getCageTarget();
            int targetValue;
            if (newCage.getNumberOfTiles() > 1) {
                String targetInfo[] = target.split("(?=[+÷\\-x]+)");
                targetValue = Integer.parseInt(targetInfo[0]);
                Character operator = targetInfo[1].charAt(0);
                newCage.setOperator(operator);
            }
            else {
                targetValue = Integer.parseInt(target);
            }
            newCage.setTarget(target);
            newCage.setTargetValue(targetValue);


            //setting the first tile of each cage to the appropriate operator
            newCage.getOperatorTile().setTarget(newCage.getTarget());

            //processing the next line of the text
            interpreter.processNextLine();

        }
        //calculating the dimensions of the grid
        dimensions = (int)Math.sqrt(highest);


        gridRepresentation = new GridSquare[highest];
        columns = new Column[dimensions];
        rows = new Row[dimensions];

        //storing tiles in the grip respresntation and grid pane
        Iterator<Cage> cageIterator = cages.iterator();
        while (cageIterator.hasNext()) {
            Cage thisCage = cageIterator.next();

            Iterator<GridSquare> tileIterator = thisCage.getTiles().iterator();
            while (tileIterator.hasNext()) {
                GridSquare thisTile = tileIterator.next();

                Integer tileLocation = thisTile.getLocation();

                gridRepresentation[tileLocation-1] = thisTile;

                //calculating each tiles 2d coordinates
                thisTile.calculateLocation();

                //fetching the tiles x and y positions
                int xpos = thisTile.getX2d();
                int ypos = thisTile.getY2d();
                addTileToRowAndColumn(thisTile, xpos, ypos);
                add(thisTile, xpos, ypos);
            }

            Text target = thisCage.getOperatorTile().getTarget();

            add(target, thisCage.getOperatorTile().getX2d(), thisCage.getOperatorTile().getY2d());
            GridPane.setHalignment(target, HPos.LEFT);
            GridPane.setValignment(target, VPos.TOP);
            GridPane.setMargin(target, new Insets(5,0,0,5));
        }


        //setting the dimenions of each tile
        setDimensionsOfTiles(500/dimensions);

        //drawing all the tiles
        drawTiles();

        //setting row and column restraints
        setRowAndColumnRestraints();


        undoStack = new Stack();
        redoStack = new Stack();


        //solving the grid
        solver.solveGridTileByTile();
    }


    //checking for errors in the created grid
    private void checkIfGridIsValid() {
        //if a cage contains tiles that are not adjacent return false, do not create grid
        Iterator<Cage> cageIterator = cages.iterator();
        while (cageIterator.hasNext()) {
            Cage thisCage = cageIterator.next();

            if (!thisCage.areTilesAdjacent()) {
                valid = false;
                invalidMessage = "Cages Can Only Contain Adjacent Tiles";
                break;
            }
        }

        //if a tile is a part of more than one cage it will appear more than once in
        //the list of locations
        Set locationSet = new HashSet(locations);
        //if values are repeated than the set will contain fewer elements than the list
        if (locationSet.size() < locations.size()) {
            valid = false;
            invalidMessage += "\nTiles Cannot Appear Multiple Times Or In More Than One Cage";
        }
    }
}
