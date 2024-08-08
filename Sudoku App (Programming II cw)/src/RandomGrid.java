import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Text;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.Stack;

//a type of grid that is randomly generated
public class RandomGrid extends Grid {

    public RandomGrid(CheckBox mistakeToggle, int gridSize) {
        super(mistakeToggle);

        createGrid(gridSize);
    }

    private void createGrid(int gridSize) {
        Random rand = new Random();

        dimensions = gridSize;

        //filling the grid with values so that there is one of each number per row and column
        //if the grid cannot be filled in with the choices made then a new grid is generated
        boolean validGrid = false;
        while (!validGrid) {
            //clearing the grid of previous attempts
            getChildren().clear();

            //clearing rows and columns if multiple attempts are tried
            rows = new Row[dimensions];
            columns = new Column[dimensions];

            //clearing the grid representation if multiple attempts are tried
            gridRepresentation = new GridSquare[dimensions*dimensions];

            validGrid = true;

            for (int x = 1; x <= dimensions * dimensions; x++) {
                GridSquare tile = new GridSquare(x, this);

                //fetching the tiles x and y positions
                int xpos = tile.getX2d();
                int ypos = tile.getY2d();
                addTileToRowAndColumn(tile, xpos, ypos);

                //values possible in each row and column
                ArrayList<Integer> rowValuesPossible = rows[ypos].getPossibleValuesLeft();
                ArrayList<Integer> columnValuesPossible = columns[xpos].getPossibleValuesLeft();
                ArrayList<Integer> valuesPossible = new ArrayList<>(rowValuesPossible);
                //the list of possible values the tile could be
                valuesPossible.retainAll(columnValuesPossible);

                //if there are no possiblities left then the grid is invalid
                if (valuesPossible.size() == 0) {
                    validGrid = false;
                    break;
                }

                //randomly selecting a value from the list of possible values
                int valuePositionToUse = rand.nextInt(valuesPossible.size());
                tile.setNum(valuesPossible.get(valuePositionToUse));

                add(tile, tile.getX2d(), tile.getY2d());
                gridRepresentation[x-1] = tile;

                //adding event handlers for when the tile is clicked
                tile.setOnMouseClicked(e -> {
                    changeSelection(tile, true);
                });
            }

        }


        //initialising the list of cages
        cages = new ArrayList<>();

        //generating new cages for each tile that does not have a cage
        for (int i = 0; i < dimensions * dimensions; i++) {
            GridSquare tile = gridRepresentation[i];

            //if the tile hasn't already been assigned a cage
            if (tile.getCage() == null) {
                Cage newCage = new Cage(mistakeToggle, this);

                //generating a new cage from a starting tile
                newCage.generate(tile);

                cages.add(newCage);
            }
        }


        //the algorithm has a chance to merge any cages of size of 1 into adjacent cages
        Iterator <Cage> cageIterator = cages.iterator();
        ArrayList<Cage> cagesToRemove = new ArrayList<>();
        while(cageIterator.hasNext()) {
            Cage currentCage = cageIterator.next();

            //if a cage has a size of 1 and is not a cage to be removed
            if (currentCage.getTiles().size() == 1 && !cagesToRemove.contains(currentCage)) {
                //fetching the one tile in the cage
                GridSquare tile = currentCage.getTiles().get(0);

                int chanceToMerge = rand.nextInt(100);

                //there is a chance the tile will merge with an adjacent cage
                if (chanceToMerge < tile.getProbabilityOfMerging()) {
                    //choosing a random adjacent tile
                    ArrayList<GridSquare> adjacentTiles = tile.getAdjacentTiles();

                    //making a list of valid adjacent tiles to merge
                    ArrayList<GridSquare> validAdjacents = new ArrayList<>();
                    Iterator <GridSquare> adjacentIterator = adjacentTiles.iterator();
                    while (adjacentIterator.hasNext()) {
                        GridSquare adjacentTile = adjacentIterator.next();
                        if (!adjacentTile.getCage().isAtMaxSize()) validAdjacents.add(adjacentTile);
                    }
                    //merge cages if there are any valid adjacent tiles
                    if (validAdjacents.size() > 0) {
                        GridSquare adjacentTile = validAdjacents.get(rand.nextInt(validAdjacents.size()));

                        //merging tiles cages
                        cagesToRemove.add(tile.getCage());
                        mergeTilesCages(adjacentTile, tile);
                    }
                }
            }
        }

        //removing any now empty cages
        cageIterator = cagesToRemove.iterator();
        while (cageIterator.hasNext()) {
            Cage currentCage = cageIterator.next();
            cages.remove(currentCage);
        }


        //generating a target for each cage
        cageIterator = cages.iterator();
        while (cageIterator.hasNext()) {
            Cage currentCage = cageIterator.next();

            currentCage.generateTarget();
        }


        //adding the targets of each cage to the grid
        cageIterator = cages.iterator();
        while (cageIterator.hasNext()) {
            Cage thisCage = cageIterator.next();

            Text target = thisCage.getOperatorTile().getTarget();

            add(target, thisCage.getOperatorTile().getX2d(), thisCage.getOperatorTile().getY2d());
            GridPane.setHalignment(target, HPos.LEFT);
            GridPane.setValignment(target, VPos.TOP);
            GridPane.setMargin(target, new Insets(5,0,0,5));
        }


        //clearing all the tiles of their numbers
        for (int i = 0; i < dimensions * dimensions; i++) {
            GridSquare tile = gridRepresentation[i];

            tile.setNum(0);
        }


        //setting the dimensions of each tile
        setDimensionsOfTiles(500/dimensions);

        //draws all the tiles
        drawTiles();

        //setting row and column restraints
        setRowAndColumnRestraints();

        undoStack = new Stack();
        redoStack = new Stack();

        //solving the grid
        solver.solveGridTileByTile();
    }


    //merges two tiles cages
    private void mergeTilesCages(GridSquare tile1, GridSquare tile2) {
        //adding the adjacent tile to the other tiles cage
        tile2.setCage(tile1.getCage());
        tile1.getCage().addTile(tile2);
    }
}
