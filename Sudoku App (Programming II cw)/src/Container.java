import javafx.scene.control.CheckBox;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;

public class Container {

    protected ArrayList<GridSquare> tiles;

    protected CheckBox mistakeToggle;

    //0 means incorrect
    //1 means container incomplete
    //2 means container correct
    protected Integer correct;

    //constructor to be used by cages
    public Container(CheckBox mistake) {
        this.mistakeToggle = mistake;

        tiles = new ArrayList<>();

        correct = 1;
    }


    //adds a tile to the container
    public void addTile(GridSquare tile) {
        tiles.add(tile);
    }


    //returns true if there are any repeating digits in the container
    public boolean isRepeats() {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            Integer value = tile.getNumber();

            //making a reverse iterator
            ListIterator<GridSquare> tileReverse = tiles.listIterator(tiles.size());

            //iterate in reverse through list checking for repetitions
            while (tileReverse.hasPrevious()) {
                GridSquare reverseTile = tileReverse.previous();

                //if the value is repeated and it is not checking itself, and the value is not 0, return true
                if (value == reverseTile.getNumber() && tile != reverseTile && value != 0) {
                    return true;
                }
            }
        }

        //otherwise there are no repeats, return false7
        return false;
    }


    //tests if all tiles in the container have a number entered
    //returns true if cage is full, otherwise false
    public boolean isFull() {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //if a cell is empty return false
            if (tile.getNumber() == 0) return false;
        }

        //otherwise the cage is full
        return true;
    }


    //for each tile in the container, checks if the tile is causing any mistakes and colours it appropriately
    public void colourMistakes() {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //only colour tile if it is not selected
            if (!tile.isSelected()) {
                tile.drawMistakes();
            };
        }
    }


    //hides all mistakes by colouring tiles normally
    public void hideMistakes() {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //only mark tile as selected if it is not selected
            if (!tile.isSelected())  tile.drawNormal();
        }
    }


    public void setCorrect(Integer value) {
        correct = value;
    }

    public Integer containerStatus() {
        return correct;
    }


    public boolean isMistakeToggleOn() {
        return mistakeToggle.isSelected();
    }


    //clears the container
    public void clear() {
        tiles.clear();
    }


    public int getNumberOfTiles() {
        return tiles.size();
    }


    public void setTileValuesTo0() {
        Iterator<GridSquare> tileIterator = tiles.iterator();
        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            tile.setNum(0);
        }
    }
}
