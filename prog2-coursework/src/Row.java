import javafx.scene.control.CheckBox;

import java.util.ArrayList;
import java.util.Iterator;

public class Row extends Container {
    protected int dimensions;

    //constructor to be used by rows and columns
    public Row(int dimensions, CheckBox mistake) {
        super(mistake);

        this.dimensions = dimensions;
        this.mistakeToggle = mistake;

        tiles = new ArrayList<>();

        correct = 1;
    }


    public void flagMistakes(boolean flag) {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //flagging the mistake as row
            tile.flagMistakeRow(flag);
        }
    }


    public ArrayList<Integer> getPossibleValuesLeft() {
        ArrayList<Integer> valuesUsed = new ArrayList<>();

        for (int i = 0; i < tiles.size(); i++) {
            GridSquare tile = tiles.get(i);

            valuesUsed.add(tile.getNumber());
        }

        ArrayList<Integer> valuesLeft = new ArrayList<>();

        for (Integer i = 1; i <= dimensions; i++) {
            //if a value hasn't been used, add it to the list of possible values
            if (!valuesUsed.contains(i)) {
                valuesLeft.add(i);
            }
        }

        return valuesLeft;
    }
}
