import javafx.scene.control.CheckBox;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Random;

public class Cage extends Container {
    private String target;
    private Character operator;
    private Integer targetValue;

    //constant representing the percentage chance of the cage growing in a direction
    private final Integer chanceOfGrowth = 20;
    //constant representing the maximum size of a cage
    private final Integer maxCageSize = 6;

    private Grid grid;

    ArrayList<Integer[]> possibleSolutions;

    private ArrayList<ArrayList<Integer>> permutations;

    public Cage(CheckBox mistakes, Grid g) {
        //passing mistake checkbox to container constructor
        super(mistakes);

        tiles = new ArrayList<>();
        this.grid = g;
    }

    public ArrayList<GridSquare> getTiles() {
        return tiles;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public String getTarget() {
        return target;
    }

    public void setTargetValue(Integer target) {
        targetValue = target;
    }

    public Integer getTargetValue() {
        return targetValue;
    }

    public void setOperator(Character operator) {
        this.operator = operator;
    }

    public Character getOperator() {
        return operator;
    }


    public GridSquare getOperatorTile() {
        return tiles.get(0);
    }


    public boolean doesCageMeetTarget() {
        //creating an array of tile values to be tested
        int tileValues[] = getIntArrayOfTiles();

        //if the cage only contains one tile it only needs to check if the value meets the target
        if (tiles.size() == 1) {
            if (tiles.get(0).getNumber() == targetValue) return true;
        }
        //otherwise all permutations of values in the cage need to be tested with the cages operator
        else {
            /*
            AllPermutation perm = new AllPermutation(tileValues);
            int[] permutation;
            permutation = perm.GetFirst();
            if (doesPermutationMeetTarget(permutation)) return true;
            while (perm.HasNext())
            {
                permutation = perm.GetNext();
                if (doesPermutationMeetTarget(permutation)) return true;
            }
            */
            permutations = new ArrayList<ArrayList<Integer>>();
            heapPermutations(tileValues, tileValues.length, tileValues.length);

            Iterator<ArrayList<Integer>> permutationIterator = permutations.iterator();
            while (permutationIterator.hasNext()) {
             ArrayList<Integer> permutation = permutationIterator.next();

             int[] permutationArray = new int[permutation.size()];
             for (int i = 0; i < permutation.size(); i++) {
                 permutationArray[i] = permutation.get(i);
             }

             if (doesPermutationMeetTarget(permutationArray)) return true;
            }
        }

        //if the contents of the cage has not met the target then return false
        return false;
    }


    //Generating permutation using Heaps Algorithm
    private void heapPermutations(int a[], int size, int n)
    {
        //if size becomes 1 then test permutation to see if it meets the cage target
        if (size == 1) {
            //add the permutation to the list of permutations
            ArrayList<Integer> toBeAdded = new ArrayList<>();
            for (int i = 0; i < a.length; i++) {
                toBeAdded.add(a[i]);
            }
            permutations.add(toBeAdded);
        }

        for (int i=0; i<size; i++)
        {
            heapPermutations(a, size-1, n);

            //if size is odd, swap first and last
            //element
            if (size % 2 == 1)
            {
                int temp = a[0];
                a[0] = a[size-1];
                a[size-1] = temp;
            }

            //if size is even, swap ith and last
            //element
            else
            {
                int temp = a[i];
                a[i] = a[size-1];
                a[size-1] = temp;
            }
        }
    }


    //swaps two indexes of an array
    private void swap(Integer[] input, int a, int b) {
        Integer tmp = input[a];
        input[a] = input[b];
        input[b] = tmp;
    }

    //returns an array of the values of the tiles in the cage
    private int[] getIntArrayOfTiles() {
        int[] values = new int[tiles.size()];

        for (int i = 0; i < tiles.size(); i++) {
            GridSquare tile = tiles.get(i);

            values[i] = tile.getNumber();
        }

        return values;
    }

    //tests a permutation to see if it meets the target value
    private boolean doesPermutationMeetTarget(int[] values) {
        int total = chainOperatorOnValues(getOperator(), values);

        if (total == targetValue) return true;
        else return false;
    }

    //chains the given operator on all the values
    private int chainOperatorOnValues(Character operator, int[] values) {
        int total = 0;
        
        //if addition then the total starts at 0
        if (operator == '+') total = 0;
            //if multiplication then the total starts at 1
        else if (operator == 'x') total = 1;
            //otherwise if division or subtraction, then the total starts at the value of the first number in the permutation
        else if (operator == '\u00F7' || operator == '-') total = values[0];


        for (int i = 0; i < values.length; i++) {
            //if division or subtraction then start one ahead
            if ((operator == '\u00F7' || operator == '-') && i == 0);

            else if (operator == '+') total = total + values[i];
            else if (operator == 'x') total = total * values[i];
            else if (operator == '-') total = total - values[i];
            else if (operator == '\u00F7') total = total / values[i];
        }
        
        return total;
    }


    //returns whether all tiles within the cage are adjacent
    public boolean areTilesAdjacent() {

        GridSquare initialTile = tiles.get(0);

        //traversing through the tiles in the cage
        visitTilesInCage(initialTile);

        Iterator<GridSquare> tileIterator = tiles.iterator();
        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //if any tiles haven't been visited then return false
            if (!tile.hasBeenVisited()) {
                clearVisitFlags();
                return false;
            }
        }

        //otherwise all tiles are adjacent and return true
        clearVisitFlags();
        return true;
    }

    //all tiles are adjacent if a tile in the cage can visit all other tiles in the cage
    //by only traversing through adjacent tiles in the same cage
    public void visitTilesInCage(GridSquare tile) {
        //flag the tile as visited
        tile.setVisited(true);

        //if the tile has a tile above it
        if (tile.doesAboveTileExist()) {
            //if that tile is a part of this cage and hasn't already been visited
            if (tile.getAboveTile().getCage() == this && !tile.getAboveTile().hasBeenVisited()) {
                //visit the above tile
                visitTilesInCage(tile.getAboveTile());
            }
        }

        //if the tile has a tile to the right of it
        if (tile.doesRightTileExist()) {
            //if that tile is a part of this cage
            if (tile.getRightTile().getCage() == this && !tile.getRightTile().hasBeenVisited()) {
                //visit the right tile
                visitTilesInCage(tile.getRightTile());
            }
        }

        //if the tile has a tile below it
        if (tile.doesBelowTileExist()) {
            //if that tile is a part of this cage
            if (tile.getBelowTile().getCage() == this && !tile.getBelowTile().hasBeenVisited()) {
                //visit the below tile
                visitTilesInCage(tile.getBelowTile());
            }
        }

        //if the tile has a tile to the left of it
        if (tile.doesLeftTileExist()) {
            //if that tile is a part of this cage
            if (tile.getLeftTile().getCage() == this && !tile.getLeftTile().hasBeenVisited()) {
                //visit the left tile
                visitTilesInCage(tile.getLeftTile());
            }
        }
    }

    //clears all visited flags in the cage
    private void clearVisitFlags() {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //setting visit flags to false
            tile.setVisited(false);
        }
    }


    public void flagMistakes(boolean flag) {
        Iterator<GridSquare> tileIterator = tiles.iterator();

        while (tileIterator.hasNext()) {
            GridSquare tile = tileIterator.next();

            //flagging the mistake as cage
            tile.flagMistakeCage(flag);
        }
    }


    //generates a new cage on the grid using the first assigned tile
    public void generate(GridSquare startingTile) {
        //adding the new tile to the cage
        addTile(startingTile);
        startingTile.setCage(this);

        spreadGrowth(startingTile);
    }

    private void spreadGrowth(GridSquare tile) {
        //grow the cage
        if (tile.doesRightTileExist()) growCage(tile.getRightTile());

        if (tile.doesBelowTileExist()) growCage(tile.getBelowTile());

        if (tile.doesLeftTileExist()) growCage(tile.getLeftTile());

        if (tile.doesAboveTileExist()) growCage(tile.getAboveTile());
    }

    //tests if the cage is valid to grow
    private boolean testGrowth(GridSquare tile) {
        //generating a random number between 0 and 99
        Random rand = new Random();
        int randInt = rand.nextInt(100);

        //do not grow the cage if the generator has not met the probability
        if (randInt > chanceOfGrowth) return false;
        //do not grow the cage if the tile already has a cage
        if (tile.getCage() != null) return false;
        //do not grow the cage if the cage is at max capacity
        if (tiles.size() > maxCageSize) return false;

        return true;
    }

    //grows the cage during generation
    private void growCage(GridSquare tile) {
        //do not grow the cage if it does not meet the requirements
        if (!testGrowth(tile)) return;

        //adding the new tile to the cage
        addTile(tile);
        tile.setCage(this);
    }


    public void generateTarget() {
        //if the cage only contains one tile, then the target of the cage will be the value of the tile
        if (tiles.size() == 1) {
            Integer targetValue = tiles.get(0).getNumber();
            setTarget(Integer.toString(targetValue));
            setTargetValue(targetValue);
            getOperatorTile().setTarget(getTarget());
        }
        //otherwise a operator and target need to be selected
        else {
            //randomly selecting an operator and permutation

            permutations = new ArrayList<>();

            int[] tileValues = getIntArrayOfTiles();
            heapPermutations(tileValues, tileValues.length, tileValues.length);

            ArrayList<ArrayList<Integer>> divisionPermutations = getDivisionPermutations();
            ArrayList<ArrayList<Integer>> subtractionPermutations = getSubtractionPermutations();

            /*

            boolean divisionPossible = false;
            boolean subtractionPossible = false;

            //checking if subtraction or division is possible for the values in the cage
            int[] tileValues = getIntArrayOfTiles();
            AllPermutation perm = new AllPermutation(tileValues);
            int[] permutation;
            permutation = perm.GetFirst();
            if (isDivisionPossible(permutation)) divisionPossible = true;
            if (isSubtractionPossible(permutation)) subtractionPossible = true;
            while (perm.HasNext())
            {
                permutation = perm.GetNext();
                if (isDivisionPossible(permutation)) divisionPossible = true;
                if (isSubtractionPossible(permutation)) subtractionPossible = true;
            }

             */

            ArrayList<Integer> permutation = new ArrayList<>();

            Random rand = new Random();

            //choosing an operator based on what permutations are possible
            boolean validOperator = false;
            while (!validOperator) {
                validOperator = true;
                int operatorToUse = rand.nextInt(4);
                switch (operatorToUse) {
                    /*
                    case 0:
                        setOperator('x');
                        permutation = getRandomPermutation();
                        break;
                    case 1:
                        setOperator('+');
                        permutation = getRandomPermutation();
                        break;
                    case 2:
                        //if no division or subtraction permutations are possible then choose a new operator
                        if (!isDivisionPossible(tileValues)) validOperator = false;
                        else {
                            setOperator('÷');
                            permutation = getRandomDivisionPermutation();
                        }
                        break;
                    case 3:
                        if (isSubtractionPossible(tileValues)) validOperator = false;
                        else {
                            setOperator('-');
                            permutation = getRandomSubtractionPermutation();
                        }
                        break;

                     */
                    case 0:
                        setOperator('x');
                        permutation = getRandomPermutation();
                        break;
                    case 1:
                        setOperator('+');
                        permutation = getRandomPermutation();
                        break;
                    case 2:
                        //if no division or subtraction permutations are possible then choose a new operator
                        if (divisionPermutations.size() == 0) validOperator = false;
                        else {
                            setOperator('\u00F7');
                            permutation = divisionPermutations.get(rand.nextInt(divisionPermutations.size()));
                        }
                        break;
                    case 3:
                        if (subtractionPermutations.size() == 0) validOperator = false;
                        else {
                            setOperator('-');
                            permutation = subtractionPermutations.get(rand.nextInt(subtractionPermutations.size()));
                        }
                        break;
                }
            }

            int[] permutationArray = new int[permutation.size()];
            for (int i = 0; i < permutation.size(); i++) {
                permutationArray[i] = permutation.get(i);
            }

            int targetValue = chainOperatorOnValues(getOperator(), permutationArray);
            setTargetValue(targetValue);
            setTarget(Integer.toString(targetValue) + operator);
            getOperatorTile().setTarget(getTarget());
        }
    }

    /*
    private int[] getRandomPermutation() {
        Random rand = new Random();

        int[] last = new int[tiles.size()];
        int[] tileValues = getIntArrayOfTiles();
        AllPermutation perm = new AllPermutation(tileValues);
        int[] permutation;
        permutation = perm.GetFirst();
        if ((rand.nextInt(10) > 1)) {
            last = permutation;
            return permutation;
        }
        while (perm.HasNext())
        {
            permutation = perm.GetNext();
            if ((rand.nextInt(10) > 1)) {
                last = permutation;
                return permutation;
            }
        }

        return last;
    }

    //returns true if division is possible
    private boolean isDivisionPossible(int[] permutation) {
        int total = permutation[0];
        boolean validDivisionPermutation = true;
        for (int i = 1; i < permutation.length; i++) {
            //if the division chain doesn't give an integer at any point then it is not a valid permutation
            if (total % permutation[i] != 0) {
                return false;
            }
            else total = total / permutation[i];
        }

        return true;
    }

    private int[] getRandomDivisionPermutation() {
        Random rand = new Random();

        int[] last = new int[tiles.size()];
        int[] tileValues = getIntArrayOfTiles();
        AllPermutation perm = new AllPermutation(tileValues);
        int[] permutation;
        permutation = perm.GetFirst();
        if (isDivisionPossible(permutation) && (rand.nextInt(3) > 1)) {
            last = permutation;
            return permutation;
        }
        while (perm.HasNext())
        {
            permutation = perm.GetNext();
            if (isDivisionPossible(permutation) && (rand.nextInt(3) > 1)) {
                last = permutation;
                return permutation;
            }
        }

        return last;
    }

    //returns true if subtraction is possible
    private boolean isSubtractionPossible(int[] permutation) {
        int total = permutation[0];
        boolean validSubtractionPermutation = true;
        for (int i = 1; i < permutation.length; i++) {
            //if the division chain doesn't give an integer at any point then it is not a valid permutation
            if (total - permutation[i] < 0) {
                return false;
            }
            else total = total - permutation[i];
        }

        return true;
    }

    private int[] getRandomSubtractionPermutation() {
        Random rand = new Random();

        int[] last = new int[tiles.size()];
        int[] tileValues = getIntArrayOfTiles();
        AllPermutation perm = new AllPermutation(tileValues);
        int[] permutation;
        permutation = perm.GetFirst();
        if (isSubtractionPossible(permutation) && (rand.nextInt(3) > 1)) {
            last = permutation;
            return permutation;
        }
        while (perm.HasNext())
        {
            permutation = perm.GetNext();
            if (isSubtractionPossible(permutation) && (rand.nextInt(3) > 1)) {
                last = permutation;
                return permutation;
            }
        }

        return last;
    }
     */


    //returns all permutations that will be valid for division when generating the cage
    private ArrayList<ArrayList<Integer>> getDivisionPermutations() {
        ArrayList<ArrayList<Integer>> divisionPermutations = new ArrayList<>();

        Iterator<ArrayList<Integer>> permutationIterator = permutations.iterator();
        while (permutationIterator.hasNext()) {
            ArrayList<Integer> permutation = permutationIterator.next();

            int total = permutation.get(0);
            boolean validDivisionPermutation = true;
            for (int i = 1; i < permutation.size(); i++) {
                //if the division chain doesn't give an integer at any point then it is not a valid permutation
                if (total % permutation.get(i) != 0) {
                    validDivisionPermutation = false;
                    break;
                }
                else total = total / permutation.get(i);
            }

            if (validDivisionPermutation) divisionPermutations.add(permutation);
        }

        return divisionPermutations;
    }


    //returns all permutations that will be valid for subtraction when generating the cage
    private ArrayList<ArrayList<Integer>> getSubtractionPermutations() {
        ArrayList<ArrayList<Integer>> subtractionPermutations = new ArrayList<>();

        Iterator<ArrayList<Integer>> permutationIterator = permutations.iterator();
        while (permutationIterator.hasNext()) {
            ArrayList<Integer> permutation = permutationIterator.next();

            int total = permutation.get(0);
            boolean validSubtractionPermutation = true;
            for (int i = 1; i < permutation.size(); i++) {
                //if the division chain doesn't give an integer at any point then it is not a valid permutation
                if (total - permutation.get(i) < 0) {
                    validSubtractionPermutation = false;
                    break;
                }
                else total = total - permutation.get(i);
            }

            if (validSubtractionPermutation) subtractionPermutations.add(permutation);
        }

        return subtractionPermutations;
    }

    private ArrayList<Integer> getRandomPermutation() {
        Random rand = new Random();

        return permutations.get(rand.nextInt(permutations.size()));
    }


    //returns true if the cage is at its max size
    public boolean isAtMaxSize() {
        if (tiles.size() >= maxCageSize) return true;
        else return false;
    }


    public ArrayList<Integer[]> getPossibleSolutionsToCage() {
        possibleSolutions = new ArrayList<>();

        for (final String val : baseCounting(grid.dimensions, getNumberOfTiles())) {
            Integer[] values = new Integer[val.length()];
            for (int i = 0; i < val.length(); i++) {
                values[i] = Character.getNumericValue(val.charAt(i)) + 1;
            }
            enterArrayIntoTiles(values);

            boolean valid = true;
            if (!doesCageMeetTarget()) {
                valid = false;
            } else {
                Iterator<GridSquare> tileIterator = tiles.iterator();
                while (tileIterator.hasNext()) {
                    GridSquare tile = tileIterator.next();
                    if (tile.getRow().isRepeats()) {
                        //System.out.println(Arrays.toString(values));
                        valid = false;
                    } else if (tile.getColumn().isRepeats()) {
                        valid = false;
                    }
                }
            }

            if (valid) possibleSolutions.add(values);
        }

        setTileValuesTo0();

        return possibleSolutions;
    }

    private static Iterable<String> baseCounting(final int radix, final int digits) {
        return new Iterable<String>() {

            public Iterator<String> iterator() {
                return new Iterator<String>() {

                    private final String pad;
                    {
                        final StringBuilder buf = new StringBuilder(digits);
                        for (int n = digits; n >= 0; --n) {
                            buf.append('0');
                        }
                        pad = buf.toString();
                    }

                    private final int hi = (int) Math.pow(radix, digits);
                    private int cursor;

                    public boolean hasNext() {
                        return cursor < hi;
                    }

                    public String next() {
                        final String rsl = Integer.toString(cursor++, radix);
                        return pad.substring(0, digits - rsl.length()) + rsl;
                    }

                    public void remove() {
                        throw new UnsupportedOperationException();
                    }
                };
            }
        };
    }


    //returns true if values are entered successfully
    public boolean enterArrayIntoTiles(Integer[] values) {
        if (values.length != tiles.size()) return false;

        for (int i = 0; i < values.length; i++) {
            tiles.get(i).setNum(values[i]);
        }

        return true;
    }
}
