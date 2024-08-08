import javafx.animation.PauseTransition;
import javafx.util.Duration;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class Solver {
    private Grid grid;

    private ArrayList<Cage> cages;

    private int[] original;
    private int[] solution;

    public Solver(Grid g) {
        grid = g;
    }

    /*
    public void solveGridCageByCage(Grid g) {
        grid = g;

        cages = grid.getCages();

        Cage cage = cages.get(0);

        ArrayList<Integer[]> possibleValues = cage.getPossibleSolutionsToCage();

        Iterator<Integer[]> cageSolutionIterator = possibleValues.iterator();
        while (cageSolutionIterator.hasNext()) {
            Integer[] values = cageSolutionIterator.next();
            if (attemptCage(0, values)) {
                break;
            }
        }
    }*/

    public void solveGridTileByTile() {
        original = new int[grid.getAllTiles().length];
        copyGridValues(original);

        for (int i = 1; i <= grid.dimensions; i++) {
            if (attemptTile(0, i)) break;
        }

        //copying solution to solution array
        solution = new int[grid.getAllTiles().length];
        copyGridValues(solution);

        populateGridWithArray(original);
    }

    public void setSolution() {
        populateGridWithArray(solution);
    }

    //copying grid values to solution array
    private void copyGridValues(int[] array) {
        for (int i = 0; i < grid.getAllTiles().length; i++) {
            array[i] = grid.getAllTiles()[i].getNumber();
        }
    }

    private void populateGridWithArray(int[] array) {
        for (int i = 0; i < grid.getAllTiles().length; i++) {
            grid.getAllTiles()[i].setNum(array[i]);
        }
    }

    private boolean hasSolution() {
        if (solution == null) return false;
        else return true;
    }

    /*
    public void solveGridTileByTileIteratively(Grid g) {
        grid = g;

        int tileIndex = 0;
        while (true) {
            GridSquare tile = grid.getAllTiles()[tileIndex];

            while (tile.getNumber() <= grid.dimensions) {
                tile.setNum(tile.getNumber() + 1);

                if (tile.getNumber() > grid.dimensions) {
                    tile.clearNum();
                    tileIndex--;
                    break;
                }

                boolean valid = true;
                if (tile.getColumn().isRepeats()) valid = false;
                if (tile.getRow().isRepeats()) valid = false;
                if (tile.getCage().isFull() && !tile.getCage().doesCageMeetTarget()) valid = false;

                attempts++;
                //System.out.println("attempts: " + attempts);
                System.out.println(tileIndex);

                if (valid) {
                    //grid.prettyPrintGrid();
                    tileIndex++;
                    break;
                }
                else if (!valid && tile.getNumber() == grid.dimensions) {
                    tile.clearNum();
                    tileIndex--;
                    break;
                }
            }

            if (tileIndex == grid.dimensions * grid.dimensions) break;
        }
    }

     */

    private int attempts = 0;
    private boolean attemptTile(int tileIndex, int value) {
        GridSquare tile = grid.getAllTiles()[tileIndex];

        tile.setNum(value);
        attempts++;

        if (tile.getColumn().isRepeats()) return false;
        if (tile.getRow().isRepeats()) return false;
        if (tile.getCage().isFull() && !tile.getCage().doesCageMeetTarget()) return false;

        if (doAllTilesContainValues()) return true;

        tileIndex++;
        //System.out.println(tileIndex);

        for (int i = 1; i <= grid.dimensions; i++) {
            if (attemptTile(tileIndex, i)) return true;
            else grid.getAllTiles()[tileIndex].clearNum();
        }

        return false;
    }

    /*
    private boolean attemptCage(int cageIndex, Integer[] values) {
        Cage cage = cages.get(cageIndex);

        //System.out.println(Arrays.toString(values));
        cage.enterArrayIntoTiles(values);

        if (doAllCagesContainValues()) return true;

        cageIndex++;

        Cage nextCage = cages.get(cageIndex);
        ArrayList<Integer[]> solutions = nextCage.getPossibleSolutionsToCage();

        Iterator<Integer[]> solutionIterator = solutions.iterator();
        System.out.println(solutions.size());
        while (solutionIterator.hasNext()) {
            Integer[] currentAttempt = solutionIterator.next();

            if (attemptCage(cageIndex, currentAttempt)) return true;
            else nextCage.setTileValuesTo0();
        }

        //return false if there are no successful attempts for the cage
        return false;
    }
     */


    /*
    private boolean doAllCagesContainValues() {
        //if all cages are full then the grid has been completed
        Iterator<Cage> cageIterator = cages.iterator();
        boolean allCagesComplete = true;
        while (cageIterator.hasNext()) {
            Cage currentCage = cageIterator.next();

            if (!currentCage.isFull()) allCagesComplete = false;
        }
        return allCagesComplete;
    }
     */

    private boolean doAllTilesContainValues() {
        for (int i = 0; i < grid.getAllTiles().length; i++) {
            GridSquare tile = grid.getAllTiles()[i];

            if (tile.getNumber() == 0) return false;
        }

        return true;
    }


    public void showHint() {
        ArrayList<GridSquare> tilesToShowHint = new ArrayList<>();

        //only show hints for incorrect squares
        for (int i = 0; i < grid.getAllTiles().length; i++) {
            if (grid.getAllTiles()[i].getNumber() != solution[i]) tilesToShowHint.add(grid.getAllTiles()[i]);
        }

        if (tilesToShowHint.size() !=0 ) {
            Random rand = new Random();
            GridSquare tile = tilesToShowHint.get(rand.nextInt(tilesToShowHint.size()));

            int original = tile.getNumber();
            tile.setNum(solution[tile.getLocation() - 1]);

            PauseTransition pause = new PauseTransition(Duration.seconds(1));
            pause.setOnFinished(event -> {
                    tile.setNum(original);
            });
            pause.play();
        }
    }
}
