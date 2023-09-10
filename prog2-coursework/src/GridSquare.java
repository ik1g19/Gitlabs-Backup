import javafx.animation.AnimationTimer;
import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.geometry.HPos;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.util.Duration;

import java.util.ArrayList;
import java.util.Iterator;

public class GridSquare extends Canvas {

    private Integer num = 0;
    private Text numText;
    private Text target;

    //pointers to the tiles cage, column and row
    private Cage cage;
    private Column column;
    private Row row;

    private int location;
    private int x2d;
    private int y2d;

    private int dimensions;

    private int width;
    private int height;

    //flags to store whether the sides should be thick or thin
    private Boolean aboveThick = true;
    private Boolean rightThick = true;
    private Boolean belowThick = true;
    private Boolean leftThick = true;

    private Boolean selected = false;

    private GraphicsContext gc;

    private Grid grid;

    //booleans to keep track of whether mistakes should be shown for the cage, row or column
    private boolean mistakeCage;
    private boolean mistakeRow;
    private boolean mistakeColumn;

    //flag used to check if a tile has been visited when algorithms traverse the grid
    private boolean visited = false;

    //the percentage chance of a tile merging into the adjacent cage if it is in a cage of size 1
    private final int probabilityOfMergingWithCage = 90;

    private Timeline timeline;
    private AnimationTimer timer;

    /*
    //the animation thread for each tile
    private Thread animationThread;
    private boolean continueAnimation = true;*/

    //constructor used when creating tiles for a text loaded grid
    public GridSquare(Integer location, Cage cage, Grid grid) {
        this.grid = grid;
        this.cage = cage;
        this.location = location;

        mistakeCage = false;
        mistakeRow = false;
        mistakeColumn = false;

        gc = getGraphicsContext2D();
    }

    //constructor used when creating tiles for a random grid
    public GridSquare(Integer location, Grid grid) {
        this.grid = grid;
        this.location = location;
        calculateLocation();

        mistakeCage = false;
        mistakeRow = false;
        mistakeColumn = false;

        gc = getGraphicsContext2D();
    }


    public void setWidth(int width) {
        this.width = width;
        super.setWidth(width);
    }

    public void setHeight(int height) {
        this.height = height;
        super.setHeight(height);
    }

    //function to draw the rectangle
    public void drawRect() {
        gc.setStroke(Color.BLACK);

        //if the top of the rectangle should be thick
        if (aboveThick) {
            gc.setLineWidth(5);
        }
        //if the top of the rectangle should be thin
        else{
            gc.setLineWidth(1);
        }
        gc.strokeLine(0, 0, width, 0);

        //if the right of the rectangle should be thick
        if (rightThick) {
            gc.setLineWidth(5);
        }
        //if the right of the rectangle should be thin
        else{
            gc.setLineWidth(1);
        }
        gc.strokeLine(width, 0, width, height);

        //if the bottom of the rectangle should be thick
        if (belowThick) {
            gc.setLineWidth(5);
        }
        //if the bottom of the rectangle should be thin
        else{
            gc.setLineWidth(1);
        }
        gc.strokeLine(width, height, 0, height);

        //if the left of the rectangle should be thick
        if (leftThick) {
            gc.setLineWidth(5);
        }
        //if the left of the rectangle should be thin
        else{
            gc.setLineWidth(1);
        }
        gc.strokeLine(0, height, 0, 0);
    }

    //draws a rect without any cage lines
    public void drawBasicRect() {
        gc.setStroke(Color.BLACK);
        gc.setLineWidth(1);

        gc.strokeLine(0, 0, width, 0);
        gc.strokeLine(width, 0, width, height);
        gc.strokeLine(width, height, 0, height);
        gc.strokeLine(0, height, 0, 0);
    }

    //draws a normal tile while the program is running
    public void drawNormal() {
        gc.clearRect(0,0, width, height);
        gc.setFill(Color.BLACK);
        drawRect();
    }

    //mark tile as having no mistakes
    public void flagNoMistakes() {
        mistakeCage = false;
        mistakeRow = false;
        mistakeColumn = false;
    }


    //select this tile
    public void select() {
        //draw as selected
        drawSelected();

        selected = true;
    }

    //draw this tile as selected
    public void drawSelected() {
        gc.clearRect(0,0,width,height);
        gc.setGlobalAlpha(0.5);
        gc.setFill(Color.web("#00ffff"));
        gc.fillRect(0, 0, width, height);
        gc.setGlobalAlpha(1);
        drawRect();
    }


    //unselect this tile
    public void unselect(boolean mistakesShown) {
        if (mistakesShown) drawMistakes();
        else drawNormal();

        selected = false;
    }

    //draws the tile as unselected, or highlights it if it causes any mistakes
    private void drawUnselected() {
        gc.clearRect(0,0,width,height);

        //if the tile is causing any mistakes, colour it appropriately
        if (isTileCausingAnyMistakes()) drawMistakes();
        //otherwise unselect normally
        else drawNormal();
    }

    //draws any mistakes if they exist
    //returns true if any mistakes were present
    public void drawMistakes() {
        //priority of mistake colouring is cage, row then column
        if (mistakeCage) {
            drawMistakeCage();
        }
        else if (mistakeRow) {
            drawMistakeRow();
        }
        else if (mistakeColumn) {
            drawMistakeColumn();
        }
        //otherwise there are no mistakes and draw normally
        else {
            drawNormal();
        }
    }

    //returns true if the tile is causing any mistakes
    //otherwise returns false
    public boolean isTileCausingAnyMistakes() {
        if (mistakeCage || mistakeColumn || mistakeRow) return true;
        else return false;
    }

    /*public void addListeners() {
        hbox.widthProperty().addListener(new ChangeListener<Number>() {
            @Override
            public void changed(ObservableValue<? extends Number> observable,
                                Number oldValue, Number newValue) {

                double width = hbox.getWidth();

                setWidth(width);
                num.prefWidth(20);
            }
        });

        vbox.widthProperty().addListener(new ChangeListener<Number>() {
            @Override
            public void changed(ObservableValue<? extends Number> observable,
                                Number oldValue, Number newValue) {

                double height = vbox.getHeight();

                setHeight(height);
                num.prefWidth(20);
            }
        });
    }*/

    public void setNum(Integer n) {
        //removing the old text
        grid.getChildren().remove(getNumberText());

        num = n;

        //if the value is not 0 update the text
        if (n != 0) {
            numText = new Text(Integer.toString(n));
            numText.setFont(Font.font("verdana", FontWeight.BOLD, FontPosture.REGULAR, 20));

            grid.add(getNumberText(), getX2d(), getY2d());
            GridPane.setHalignment(getNumberText(), HPos.CENTER);
        }
    }

    public void incrementNum() {
        //if reached dimension size loop round
        if (num == dimensions) setNum(0);
        //otherwise increment
        else setNum(num + 1);
    }

    public void clearNum() {
        num = 0;

        grid.getChildren().remove(numText);
    }

    public Integer getNumber() {
        return num;
    }

    public Text getNumberText() {
        return numText;
    }

    public void setTarget(String target) {
        this.target = new Text(target);
    }

    public Text getTarget() {
        return target;
    }


    public void setCage(Cage cage) {
        this.cage = cage;
    }

    public Cage getCage() {
        return cage;
    }


    //deduces which of the sides of the tile should be thick
    public void deduceThickness() {
        //if the right tile is apart of the same cage then the right side should not be thick
        if (doesRightTileExist()) {
            if (getRightTile().getCage() == cage) rightThick = false;
        }

        //if the below tile is apart of the same cage then the below side should not be thick
        if (doesBelowTileExist()) {
            if (getBelowTile().getCage() == cage) belowThick = false;
        }

        //if the left tile is apart of the same cage then the left side should not be thick
        if (doesLeftTileExist()) {
            if (getLeftTile().getCage() == cage) leftThick = false;
        }

        //if the above tile is apart of the same cage then the above side should not be thick
        if (doesAboveTileExist()) {
            if (getAboveTile().getCage() == cage) aboveThick = false;
        }
    }


    public void calculateLocation() {
        dimensions = grid.getDimensions();

        x2d = convert1Dto2DCoordsX(location);
        y2d = convert1Dto2DCoordsY(location);
    }

    public int getLocation() {
        return location;
    }


    //array is 0 based
    //takes index in 1d array and responds its x position in 2d array
    private int convert1Dto2DCoordsX(int x) {
        int x2d;
        if (x <= dimensions) x2d = x - 1;
        else if (x % dimensions == 0) x2d = dimensions - 1;
        else x2d = (x % dimensions) - 1;
        return x2d;
    }

    //takes index in 1d array and responds its y position in 2d array
    private int convert1Dto2DCoordsY(int x) {
        int y2d;
        if (x <= dimensions) y2d = 0;
        else if (x % dimensions == 0) y2d = (x / dimensions) - 1;
        else y2d = x / dimensions;
        return y2d;
    }


    public int getX2d() {
        return x2d;
    }

    public int getY2d() {
        return y2d;
    }


    public Boolean isSelected() {
        return selected;
    }


    //marks the tile as part of a cage that is wrong
    public void drawMistakeCage() {
        gc.clearRect(0,0,width,height);
        gc.setGlobalAlpha(0.5);
        gc.setFill(Color.web("#d90d0d"));
        gc.fillRect(0, 0, width, height);
        gc.setGlobalAlpha(1);
        drawRect();
    }

    //flag as causing a cage mistake
    public void flagMistakeCage(boolean flag) {
        mistakeCage = flag;
    }

    //marks the tile as part of a column that is wrong
    public void drawMistakeColumn() {
        mistakeColumn = true;

        gc.clearRect(0,0,width,height);
        gc.setGlobalAlpha(0.5);
        gc.setFill(Color.web("#f7f414"));
        gc.fillRect(0, 0, width, height);
        gc.setGlobalAlpha(1);
        drawRect();
    }

    //flag as causing a column mistake
    public void flagMistakeColumn(boolean flag) {
        mistakeColumn = flag;
    }

    //marks the tile as part of a row that is wrong
    public void drawMistakeRow() {
        gc.clearRect(0,0,width,height);
        gc.setGlobalAlpha(0.5);
        gc.setFill(Color.web("#14aff7"));
        gc.fillRect(0, 0, width, height);
        gc.setGlobalAlpha(1);
        drawRect();
    }

    //flag as causing a row mistake
    public void flagMistakeRow(boolean flag) {
        mistakeRow = flag;
    }


    //returns false if there are no mistakes and true if there are mistakes
    public boolean checkCageForMistakes() {
        //if all tiles in the cage have a value entered, test if cage has been completed successfully
        if (cage.isFull()) {
            //if cage has been completed correctly flag it and remove any mistake flags from the tiles in the cage
            if (cage.doesCageMeetTarget()) {
                cage.setCorrect(2);
                return false;
            }
            //otherwise flag the cage as wrong
            else {
                cage.setCorrect(0);
                return true;
            }
        }

        cage.setCorrect(1);
        return false;
    }

    //returns true if column contains mistakes, otherwise false
    public boolean checkColumnForMistakes() {
        //only check column for mistakes if column is full
        if (column.isFull()) {
            //if column contains repeat mark it as incorrect and show mistakes, return true
            if (column.isRepeats()) {
                column.setCorrect(0);
                return true;
            }
            //otherwise column contains repeat, mark it as incorrect and return false
            else {
                column.setCorrect(2);
                return false;
            }
        }
        //otherwise column is incomplete, mark it as incomplete and return false
        else {
            column.setCorrect(1);
            return false;
        }
    }

    //returns true if row contains mistakes, otherwise false
    public boolean checkRowForMistakes() {
        //only check row for mistakes if row is full
        if (row.isFull()) {
            //if row contains repeat mark it as incorrect and show mistakes, return true
            if (row.isRepeats()) {
                row.setCorrect(0);
                return true;
            }
            //otherwise mark row as correct and hide any previous mistakes, return false
            else {
                row.setCorrect(2);
                return false;
            }
        }
        //otherwise return false
        else {
            row.setCorrect(1);
            return false;
        }
    }


    public Column getColumn() {
        return column;
    }

    public Row getRow() {
        return row;
    }

    public void setColumn(Column col) {
        column = col;
    }

    public void setRow(Row row) {
        this.row = row;
    }


    //a set of functions that return whether adjacent tiles exist
    public boolean doesAboveTileExist() {
        if (y2d == 0) return false;
        else return true;
    }

    public boolean doesRightTileExist() {
        if (x2d == dimensions-1) return false;
        else return true;
    }

    public boolean doesBelowTileExist() {
        if (y2d == dimensions-1) return false;
        else return true;
    }

    public boolean doesLeftTileExist() {
        if (x2d == 0) return false;
        else return true;
    }


    //a set of functions to fetch adjacent tiles
    public GridSquare getAboveTile() {
        return grid.getTileByLocation(location - dimensions);
    }

    public GridSquare getRightTile() {
        return grid.getTileByLocation(location + 1);
    }

    public GridSquare getBelowTile() {
        return grid.getTileByLocation(location + dimensions);
    }

    public GridSquare getLeftTile() {
        return grid.getTileByLocation(location - 1);
    }

    //returns an array list of adjacent tiles
    public ArrayList<GridSquare> getAdjacentTiles() {
        ArrayList<GridSquare> adjacent = new ArrayList<>();

        if (doesRightTileExist()) adjacent.add(getRightTile());
        if (doesBelowTileExist()) adjacent.add(getBelowTile());
        if (doesLeftTileExist()) adjacent.add(getLeftTile());
        if (doesAboveTileExist()) adjacent.add(getAboveTile());

        return adjacent;
    }


    public boolean hasBeenVisited() {
        return visited;
    }

    public void setVisited(boolean flag) {
        visited = flag;
    }
    
    
    public int getProbabilityOfMerging() {
        return probabilityOfMergingWithCage;
    }


    public void drawRGB(int r, int g, int b) {
        gc.clearRect(0,0,width,height);
        gc.setGlobalAlpha(0.5);
        gc.setFill(Color.rgb(r, g, b));
        gc.fillRect(0, 0, width, height);
        gc.setGlobalAlpha(1);
        drawRect();
    }


    public GraphicsContext getGraphicsContext() {
        return gc;
    }


    /*
    public void startAnimationThread() {
        continueAnimation = true;
        Runnable r = new WinningAnimationThread(this, 255, 255, 0);
        animationThread = new Thread(r);
        animationThread.start();
    }

    public void stopAnimationThread() {
        continueAnimation = false;
    }


    public boolean shouldContinueAnimation() {
        return continueAnimation;
    }*/

    public void startWinAnimation() {
        DoubleProperty r  = new SimpleDoubleProperty();
        DoubleProperty g  = new SimpleDoubleProperty();
        DoubleProperty b  = new SimpleDoubleProperty();

        timeline = new Timeline(
                new KeyFrame(Duration.seconds(0),
                        new KeyValue(r, 255),
                        new KeyValue(g, 255),
                        new KeyValue(b, 0)
                ),
                new KeyFrame(Duration.seconds(1),
                        new KeyValue(r, 255),
                        new KeyValue(g, 255),
                        new KeyValue(b, 255)
                )
        );
        timeline.setAutoReverse(true);
        timeline.setCycleCount(Timeline.INDEFINITE);

        timer = new AnimationTimer() {
            public void handle(long now) {
                int rInt = (int)r.doubleValue();
                int gInt = (int)g.doubleValue();
                int bInt = (int)b.doubleValue();
                drawRGB(rInt, gInt, bInt);
            }
        };

        timer.start();
        timeline.play();
    }

    public void stopWinAnimation() {
        timer.stop();
        timeline.stop();
    }
}
