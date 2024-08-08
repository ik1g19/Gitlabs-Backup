package src;

import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.Chart;
import javafx.scene.chart.LineChart;
import javafx.scene.chart.PieChart;
import javafx.scene.control.*;
import javafx.stage.Stage;

import java.util.*;

public class MenuBarHandler {

    private Stage stage;
    private int fontSize;
    private List<Node> fileSelectNodes;
    private List<Node> dataViewNodes;
    private List<Chart> dataViewCharts;
    private final String DATA_VIEW_SCREEN = "Data View Screen";
    private final String FILE_SELECT_SCREEN = "File Select Screen";

    private final int SMALL_FONT = 10;
    private final int MEDIUM_FONT = 15;
    private final int LARGE_FONT = 20;

    private ArrayList<Stage> windows =new ArrayList<Stage>();

    public MenuBarHandler(Stage stage,ArrayList<Stage> windows) {
        this.stage = stage;
        this.windows=windows;
        this.windows.add(stage);
    }

    public MenuBar dataViewScreen(DatabaseController dbc, Scene fileSelectScene, List<Label> allLabels, List<RadioButton> allButtons,QueryExecutor qry_exec) {
        MenuBar menuBar = new MenuBar();

        Menu file = new Menu("File");
        MenuItem back = new MenuItem("Back to File Selection");
        MenuItem newWindow =  new MenuItem("Open new window");


        newWindow.setOnAction(e ->  windowOpener(dbc,qry_exec,fileSelectScene));
        back.setOnAction(e ->  windowCloser(dbc,fileSelectScene));
        SeparatorMenuItem separator = new SeparatorMenuItem();
        SeparatorMenuItem separator1 = new SeparatorMenuItem();
        MenuItem exit = new MenuItem("Exit");

        exit.setOnAction(e -> { System.exit(0); });

        dataViewNodes = new ArrayList<Node>();
        dataViewNodes.addAll(allLabels);
        dataViewNodes.addAll(allButtons);
        Menu preferences = setupPreferences(dataViewNodes);

        file.getItems().addAll(back, separator,newWindow,separator1, exit);

        menuBar.getMenus().addAll(file, preferences);

        return menuBar;
    }

    public void addChart(PieChart chart) {
        dataViewNodes.add((Node) chart);
        updateFontSize(DATA_VIEW_SCREEN);
    }

    public void addChart(BarChart chart) {
        dataViewNodes.add((Node) chart);
        updateFontSize(DATA_VIEW_SCREEN);
    }

    public void addChart(LineChart chart) {
        dataViewNodes.add((Node) chart);
        updateFontSize(DATA_VIEW_SCREEN);
    }

    public MenuBar fileSelectScreen(List<Label> allLabels, List<Button> allButtons) {
        MenuBar menuBar = new MenuBar();

        Menu file = new Menu("File");
        MenuItem exit = new MenuItem("Exit");
        exit.setOnAction(e -> { System.exit(0); });

        file.getItems().addAll(exit);

        fileSelectNodes = new ArrayList<Node>();
        fileSelectNodes.addAll(allLabels);
        fileSelectNodes.addAll(allButtons);

        Menu preferences = setupPreferences(fileSelectNodes);

        menuBar.getMenus().addAll(file, preferences);

        return menuBar;
    }


    public Menu setupPreferences(List<Node> nodes) {
        Menu preferences = new Menu("Preferences");

        Menu fontSize = new Menu("Font Size");
        MenuItem small = new MenuItem("Small");
        MenuItem medium = new MenuItem("Medium");
        MenuItem large = new MenuItem("Large");

        small.setOnAction(e -> { setFontSize(SMALL_FONT, nodes); });
        medium.setOnAction(e -> { setFontSize(MEDIUM_FONT, nodes); });
        large.setOnAction(e -> { setFontSize(LARGE_FONT, nodes); });

        fontSize.getItems().addAll(small, medium, large);
        preferences.getItems().addAll(fontSize);
        return preferences;
    }

    private void setFontSize(int size, List<Node> nodes) {
        for (Node node : nodes){
            node.setStyle("-fx-font: " + size + "px \"Arial\";");
        }
        fontSize = size;
    }

    public void updateFontSize(String currentScene) {
        if (fontSize == 0) {
            fontSize = MEDIUM_FONT;
        }
        if (currentScene.equals(DATA_VIEW_SCREEN)) {
            setFontSize(fontSize, dataViewNodes);
        }
        else if (currentScene.equals(FILE_SELECT_SCREEN)) {
            setFontSize(fontSize, fileSelectNodes);
        }
        else {
            System.out.println("ERROR: MenuBarHandler.java - updateFoneSize() - Unknown 'currentScene'");
        }
    }

    private void backToFileSelect(DatabaseController dbc, Scene fileSelectScene) {
        dbc.tearDown();

        stage.setScene(fileSelectScene);
        stage.setWidth(550);
        stage.setHeight(330);
        stage.setMinWidth(550);
        stage.setMinHeight(300);
        stage.show();

        updateFontSize(FILE_SELECT_SCREEN);
    }
    public void transferNodes(List<Node> fileSelectNodes){
        this.fileSelectNodes = fileSelectNodes;
    }
    private void windowOpener(DatabaseController dbc,QueryExecutor qry_exec,Scene fileSelectScene){
        Stage newStage=new Stage();
        MenuBarHandler newBar =  new MenuBarHandler(newStage,windows);
        newBar.transferNodes(fileSelectNodes);
        new DataViewScreen(newStage, dbc, qry_exec, newBar,fileSelectScene);
    }
    private void windowCloser(DatabaseController dbc,Scene fileSelectScene){
        for(Stage i:windows){
            if(i!=stage){
                i.close();
            }
        }
        Database db = new Database("./logs.db");
        DatabaseController db_ctrl = new DatabaseController(db);
        FileSelectScreen screen = new FileSelectScreen(stage, db_ctrl);

    }
}