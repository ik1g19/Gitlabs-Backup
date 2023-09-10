package src;

import  javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.chart.*;
import javafx.scene.chart.XYChart.Series;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/*
 @desc - class for managing the window which will display 'graphs and such'
 */
public class DataViewScreen {
    //    important ones

    private DatabaseController dbc;
    private QueryExecutor qry_exec;
    private Stage stage;
    private Scene fileSelectScene;

    private MenuBarHandler menuBarHandler;

    //      buttons

    private ToggleGroup metricGroup = new ToggleGroup();

    private RadioButton imprnsBtn = new RadioButton("Impressions");
    private RadioButton clicks = new RadioButton("Clicks");
    private RadioButton uniqueUsrs = new RadioButton("Unique Users");
    private RadioButton bounces = new RadioButton("Bounces");
    private RadioButton conversions = new RadioButton("Conversions");
    private RadioButton cost = new RadioButton("Total Cost");
    private RadioButton ctr = new RadioButton("Click-Through-Rate");
    private RadioButton cpa = new RadioButton("Cost-Per-Acquisition");
    private RadioButton cpc = new RadioButton("Cost-Per-Click");
    private RadioButton cpm = new RadioButton("Cost-Per-Thousand Impressions");

    private Button back = new Button("Back to File Selection");

    private ComboBox<String> filterSelect;
    private ComboBox<String> granularitySelect;
    private ComboBox<String> fontSize;



    //        labels

    private Label metricDisplay = new Label("Click above to select a metric to display");
    private Label metricDisplayFemale = new Label("");
    private Label metricDisplayMale = new Label("");
    private Label appliedFilters = new Label("No filters applied.");
    private Label bounceRateLabel = new Label("Calculating bounce rate...");


    //      organisation

    private VBox metrics = new VBox();
    private VBox filterBox = new VBox();
    private BorderPane uiDivider = new BorderPane();
    private HBox filtersNGraphs = new HBox();
    private HBox metricBox = new HBox();
    private AnchorPane bottomOfUI = new AnchorPane();
    private HBox dropDowns = new HBox();
    private VBox multiGraphs = new VBox();


    //     #justChartThings

    private ArrayList<Series> series_line = new ArrayList<Series>();
    private LineChart linechart;
    private PieChart piechart;
    final Axis xAxis = new NumberAxis();
    final Axis yAxis = new NumberAxis();


    //lists of things to change fontSize on
    private List<Label> allLabels= Arrays.asList(metricDisplay,metricDisplayFemale,metricDisplayMale,appliedFilters,bounceRateLabel);
    private List<RadioButton> allButtons=Arrays.asList(imprnsBtn,clicks,uniqueUsrs,bounces,conversions,cost,ctr,cpa,cpc,cpm);
    private int sizeOfFont=15;

    private String currentFilter;

    // Multi-threading
    private RadioButton inUse;

    /*
     @desc - constructs a window to view the input files' data
     @param - stage: stage to use as the window
     */
    public DataViewScreen(Stage stage, DatabaseController dbc, QueryExecutor qry_exec, MenuBarHandler menuBarHandler, Scene fileSelectScene) {
        this.dbc = dbc;
        this.stage = stage;
        this.qry_exec = qry_exec;
        this.fileSelectScene = fileSelectScene;

        this.menuBarHandler = menuBarHandler;

        buttonSetup();

        arrangement();

        addListeners();

        imprnsBtn.fire();

        initFonts();

        menuBarHandler.updateFontSize("Data View Screen");

        Scene scene = new Scene(uiDivider);

        stage.setTitle("Data viewer");
        stage.setScene(scene);
        stage.setHeight(700);
        stage.setWidth(1200);
        stage.show();
    }

    /*
     @desc - configurations and setup for buttons
     */
    private void buttonSetup() {
        imprnsBtn.setToggleGroup(metricGroup);
        clicks.setToggleGroup(metricGroup);
        uniqueUsrs.setToggleGroup(metricGroup);
        bounces.setToggleGroup(metricGroup);
        conversions.setToggleGroup(metricGroup);
        cost.setToggleGroup(metricGroup);
        ctr.setToggleGroup(metricGroup);
        cpa.setToggleGroup(metricGroup);
        cpc.setToggleGroup(metricGroup);
        cpm.setToggleGroup(metricGroup);

        ObservableList<String> filters = FXCollections.observableArrayList(
                "Date Range",
                "Gender",
                "Age",
                "Income",
                "Context",
                "No Filter"
        );
        filterSelect = new ComboBox<>(filters);

        ObservableList<String> size = FXCollections.observableArrayList(
                "Small",
                "Medium",
                "Large"
        );
        fontSize = new ComboBox<>(size);
        fontSize.getSelectionModel().select(1);


        ObservableList<String> intervals = FXCollections.observableArrayList(
                "Hour",
                "Day",
                "Week",
                "Month"
        );
        granularitySelect = new ComboBox<>(intervals);
        granularitySelect.getSelectionModel().select(1);
    }



    /*
     @desc - arrangement of elements on screen
     */
    private void arrangement() {
        metrics.getChildren().addAll(imprnsBtn,  clicks,uniqueUsrs,bounces,
                conversions,cost,  ctr,       cpa,
                cpc,        cpm);
        metrics.setSpacing(20);

        metricDisplay.setFont(Font.font("Arial", 16));
        metricDisplay.setWrapText(true);

        metricBox.getChildren().addAll(metricDisplay, metricDisplayFemale, metricDisplayMale);
        metricBox.setPadding(new Insets(10));
        metricDisplay.setPadding(new Insets(10));
        metricDisplayFemale.setPadding(new Insets(10));
        metricDisplayMale.setPadding(new Insets(10));
        metrics.setPadding(new Insets(10,10,10,10));

        appliedFilters.setPadding(new Insets(10));

        filterBox.getChildren().addAll(new Text("Filter:"), filterSelect, appliedFilters, metricBox);
        filterBox.setPadding(new Insets(10,0,0,0));
        filterBox.setAlignment(Pos.TOP_CENTER);

        bottomOfUI.getChildren().addAll(dropDowns, back);

        dropDowns.getChildren().addAll(fontSize, granularitySelect);

        // bottomOfUI.setLeftAnchor(metricBox,0.0);

        bottomOfUI.setRightAnchor(back,0.0);
        bottomOfUI.setPadding(new Insets(10));

        uiDivider.setTop(menuBarHandler.dataViewScreen(dbc, fileSelectScene, allLabels, allButtons,qry_exec));
        uiDivider.setLeft(metrics);
        uiDivider.setCenter(filtersNGraphs);
        uiDivider.setBottom(bottomOfUI);
    }



    /*
     @desc - attaches listeners to elements of the screen
     */
    private void addListeners() {
        metricGroup.selectedToggleProperty().addListener((observableValue, oldValue, newValue) -> metricUpdate((RadioButton) newValue));

        filterSelect.valueProperty().addListener( (observableValue, oldValue, newValue) -> filterUpdate(newValue));


        back.setOnAction(e -> {
            dbc.tearDown();

            stage.setScene(fileSelectScene);
            stage.setWidth(500);
            stage.setHeight(500);
            stage.setMinWidth(500);
            stage.setMinHeight(500);
            stage.show();
        });
        fontSize.setOnAction(new EventHandler<ActionEvent>() {

            @Override
            public void handle(ActionEvent event) {
                int selected = fontSize.getSelectionModel().getSelectedIndex();
                if(selected == 0 ){
                    sizeOfFont=10;
                    initFonts();
                }
                else if(selected == 1 ){
                    sizeOfFont=15;
                    initFonts();
                }
                else if(selected == 2 ){
                    sizeOfFont=20;
                    initFonts();
                }
                stage.sizeToScene();
            }
        });


        granularitySelect.valueProperty().addListener((observableValue, oldValue, newValue) -> {
            // if there is a filter
            if(!currentFilter.equals("No Filter")) {
                filterUpdate(currentFilter);
            }else{
                metricUpdate((RadioButton) metricGroup.getSelectedToggle());
            }
        });
    }

    // Function defining update by changing metric
    private void metricUpdate(RadioButton selected){
        String granularity = granularitySelect.getSelectionModel().getSelectedItem();
        String metricText, metricTextMale, metricTextFemale;
        appliedFilters.setText("No filters applied.");
        currentFilter = "No Filter";

        filterBox.getChildren().clear();
        filterBox.getChildren().addAll(new Text("Filters"), filterSelect, appliedFilters, metricBox);

        Thread t = null;

        if (selected == imprnsBtn){

            metricText = "Number of impressions is "+ qry_exec.executeQuery("Impressions", granularity).get(0).get(0)[0];
            metricTextMale = "";
            metricTextFemale = "";
            Series metric3 = new Series();
            metric3.setName("Impressions per day");
            List<String[]> l1 = qry_exec.executeQuery("Impressions", granularity).get(1);
            int i = 0;
            while(i<l1.size()){
                metric3.getData().add(new XYChart.Data(l1.get(i)[0], Integer.parseInt(l1.get(i)[1])));
                i++;

            }
            series_line = new ArrayList<Series>(){
                {
                    add(metric3);
                }
            };

            Axis x = new CategoryAxis();
            x.setLabel("Day");
            yAxis.setLabel("Number of impressions");


            setLineChart(buildLineChart(series_line, x, yAxis, "Impressions per day"));

            filterSelect.getSelectionModel().clearSelection();
        }

        else if(selected == clicks){
            metricText = "Number of clicks is " + qry_exec.executeQuery("Clicks", granularity).get(0).get(0)[0];
            int maleCount = Integer.parseInt(qry_exec.executeQuery("Clicks", "gender","Male", granularity).get(0).get(0)[0]);
            int femaleCount = Integer.parseInt(qry_exec.executeQuery("Clicks", "gender","Female", granularity).get(0).get(0)[0]);
            int totalClicks = maleCount + femaleCount;
            metricTextMale = "Male: " + maleCount * 100 / totalClicks + "%";
            metricTextFemale = "Female: " + femaleCount * 100 / totalClicks + "%";

            System.out.println("Male: " + maleCount);
            System.out.println("Female: " + femaleCount);
            System.out.println("Total: " + totalClicks);

            Series metric3 = new Series();
            metric3.setName("Clicks per day");

            multiGraphs.getChildren().clear();
            multiGraphs.setSpacing(10);

            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox, multiGraphs);
            filtersNGraphs.setHgrow(multiGraphs, Priority.ALWAYS); // Resize chart

            List<String[]> l1 = qry_exec.executeQuery("Clicks", granularity).get(1);
            int i =0;
            while(i<l1.size()){
                metric3.getData().add(new XYChart.Data(l1.get(i)[0], Integer.parseInt(l1.get(i)[1])));
                i++;
                //indexOfDate++;

            }
            series_line = new ArrayList<Series>(){
                {
                    add(metric3);
                }
            };

            Axis x = new CategoryAxis();
            x.setLabel("Day");
            NumberAxis y = new NumberAxis();
            y.setLabel("Number of clicks");

            LineChart lineChart = buildLineChart(series_line, x, y, "Clicks per day");
            multiGraphs.setVgrow(lineChart, Priority.ALWAYS);
            multiGraphs.getChildren().add(lineChart);


            inUse = clicks;
            t = new Thread(
                    () -> {
                        List < String[]>results = qry_exec.executeQuery("Clicks", "age groups", 2, granularity);
                        // Pie Chart
                        ArrayList<PieChart.Data> data = new ArrayList<PieChart.Data>() {
                            {
                                results.forEach(x ->
                                        add(new PieChart.Data(x[0], Integer.valueOf(x[1])))
                                );
                            }
                        };
                        piechart = buildPieChart(data, "Clicks by age ranges");
                        if (inUse == clicks)
                            Platform.runLater(
                                    () ->{
                                        setMultiChart(piechart, lineChart);
                                    }
                            );

                    });
            t.start();
            filterSelect.getSelectionModel().clearSelection();
        }

        else if(selected == uniqueUsrs){
            metricText = "Number of unique users is "+qry_exec.executeQuery("Unique Users", granularity).get(0).get(0)[0];
            metricTextMale = "";
            metricTextFemale = "";

            Series metric3 = new Series();
            metric3.setName("Unique users per day");
            List<String[]> l1 = qry_exec.executeQuery("Unique Users", granularity).get(1);
            int i =0;
            while(i<l1.size()){
                metric3.getData().add(new XYChart.Data(l1.get(i)[0], Integer.parseInt(l1.get(i)[1])));
                i++;

            }
            series_line = new ArrayList<Series>(){
                {
                    add(metric3);
                }
            };

            Axis x = new CategoryAxis();
            x.setLabel("Day");
            yAxis.setLabel("Number of unique users");
            setLineChart(buildLineChart(series_line, x, yAxis, "Unique users per day"));

            filterSelect.getSelectionModel().clearSelection();
        }
        else if (selected == conversions){
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricText = "Not yet implemented.";
            metricTextMale = "";
            metricTextFemale = "";
        }
        else if(selected== cost){
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricText = "Calculating total cost...";
            metricTextMale = "";
            metricTextFemale = "";
            t = new Thread(
                    () -> {
                        List<List<String[]>> result = qry_exec.executeQuery("Total Cost", granularity);
                        Platform.runLater(
                                () -> {
                                    metricDisplay.setText("Total cost is " + result.get(0).get(0)[0]);
                                    metricDisplayMale.setText("");
                                    metricDisplayFemale.setText("");
                                    Series metric3 = new Series();
                                    metric3.setName("Total cost per day");
                                    List<String[]> l1 = result.get(1);
                                    int i = 0;
                                    double cumulativeCost=0;
                                    while (i < l1.size()) {
                                        cumulativeCost=cumulativeCost+Double.parseDouble(l1.get(i)[1]);
                                        metric3.getData().add(new XYChart.Data(l1.get(i)[0], cumulativeCost));
                                        //metric3.getData().add(new XYChart.Data(l1.get(i)[0], Double.parseDouble(l1.get(i)[1])));
                                        i++;

                                    }
                                    series_line = new ArrayList<Series>() {
                                        {
                                            add(metric3);
                                        }
                                    };

                                    Axis x = new CategoryAxis();
                                    x.setLabel("Day");
                                    yAxis.setLabel("cost in pence");
                                    setLineChart(buildLineChart(series_line, x, yAxis, "Total cost per day"));
                                }
                        );
                    }
            );
            t.start();
            filterSelect.getSelectionModel().clearSelection();
        }
        else if (selected == bounces){

            metricText = "Loading bounces...";
            metricTextFemale = "";
            metricTextMale = "";
            final HBox bounceRateBox = new HBox();

            bounceRateLabel = new Label("Calculating bounce rate...");
            bounceRateLabel.setFont(Font.font("Arial", sizeOfFont));
            bounceRateLabel.setWrapText(true);
            bounceRateLabel.setPadding(new Insets(10));

            bounceRateBox.getChildren().add(bounceRateLabel);
            bounceRateBox.setPadding(new Insets(10));

            filterBox.getChildren().clear();
            filterBox.getChildren().addAll(new Text("Filters"), filterSelect, appliedFilters, metricBox, bounceRateBox);

            multiGraphs.getChildren().clear();
            multiGraphs.setSpacing(10);

            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox, multiGraphs);
            filtersNGraphs.setHgrow(multiGraphs, Priority.ALWAYS); // Resize chart

            // Bounces thread
            Thread bounce_t = new Thread (
                    () ->{
                        Integer bounces  = Integer.valueOf(qry_exec.executeQuery("Bounces", granularity).get(0).get(0)[0]);
                        Integer totalClicks = Integer.valueOf(qry_exec.executeQuery("Clicks", granularity).get(0).get(0)[0]);
                        Platform.runLater(

                                () -> {
                                    metricDisplay.setText("Number of bounces: "+ bounces);

                                    // Pie Chart
                                    ArrayList<PieChart.Data> data = new ArrayList<PieChart.Data>(){
                                        {
                                            add(new PieChart.Data("Bounce clicks", bounces));
                                            add(new PieChart.Data("Normal clicks", totalClicks-bounces));
                                        }
                                    };
                                    piechart = buildPieChart(data, "Proportion of bounces");
                                    multiGraphs.setVgrow(piechart, Priority.ALWAYS);
                                    multiGraphs.getChildren().add(piechart);
                                }
                        );
                    }
            );

            // Bounce rate thread
            Thread rate_t = new Thread (
                    () ->{
                        Double bounceRate  = Double.valueOf(qry_exec.executeQuery("Bounces", "bounce rate",1, granularity).get(0)[0]);
                        List<String[]> bouncesPerDay = qry_exec.executeQuery("Bounces", granularity).get(1);
                        List<String[]> clicksPerDay = qry_exec.executeQuery("Clicks", granularity).get(1);
                        Platform.runLater(
                                () -> {
                                    bounceRateLabel.setText("Bounce rate: "+ bounceRate);
                                    Series metric = new Series();
                                    metric.setName("Bounce rate per day");

                                    int i =0;
                                    while(i<bouncesPerDay.size()){
                                        double avg = (Double.parseDouble(bouncesPerDay.get(i)[1])) / (Double.parseDouble(clicksPerDay.get(i)[1]));
                                        metric.getData().add(new XYChart.Data(bouncesPerDay.get(i)[0], avg));
                                        i++;

                                    }
                                    series_line = new ArrayList<Series>(){
                                        {
                                            add(metric);
                                        }
                                    };

                                    Axis x = new CategoryAxis();
                                    x.setLabel("Day");
                                    yAxis.setLabel("Bounce rate");

                                    multiGraphs.setVgrow(buildLineChart(series_line, x, yAxis, "Bounce rate per day"), Priority.ALWAYS);
                                    multiGraphs.getChildren().addAll(buildLineChart(series_line, x, yAxis, "Bounce rate per day"));
                                }
                        );
                    }
            );

            try {
                bounce_t.start();
                bounce_t.join();
                rate_t.start();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }

        else if (selected == cpc){
            inUse = cpc;
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricText = "Calculating cost-per-click...";
            metricTextMale = "";
            metricTextFemale = "";
            t = new Thread(
                    () -> {
                        Series metric3 = new Series();
                        metric3.setName("CPC over time");
                        List<List<String[]>> result = qry_exec.executeQuery("Cost-Per-Click", granularity);
                        if (inUse == cpc)
                            Platform.runLater(
                                    ()->{
                                        metricDisplay.setText("Cost per click is " + result.get(0).get(0)[0]);
                                        metricDisplayMale.setText("");
                                        metricDisplayFemale.setText("");

                                        for(String[] record : result.get(1)){
                                            metric3.getData().add(new XYChart.Data(record[0], Double.parseDouble(record[1])));
                                        }

                                        series_line = new ArrayList<Series>(){
                                            {
                                                add(metric3);
                                            }
                                        };

                                        Axis x = new CategoryAxis();
                                        x.setLabel("Day");
                                        yAxis.setLabel("CPC in pence");
                                        setLineChart(buildLineChart(series_line, x, yAxis, "Cost per click over the campaign"));
                                    }
                            );
                    }
            );
            t.start();
            filterSelect.getSelectionModel().clearSelection();
        }
        else if (selected == cpa) {
            //List<String[]> totalImpCostPerDay = dbc.dbQuery(SQLCommands.totalImpressionCostPerDates,2);
            //List<String[]> totalClickCostPerDay = dbc.dbQuery(SQLCommands.totalClickCostPerDates,2);
            //List<String[]> totalConversionsPerDay = dbc.dbQuery(SQLCommands.totalConversionsPerDates,2);
            inUse = cpa;
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricText = "Calculating cost-per-acquisition...";
            metricTextMale = "";
            metricTextFemale = "";
            t = new Thread(
                    () -> {
                        Series metric = new Series();
                        metric.setName("CPA");
                        List<List<String[]>> result = qry_exec.executeQuery("Cost-Per-Acquisition", granularity);
                        if (inUse == cpa)
                            Platform.runLater(
                                    ()-> {
                                        metricDisplay.setText("Cost per Acquisition: " + result.get(0).get(0)[0]);
                                        metricDisplayMale.setText("");
                                        metricDisplayFemale.setText("");

                                        double convEntireTotal = 0.0;
                                        double costEntireTotal = 0.0;

                                            /*
                                            for (String[] convEntry : totalConversionsPerDay) {    // manually joining the data
                                                String date = convEntry[0];                        // fuck you sql this is your fault
                                                double convTotal = Integer.parseInt(convEntry[1]);

                                                double clickTotal = 0.0;
                                                for (String[] clickEntry : totalClickCostPerDay) {
                                                    if (clickEntry[0].equals(date)) clickTotal += Double.parseDouble(clickEntry[1]);
                                                }

                                                double impTotal = 0.0;
                                                for (String[] impEntry : totalImpCostPerDay) {
                                                    if (impEntry[0].equals(date)) impTotal += Double.parseDouble(impEntry[1]);
                                                }

                                                double totalCost = impTotal + clickTotal;
                                                double averageCPA = totalCost / convTotal;

                                                costEntireTotal += totalCost;
                                                convEntireTotal += convTotal;


                                                metric.getData().add(new XYChart.Data(date, averageCPA));
                                            }

                                            metricText += Double.toString(costEntireTotal / convEntireTotal) + " pence";
                                            */

                                        for (String[] record : result.get(1)) {
                                            metric.getData().add(new XYChart.Data(record[0], Double.parseDouble(record[1])));
                                        }
                                        series_line = new ArrayList<Series>() {{
                                            add(metric);
                                        }};

                                        Axis x = new CategoryAxis();

                                        x.setLabel("Day");
                                        yAxis.setLabel("CPA");
                                        setLineChart(buildLineChart(series_line, x, yAxis, "Cost-per-Acquisition"));
                                    }
                            );
                    }
            );
            t.start();
            filterSelect.getSelectionModel().clearSelection();
        }

        else if (selected == ctr) {
            inUse = ctr;
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricText = "Calculating click-through-rate...";
            metricTextMale = "";
            metricTextFemale = "";
            t = new Thread(
                    () -> {
                        Series metric = new Series();
                        metric.setName("CTR over time");
                        List<List<String[]>> result = qry_exec.executeQuery("Click-Through-Rate", granularity);
                        if (inUse == ctr)
                            Platform.runLater(
                                    ()->{
                                        metricDisplay.setText("Click Through Rate is " + Double.parseDouble(result.get(0).get(0)[0]));
                                        metricDisplayMale.setText("");
                                        metricDisplayFemale.setText("");

                                        for(String[] record : result.get(1)){
                                            metric.getData().add(new XYChart.Data(record[0], Double.parseDouble(record[1])));
                                        }


                                        series_line = new ArrayList<Series>(){
                                            {
                                                add(metric);
                                            }
                                        };

                                        Axis x = new CategoryAxis();
                                        x.setLabel("Day");
                                        yAxis.setLabel("CTR");
                                        setLineChart(buildLineChart(series_line, x, yAxis, "Click Through Rate over the campaign"));
                                    }
                            );
                    }
            );
            t.start();
            filterSelect.getSelectionModel().clearSelection();
        }
        else if (selected == cpm){
            filtersNGraphs.getChildren().clear();
            filtersNGraphs.getChildren().addAll(filterBox);
            metricTextMale = "";
            metricTextFemale = "";
            var result = qry_exec.executeQuery("Cost-Per-Thousand Impressions", granularity);

            metricText = "Cost-Per-Thousand Impressions is " + result.get(0).get(0)[0];

            Series metric = new Series();
            metric.setName("Cost-Per-Thousand Impressions per day");
            List<String[]> l1 = result.get(1);
            int i =0;
            while(i<l1.size()){
                metric.getData().add(new XYChart.Data(l1.get(i)[0], Double.parseDouble(l1.get(i)[1])));
                System.out.println(l1.get(i)[1]);
                i++;
            }

            series_line = new ArrayList<Series>(){
                {
                    add(metric);
                }
            };

            Axis x = new CategoryAxis();
            x.setLabel("Day");
            yAxis.setLabel("Cost-Per-Thousand Impressions");
            setLineChart(buildLineChart(series_line, x, yAxis, "Cost-Per-Thousand Impressions per day"));

            filterSelect.getSelectionModel().clearSelection();
        }

        else if (selected == null){
            // Used on graph reset
            // No actions needed
            metricText = "";
            metricTextFemale = "";
            metricTextMale = "";
        }

        else{
            metricText = selected.getText();
            metricTextFemale = "";
            metricTextMale = "";
            filterSelect.getSelectionModel().clearSelection();
        }

        metricDisplay.setText(metricText);
        metricDisplayFemale.setText(metricTextFemale);
        metricDisplayMale.setText(metricTextMale);
    }

    // What to do when filters are updated
    private void filterUpdate(String newValue){
        if (newValue != null) {
            currentFilter = newValue;
            switch (newValue) {
                case "No Filter":
                    RadioButton current = (RadioButton) metricGroup.getSelectedToggle();
                    Platform.runLater(
                            () -> {
                                current.setSelected(false);
                                current.setSelected(true);
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    appliedFilters.setText("No filters applied.");
                    break;
                case "Date Range":
                    new DateFilter(qry_exec, metricDisplay, bounceRateLabel, getTableName(metricGroup), getColumns(metricGroup), this, ((RadioButton) metricGroup.getSelectedToggle()).getText(), sizeOfFont);
                    Platform.runLater(
                            () -> {
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    break;
                case "Age":
                    //new AgeFilter(dbc, metricDisplay, getTableName(metricGroup), getColumns(metricGroup), this);
                    String[] ages = {"<25", "25-34", "35-44", "45-54", ">54"};
                    new FilterController(qry_exec, metricDisplay, bounceRateLabel, ages, "Age", getTableName(metricGroup), getColumns(metricGroup), this, ((RadioButton) metricGroup.getSelectedToggle()).getText(), sizeOfFont);
                    Platform.runLater(
                            () -> {
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    break;
                case "Context":
                    //new ContextFilter(dbc, metricDisplay, getTableName(metricGroup), getColumns(metricGroup), this);
                    String[] contexts = {"News", "Shopping", "Social Media", "Blog", "Hobbies", "Travel"};
                    new FilterController(qry_exec, metricDisplay, bounceRateLabel, contexts, "Context", getTableName(metricGroup), getColumns(metricGroup), this, ((RadioButton) metricGroup.getSelectedToggle()).getText(), sizeOfFont);
                    Platform.runLater(
                            () -> {
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    break;
                case "Income":
                    //new IncomeFilter(dbc, metricDisplay, getTableName(metricGroup), getColumns(metricGroup), this);
                    String[] incomes = {"High", "Medium", "Low"};
                    new FilterController(qry_exec, metricDisplay, bounceRateLabel, incomes, "Income", getTableName(metricGroup), getColumns(metricGroup), this, ((RadioButton) metricGroup.getSelectedToggle()).getText(), sizeOfFont);
                    Platform.runLater(
                            () -> {
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    break;
                case "Gender":
                    //new GenderFilter(dbc, metricDisplay, getTableName(metricGroup), getColumns(metricGroup), this);
                    String[] genders = {"Male", "Female"};
                    new FilterController(qry_exec, metricDisplay, bounceRateLabel, genders, "Gender",
                            getTableName(metricGroup), getColumns(metricGroup), this, ((RadioButton) metricGroup.getSelectedToggle()).getText(), sizeOfFont);
                    Platform.runLater(
                            () -> {
                                filterSelect.getSelectionModel().clearSelection();
                            }
                    );
                    break;
            }
        }
    }

    public String getGranularity(){
        return granularitySelect.getSelectionModel().getSelectedItem();
    }

    public void showFilterApplied(String filter) {
        appliedFilters.setText("Applied filter:\n\n- "+filter);
    }


    /*
     @desc - populates two series' with test data
     @return - a pair of series containing the test data
     */
    private Pair<Series, Series> createTestSeriesData() {
        Series metric1 = new Series();
        metric1.setName("Metric 1");
        //populating the series with data
        metric1.getData().add(new XYChart.Data(1, 23));
        metric1.getData().add(new XYChart.Data(2, 14));
        metric1.getData().add(new XYChart.Data(3, 15));
        metric1.getData().add(new XYChart.Data(4, 24));
        metric1.getData().add(new XYChart.Data(5, 34));
        metric1.getData().add(new XYChart.Data(6, 36));
        metric1.getData().add(new XYChart.Data(7, 22));
        metric1.getData().add(new XYChart.Data(8, 45));
        metric1.getData().add(new XYChart.Data(9, 43));
        metric1.getData().add(new XYChart.Data(10, 17));
        metric1.getData().add(new XYChart.Data(11, 29));
        metric1.getData().add(new XYChart.Data(12, 25));

        Series metric2 = new Series();
        metric2.setName("Metric 2");
        //populating the series with data
        metric2.getData().add(new XYChart.Data(1, 25));
        metric2.getData().add(new XYChart.Data(2, 10));
        metric2.getData().add(new XYChart.Data(3, 19));
        metric2.getData().add(new XYChart.Data(4, 32));
        metric2.getData().add(new XYChart.Data(5, 33));
        metric2.getData().add(new XYChart.Data(6, 35));
        metric2.getData().add(new XYChart.Data(7, 38));
        metric2.getData().add(new XYChart.Data(8, 40));
        metric2.getData().add(new XYChart.Data(9, 41));
        metric2.getData().add(new XYChart.Data(10, 10));
        metric2.getData().add(new XYChart.Data(11, 24));
        metric2.getData().add(new XYChart.Data(12, 21));

        return new Pair(metric1,metric2);
    }



    private String getTableName(ToggleGroup metricGroup) {
        switch (((RadioButton) metricGroup.getSelectedToggle()).getText()) {
            case "Impressions":
                return "impression_log";
            case "Clicks":
                return "click_log";
            case "Unique Users":
                return "click_log";
            case "Total Cost":
                return "impression_log";
            case "Bounces":
                return "server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id";
        }
        return null;
    }



    private String[] getColumns(ToggleGroup metricGroup) {
        switch (((RadioButton) metricGroup.getSelectedToggle()).getText()) {
            case "Impressions":
                return new String[] {"COUNT("+getTableName(metricGroup)+".id)"};
            case "Clicks":
                return new String[] {"COUNT("+getTableName(metricGroup)+".id)"};
            case "Unique Users":
                return new String[] {"COUNT(DISTINCT "+getTableName(metricGroup)+".id)"};
            case "Total Cost":
                return new String[] {"SUM("+getTableName(metricGroup)+".impression_cost)"};
            case "Bounces":
                return new String[] {"COUNT(impression_log.id)"};
        }
        return null;
    }

    public void setLineChart(LineChart chart){
        this.linechart = chart;
        filtersNGraphs.getChildren().clear();
        filtersNGraphs.getChildren().addAll(filterBox, linechart);
        filtersNGraphs.setHgrow(linechart, Priority.ALWAYS); // Resize chart
    }

    public LineChart getLineChart(){
        return linechart;
    }

    public void setMultiChart(PieChart piechart, LineChart linechart){
        this.piechart = piechart;
        this.linechart = linechart;
        multiGraphs.getChildren().clear();
        multiGraphs.setVgrow(piechart, Priority.SOMETIMES); // Resize chart
        multiGraphs.setVgrow(linechart, Priority.SOMETIMES); // Resize chart
        multiGraphs.getChildren().add(piechart);
        multiGraphs.getChildren().add(linechart);
    }

    public PieChart getPieChart(){
        return piechart;
    }

    private LineChart buildLineChart(ArrayList<Series> series, Axis x, Axis y, String title){
        LineChart linechart = new LineChart(x, y);
        linechart.getData().addAll(series);
        linechart.setTitle(title);
        linechart.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        menuBarHandler.addChart(linechart);
        return linechart;
    }

    private BarChart buildBarChart(ArrayList<XYChart.Series> series, Axis x, Axis y, String title){
        BarChart barchart = new BarChart(x, y);
        barchart.getData().addAll(series);
        barchart.setTitle(title);
        menuBarHandler.addChart(barchart);
        return barchart;
    }

    private PieChart buildPieChart(ArrayList<PieChart.Data> data, String title){
        ObservableList<PieChart.Data> pieChartData = FXCollections.observableArrayList(data);
        final PieChart piechart = new PieChart(pieChartData);
        piechart.setTitle(title);
        piechart.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        menuBarHandler.addChart(piechart);
        return piechart;
    }

    //changes the UI to set size
    private void initFonts(){
        for (Label l : allLabels){
            l.setFont(new Font("Arial", sizeOfFont));
        }

        metrics.getChildren().clear();

        for (RadioButton b : allButtons){
            b.setFont(new Font("Arial", sizeOfFont));
        }

        metrics.getChildren().addAll(imprnsBtn,  clicks,uniqueUsrs,bounces,
                conversions,cost,  ctr,       cpa,
                cpc,        cpm);

        metrics.setSpacing(20);
        fontSize.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        granularitySelect.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        linechart.getXAxis().tickLabelFontProperty().set(Font.font(sizeOfFont));
        yAxis.setTickLabelFont(Font.font(sizeOfFont));
        xAxis.setTickLabelFont(Font.font(sizeOfFont));
        linechart.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        if (piechart != null)
            piechart.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        filterSelect.setStyle("-fx-font: "+sizeOfFont+"px \"Arial\";");
        back.setFont(new Font("Arial", sizeOfFont));

        stage.sizeToScene();
    }
    public MenuBarHandler getMenuBarHandler(){
        return menuBarHandler;
    }
    public Stage getStage(){
        return stage;
    }

}
