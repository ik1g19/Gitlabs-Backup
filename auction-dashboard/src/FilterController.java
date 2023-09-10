package src;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.chart.*;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FilterController extends Filter {

    private QueryExecutor qry_exec;
    private String selectedValue;
    private Label metric;
    private String[] filterValues;
    private String filterName;
    private String tableName;
    private String[] columns;
    private DataViewScreen dvs;
    private String metricName;
    private Label bounceRateLabel;
    private int fontSize;
    /*
    @param db_ctrl - The controller for the database
    @param metric - a label that displays the total count for the given metric
    @param filterValues - an array containing all of the values that can be selected for that filter e.g. gender filter would be { "Male", "Female" }
    @param filterName - the name of the filter e.g. "Gender", "Income" etc.
    @param tableName - the name of the table data will be queried from
    @param columns - the return columns for the db query
    @param dvs - the object handling the main screen that is navigated to after the filter is applied or cancelled
    @param metricName - the name of the metric to be filtered
    @param fontSize - size of font to use
     */
    public FilterController(QueryExecutor qry_exec, Label metric, Label bounceRateLabel, String[] filterValues, String filterName, String tableName, String[] columns, DataViewScreen dvs, String metricName, int fontSize) {
        this.qry_exec = qry_exec;
        this.metric = metric;
        this.filterValues = filterValues;
        this.filterName = filterName;
        this.tableName = tableName;
        this.columns = columns;
        this.dvs = dvs;
        this.metricName = metricName;
        this.bounceRateLabel = bounceRateLabel;
        this.fontSize=fontSize;
        load();
    }

    @Override
    public void load() {
        Stage filterStage = new Stage();

        BorderPane borderPane = new BorderPane();
        VBox filterOptions = new VBox();
        HBox controls = new HBox();
        HBox title = new HBox();

        Button[] dataButtons = new Button[filterValues.length];
        for (int i = 0; i < dataButtons.length; i++) {
            Button filterButton = new Button(filterValues[i]);
            dataButtons[i] = filterButton;
            filterOptions.getChildren().add(filterButton);
            filterButton.setOnAction(
                    action -> {
                        setFilterValue(filterButton.getText());
                        filterStage.setScene(super.done());
                        try {
                            java.util.concurrent.TimeUnit.SECONDS.sleep(1);
                        } catch (InterruptedException e) {
                            System.err.println("Interrupted exception: " + e);
                        }
                        filterStage.close();
                        filter();
                    }
            );
        }

        Button cancel = new Button ("Cancel");
        cancel.setOnAction(
                action -> {
                    tearDown();
                    filterStage.close();
                }
        );

        filterOptions.setAlignment(Pos.CENTER);
        filterOptions.setSpacing(10);

        controls.setAlignment(Pos.CENTER);
        controls.setPadding(new Insets(5));
        controls.getChildren().add(cancel);

        title.setAlignment(Pos.CENTER);
        Label top = new Label("Select " + filterName);
        title.getChildren().add(top);

        borderPane.setBottom(controls);
        borderPane.setCenter(filterOptions);
        borderPane.setTop(title);

        initFonts(dataButtons,cancel,top);

        filterStage.setTitle(filterName + " Picker");
        filterStage.setScene(new Scene(borderPane));
        filterStage.setMinHeight(300);
        filterStage.setMinWidth(300);
        filterStage.sizeToScene();
        filterStage.show();
    }

    @Override
    public void filter() {
        // Update Label
        String currentVal = metric.getText();
        String[] valArray = currentVal.split(" ");
        String granularity = dvs.getGranularity();

        /*
        if (metric.getText().contains("bounces")){
            tableName = "server_log";
            System.out.println(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, columns, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"));
            filiteredValue = db_ctrl.dbQuery(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, columns, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),1).get(0)[0];
        } else {
            System.out.println(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, columns, ""));
            filiteredValue = db_ctrl.dbQuery(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, columns, ""),1).get(0)[0];
        }

        // Update Chart
        //System.out.println(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, new String[]{"distinct date("+tableName+".date) AS uniq_date, COUNT("+tableName+".id)"}, " GROUP BY uniq_date"));
        //List<String[]> filteredValues =  db_ctrl.dbQuery(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, new String[]{"distinct date("+tableName+".date) AS uniq_date, COUNT("+tableName+".id)"}, " GROUP BY uniq_date"),2);
        //gets the columns from the given columns, not just counting the ID
        /*
        String[] initial;

        if (metric.getText().contains("bounces")){
            initial = new String[] {"distinct date(impression_log.date) AS uniq_date"};
        } else {
            initial = new String[] {"distinct date("+tableName+".date) AS uniq_date"};
        }
        String[] all = Arrays.copyOf(initial,initial.length + columns.length);
        System.arraycopy(columns, 0, all, initial.length,columns.length);

        //System.out.println(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, all, " GROUP BY uniq_date"));
        List<String[]> filteredValues =  db_ctrl.dbQuery(SQLCommands.filter(tableName, filterName.toLowerCase(), selectedValue, all, " GROUP BY uniq_date"),2);
        */


        List<List<String[]>> queryResponse = qry_exec.executeQuery(metricName,filterName.toLowerCase(),selectedValue, granularity);
        String filiteredValue = queryResponse.get(0).get(0)[0];
        if (filiteredValue == null)
            filiteredValue = "0";
        String updatedMetric = currentVal.replace(valArray[valArray.length - 1], filiteredValue);
        metric.setText(updatedMetric);


        List<String[]> filteredValues = queryResponse.get(1);
        if (metricName.equals("Bounces")) {
            PieChart pieChart = updateBounces(Integer.valueOf(filiteredValue));
            LineChart lineChart = updateBounceRate(filteredValues, granularity);
            pieChart.setStyle("-fx-font: "+fontSize+"px \"Arial\";");
            lineChart.getXAxis().tickLabelFontProperty().set(Font.font(fontSize));
            lineChart.setStyle("-fx-font: "+fontSize+"px \"Arial\";");
            dvs.setMultiChart(pieChart, lineChart);
        } else {
            XYChart.Series data = new XYChart.Series();
            data.setName(dvs.getLineChart().getTitle());


            //cumulative chart for total cost, standard for others
            if (metricName.equals("Total Cost")) {
                double cumulativeCost = 0;
                for (int i = 0; i < filteredValues.size(); i++) {
                    String[] record = filteredValues.get(i);
                    cumulativeCost = cumulativeCost + Double.parseDouble(record[1]);
                    data.getData().add(new XYChart.Data(record[0], cumulativeCost));
                }

            } else {
                for (int i = 0; i < filteredValues.size(); i++) {
                    String[] record = filteredValues.get(i);
                    System.out.println(metric.getText() + " - " + record[1]);
                    data.getData().add(new XYChart.Data(record[0], Double.valueOf(record[1])));
                }
            }

            /*
            for (int i = 0; i < filteredValues.size(); i++) {
                String[] record = filteredValues.get(i);
                data.getData().add(new XYChart.Data(record[0], Double.valueOf(record[1])));
            }
            */
            Axis x = new CategoryAxis();
            NumberAxis y = new NumberAxis();
            y.setLabel(dvs.getLineChart().getYAxis().getLabel());
            x.setLabel("Day");
            //dvs.getLineChart().getYAxis().setLabel(dvs.getLineChart().getYAxis().getLabel());
            LineChart linechart = new LineChart(x,y);
            linechart.getData().addAll(data);
            linechart.setTitle(dvs.getLineChart().getTitle());
            if (metricName.equals("Clicks")){
                PieChart pieChart = dvs.getPieChart();
                pieChart.getData().clear();

                List<String[]> ageValues = qry_exec.executeQuery("Clicks", filterName.toLowerCase(), selectedValue, "age groups",2);
                for(String[] record : ageValues) {
                    pieChart.getData().add(new PieChart.Data(record[0], Integer.valueOf(record[1])));
                }
                dvs.setMultiChart(pieChart, linechart);
            } else
                dvs.setLineChart(linechart);
            linechart.getXAxis().tickLabelFontProperty().set(Font.font(fontSize));
            dvs.getLineChart().getYAxis().setTickLabelFont(Font.font(fontSize));
            x.setTickLabelFont(Font.font(fontSize));
            linechart.setStyle("-fx-font: "+fontSize+"px \"Arial\";");

        }

        dvs.showFilterApplied(filterName + " Filter: " + selectedValue);

    }

    private PieChart updateBounces(Integer bounces) {
        // Update pie chart
        PieChart pieChart = dvs.getPieChart();
        pieChart.getData().clear();
        Integer totalClicks = Integer.valueOf(qry_exec.executeQuery("Clicks", "").get(0).get(0)[0]);
        pieChart.getData().add(new PieChart.Data("Normal clicks",totalClicks-bounces));
        pieChart.getData().add(new PieChart.Data("Bounce clicks", bounces));

        return pieChart;
    }

    private LineChart updateBounceRate(List<String[]> filteredValues, String granularity){

        //String bounceRate = db_ctrl.dbQuery(SQLCommands.filter("server_log",filterName.toLowerCase(),selectedValue, new String[]{"(CAST (COUNT(server_log.id) AS float)) / (SELECT (CAST (COUNT(click_log.id) AS float)) FROM click_log)"}, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),1).get(0)[0];
        String bounceRate = qry_exec.executeQuery(metricName,filterName.toLowerCase(), selectedValue,"bounce rate",1).get(0)[0];
        bounceRateLabel.setText("Bounce rate: "+bounceRate);

        //List<String[]> clicksPerDay = db_ctrl.dbQuery(SQLCommands.clickLogDates, 2);
        List<String[]> clicksPerDay = qry_exec.executeQuery("Clicks", granularity).get(1);
        XYChart.Series metric = new XYChart.Series();
        metric.setName("Bounce rate per day");

        int i =0;
        while(i<filteredValues.size()){
            double avg = (Double.parseDouble(filteredValues.get(i)[1])) / (Double.parseDouble(clicksPerDay.get(i)[1]));
            metric.getData().add(new XYChart.Data(filteredValues.get(i)[0], avg));
            i++;

        }
        ArrayList<XYChart.Series> data = new ArrayList<XYChart.Series>(){
            {
                add(metric);
            }
        };

        Axis x = new CategoryAxis();
        x.setLabel("Day");
        Axis y = new NumberAxis();
        y.setLabel("Bounce Rate");

        LineChart linechart = new LineChart(x, y);
        linechart.getData().addAll(data);
        linechart.setTitle("Bounce rate per day");
        return linechart;
    }

    private void setFilterValue(String value) {
        selectedValue = value;
    }

    @Override
    public void tearDown() {
        this.selectedValue = null;
    }

    public void initFonts(Button[] dataButtons,Button cancel,Label title ){
        metric.setFont(new Font("Arial", fontSize));
        bounceRateLabel.setFont(new Font("Arial", fontSize));
        for(Button b : dataButtons){
            b.setFont(new Font("Arial", fontSize));
        }
        cancel.setFont(new Font("Arial", fontSize));
        title.setFont(new Font("Arial", fontSize));
    }
}
