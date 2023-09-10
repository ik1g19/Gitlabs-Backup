package src;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.chart.*;
import javafx.scene.chart.XYChart.Series;
import javafx.scene.control.Button;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;

import java.lang.reflect.Array;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DateFilter extends Filter {

    private QueryExecutor qry_exec;
    private String startVal;
    private String endVal;
    private Label metric;
    private String tableName;
    private String[] columns;
    private DataViewScreen dvs;
    private String metricName;
    private Label bounceRateLabel;
    private int fontSize;

    public DateFilter(QueryExecutor qry_exec, Label metric, Label bounceRateLabel, String tableName, String[] columns, DataViewScreen dvs, String metricName,int fontSize){
        this.qry_exec = qry_exec;
        this.metric = metric;
        this.tableName = tableName;
        this.columns = columns;
        this.dvs = dvs;
        this.metricName = metricName;
        this.bounceRateLabel = bounceRateLabel;
        this.fontSize=fontSize;
        load();
    }

    public void load (){
        Stage s = new Stage();

        BorderPane bp = new BorderPane();
        VBox datePickers = new VBox();
        HBox controls = new HBox();
        HBox startDate = new HBox();
        HBox endDate = new HBox();
        HBox title = new HBox();

        DatePicker start = new DatePicker();
        DatePicker end = new DatePicker();
        Button finish = new Button ("Finish");
        Button cancel = new Button ("Cancel");
        Label startLabel = new Label("Start Date:");
        Label endLabel = new Label("End Date:");

        start.valueProperty().addListener(
                (obVal, oldValue, newValue) -> {
                    if (end.getValue() != null)
                        finish.setDisable(false);
                }
        );

        end.valueProperty().addListener(
                (obVal, oldValue, newValue) -> {
                    if (start.getValue() != null)
                        finish.setDisable(false);
                }
        );

        finish.setDisable(true);
        finish.setOnAction(
                action -> {
                   setStart(start);
                   setEnd(end);
                   s.setScene(super.done());
                    try {
                        java.util.concurrent.TimeUnit.SECONDS.sleep(1);
                    } catch (InterruptedException e) {
                        System.err.println("Interrupted exception: "+e);
                    }
                    s.close();
                   filter();
                }
        );

        cancel.setOnAction(
                action -> {
                    tearDown();
                    s.close();
                }
        );

        startDate.setAlignment(Pos.CENTER);
        startDate.setSpacing(10);
        startDate.getChildren().addAll(startLabel, start);

        endDate.setAlignment(Pos.CENTER);
        endDate.setSpacing(10);
        endDate.getChildren().addAll(endLabel, end);

        controls.setAlignment(Pos.CENTER);
        controls.setPadding(new Insets(5));
        controls.setSpacing(50);
        controls.getChildren().addAll(cancel, finish);

        datePickers.setAlignment(Pos.CENTER);
        datePickers.setPadding(new Insets(5));
        datePickers.setSpacing(50);
        datePickers.getChildren().addAll(startDate,endDate);

        title.setAlignment(Pos.CENTER);
        Label top = new Label("Select dates");
        title.getChildren().add(top);

        bp.setBottom(controls);
        bp.setCenter(datePickers);
        bp.setTop(title);

        setFont(top,startLabel,endLabel,start,end,cancel,finish);
        s.setTitle("Date Picker");
        s.setScene(new Scene(bp));
        s.setMinHeight(300);
        s.setMinWidth(300);
        s.sizeToScene();
        s.show();

    }


    public void filter (){
        /*
        if (metricName.equals("Bounces")){
            System.out.println(SQLCommands.filterByDateRange(tableName, startVal, endVal, columns, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"));
            filiteredValue = db_ctrl.dbQuery(SQLCommands.filterByDateRange(tableName, startVal, endVal, columns, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),1).get(0)[0];
        } else {
            filiteredValue = db_ctrl.dbQuery(SQLCommands.filterByDateRange(tableName, startVal, endVal, columns, ""),1).get(0)[0];
        }

        // Update Chart
        String[] initial;

        if (metricName.equals("Bounces")){
            initial = new String[] {"distinct date(impression_log.date) AS uniq_date"};
        } else {
            initial = new String[] {"distinct date("+tableName+".date) AS uniq_date"};
        }
        String[] all = Arrays.copyOf(initial,initial.length + columns.length);
        System.arraycopy(columns, 0, all, initial.length,columns.length);
        System.out.println(SQLCommands.filterByDateRange(tableName, startVal, endVal, all, " GROUP BY uniq_date"));
        List<String[]> filteredValues =  db_ctrl.dbQuery(SQLCommands.filterByDateRange(tableName, startVal, endVal, all, " GROUP BY uniq_date"),2);

        */
        // Update Label
        String currentVal = metric.getText();
        String[] valArray = currentVal.split(" ");
        String granularity = dvs.getGranularity();


        List<List<String[]>> result = qry_exec.executeQuery(metricName,"date",startVal+","+endVal, granularity);
        String filiteredValue = result.get(0).get(0)[0];
        if (filiteredValue == null)
            filiteredValue = "0";
        String updatedMetric = currentVal.replace(valArray[valArray.length - 1], filiteredValue);

        metric.setText(updatedMetric);



        List<String[]> filteredValues = result.get(1);

        if (metricName.equals("Bounces")) {
            PieChart pieChart = updateBounces(Integer.valueOf(filiteredValue));
            LineChart lineChart = updateBounceRate(filteredValues, granularity);
            dvs.setMultiChart(pieChart, lineChart);
        } else {
            Series data = new Series();
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
            x.setLabel("Day");
            Axis y = new NumberAxis();
            y.setLabel(dvs.getLineChart().getYAxis().getLabel());
            LineChart linechart = new LineChart(x, y);
            linechart.getData().addAll(data);
            linechart.setTitle(dvs.getLineChart().getTitle());
            if (metricName.equals("Clicks")){
                PieChart pieChart = dvs.getPieChart();
                pieChart.getData().clear();

                List<String[]> ageValues = qry_exec.executeQuery("Clicks", "date", startVal+","+endVal, "age groups",2);
                for(String[] record : ageValues) {
                    pieChart.getData().add(new PieChart.Data(record[0], Integer.valueOf(record[1])));
                }
                dvs.setMultiChart(pieChart, linechart);
            } else
                dvs.setLineChart(linechart);
        }
        dvs.showFilterApplied("Date range: "+startVal+" to "+endVal);
    }

    private PieChart updateBounces(Integer bounces) {
        // Update pie chart
        PieChart pieChart = dvs.getPieChart();
        pieChart.getData().remove(0);
        pieChart.getData().add(new PieChart.Data("Bounce clicks", bounces));

        return pieChart;
    }

    private LineChart updateBounceRate(List<String[]> filteredValues, String granularity){

        String bounceRate = qry_exec.executeQuery(metricName,"bounce rate",1, granularity).get(0)[0];
        bounceRateLabel.setText("Bounce rate: "+bounceRate);

        //List<String[]> clicksPerDay = db_ctrl.dbQuery(SQLCommands.clickLogDates, 2);
        List<String[]> clicksPerDay = qry_exec.executeQuery("Clicks", granularity).get(1);
        Series metric = new Series();
        metric.setName("Bounce rate per day");

        int i =0;
        while(i<filteredValues.size()){
            double avg = (Double.parseDouble(filteredValues.get(i)[1])) / (Double.parseDouble(clicksPerDay.get(i)[1]));
            metric.getData().add(new XYChart.Data(filteredValues.get(i)[0], avg));
            i++;

        }
        ArrayList<Series> data = new ArrayList<Series>(){
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
        linechart.getXAxis().tickLabelFontProperty().set(Font.font(fontSize));
        y.setTickLabelFont(Font.font(fontSize));
        x.setTickLabelFont(Font.font(fontSize));
        linechart.setStyle("-fx-font: "+fontSize+"px \"Arial\";");
        return linechart;
    }


    public void tearDown(){
        this.startVal = null;
        this.endVal = null;

    }

    private void setStart(DatePicker start) {
        startVal = start.getValue().format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    private void setEnd (DatePicker end){
        endVal = end.getValue().format(DateTimeFormatter.ISO_LOCAL_DATE);
    }

    public String getStart(){return startVal;}

    public String getEnd(){return endVal;}

    private void setFont(Label top,Label startLabel,Label endLabel,DatePicker start,DatePicker end,Button cancel, Button finish){
        top.setFont(new Font("Arial", fontSize));
        startLabel.setFont(new Font("Arial", fontSize));
        endLabel.setFont(new Font("Arial", fontSize));
        cancel.setFont(new Font("Arial", fontSize));
        finish.setFont(new Font("Arial", fontSize));
        start.setStyle("-fx-font: "+fontSize+"px \"Arial\";");
        end.setStyle("-fx-font: "+fontSize+"px \"Arial\";");

    }

}
