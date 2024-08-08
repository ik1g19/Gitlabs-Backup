package src;

import SQLBuilder.QueryBuilder;

import java.util.List;

/*
 * Class to provide abstraction between the application and database
 * Builds and executes queries based on application information provided
 */

public class QueryExecutor {

    private QueryBuilder qry_builder;
    private DatabaseController dbc;

    public QueryExecutor (){
        init();
    }

    /*
     * Create connection to database
     */
    public void init(){
        Database db = new Database("./logs.db");
        dbc = new DatabaseController(db);
    }

    /*
     * Use to execute database filter query
     * Return results: [0] = Single value of filtered metric, [1] = Filtered metric value per day
     */
    public  List<List<String[]>> executeQuery (String metric, String filter, String filterValue, String granularity){
        qry_builder  = new QueryBuilder(metric, filter, filterValue, 0, "");
        List<String[]> singleFilterValue = dbc.dbQuery(qry_builder.getQuery(), 1);
        qry_builder  = new QueryBuilder(metric, filter, filterValue, 1, granularity);
        List<String[]> filterValuePerDay = dbc.dbQuery(qry_builder.getQuery(), 2);
        return List.of(singleFilterValue, filterValuePerDay);
    }

    /*
     * Use to execute database query
     * Return results: Single value of filtered specified metric
     * Specified metric options: bounce rate
     */
    public  List<String[]> executeQuery (String metric, String filter, String filterValue, String specificMetric, int columnCount){
        qry_builder  = new QueryBuilder(metric, filter, filterValue, 0, "");
        return dbc.dbQuery(qry_builder.getQuery(specificMetric), columnCount);
    }

    /*
     * Use to execute database query
     * Return results: [0] = Single value of metric, [1] = Metric value per day
     */
    public List<List<String[]>> executeQuery (String metric, String granularity){
        qry_builder  = new QueryBuilder(metric, granularity);
        return List.of(dbc.dbQuery(qry_builder.getQuery(), 1),dbc.dbQuery(qry_builder.getQueryPerDay(), 2));
    }

    /*
     * Use to execute database query
     * Return results: Single value of specified metric
     * Specified metric options: bounce rate
     */
    public List<String[]> executeQuery (String metric, String specificMetric, int columnCount, String granularity){
        qry_builder  = new QueryBuilder(metric, granularity);
        return dbc.dbQuery(qry_builder.getQuery(specificMetric), columnCount);
    }

}
