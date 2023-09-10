package SQLBuilder;

/*
 * Takes application information to build SQL queries
 */

import java.util.HashMap;

public class QueryBuilder {

    private String query;
    private String queryPerDay;
    private static final SQLBuilder builder = new SQLBuilder();
    private HashMap<String, String> extraQueries;
    private String granulStment;

    public String granularityToStatement(String granularity){
        return switch (granularity) {
            case "Hour" -> "strftime('%Y-%m-%d %H',";
            case "Day" -> "date(";
            case "Week" -> "strftime('%Y-%m week-%W', ";
            case "Month" -> "strftime('%Y-%m-01', ";
            default -> "";
        };
    }


    // Filter queries constructor
    public QueryBuilder (String metric, String filter, String filterVal, int grouped, String granularity){
        String query;
        this.granulStment = granularityToStatement(granularity);
        if (grouped == 1)
            switch (filter){
                case "age":
                case "context":
                case "income":
                case "gender":
                    query = filterGroup (metric, filter, "'"+filterVal+"'");
                    break;
                case "date":
                    String[] dates = filterVal.split(",");
                    query =  dateFilterGroup (metric, dates[0], dates[1]);
                    break;
                default:
                    query = null;
                    break;
            }
        else
            switch (filter){
                case "age":
                case "context":
                case "income":
                case "gender":
                    query = filter (metric, filter, "'"+filterVal+"'");
                    break;
                case "date":
                    String[] dates = filterVal.split(",");
                    query =  dateFilter (metric, dates[0], dates[1]);
                    break;
                default:
                    query = null;
                    break;
            }

        this.query = query;
        this.queryPerDay = null;
    }

    // Normal queries constructor
    public QueryBuilder (String metric, String granularity){
        String[] queries;
        this.granulStment = granularityToStatement(granularity);
        HashMap<String, String> extraQueries = new HashMap<>(); // Any additional queries for the metric
        switch (metric){
            case "Impressions":
                queries = buildImpressions();
                break;
            case "Clicks":
                queries = buildClicks();
                extraQueries.put("age groups",queries[2]);
                break;
            case "Unique Users":
                queries = buildUnique();
                break;
            case "Bounces":
                queries = buildBounces();
                extraQueries.put("bounce rate",queries[2]);
                break;
            case "Conversions":
                queries = buildConv();
                break;
            case "Total Cost":
                queries = buildTotalCost();
                break;
            case "Click-Through-Rate":
                queries = buildCTR();
                break;
            case "Cost-Per-Acquisition":
                queries = buildCPA();
                break;
            case "Cost-Per-Click":
                queries = buildCPC();
                break;
            case "Cost-Per-Thousand Impressions":
                queries = buildCPTI();
                break;
            default:
                queries = new String[]{""};
                break;
        }

        this.query = queries[0];
        this.queryPerDay = queries[1];
        this.extraQueries = extraQueries;
    }

    private String dateFilterGroup(String metric, String start, String end) {
        String query;
        HashMap<String, String> extraQueries = new HashMap<>(); // Any additional queries for the metric
        switch (metric) {
            case "Impressions":
                query = builder.filter(new String[]{"impression_log"}, new String[]{"date(immpression_log.date)"},start, end,
                        new String[]{granulStment + "impression_log.date) AS granularity","COUNT(id)"}, "GROUP BY granularity");
                break;
            case "Clicks":
                query = builder.filter(new String[]{"click_log"}, new String[]{"date(immpression_log.date)"},start, end,
                        new String[]{granulStment + "date) AS granularity","COUNT(id)"}, "GROUP BY granularity");
                break;
            case "Unique Users":
                query = builder.filter(new String[]{"click_log"}, new String[]{"date(immpression_log.date)"},start, end,
                        new String[]{granulStment + "click_log.date) AS granularity","COUNT(DISTINCT id)"}, "GROUP BY granularity");
                break;
            case "Bounces":
                query = builder.filter(new String[]{builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")},
                        new String[]{"date(immpression_log.date)"},start, end, new String[]{granulStment + "impression_log.date) AS granularity","COUNT(server_log.id)"}, "AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30) GROUP BY granularity");
                break;
            case "Conversions":
                // To do
                query = null;
                break;
            case "Total Cost":
                String impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
                String click_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"}, new String[]{"click_log"}, "", "GROUP BY granularity");
                query = builder.filter(new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, new String[]{"impression_tbl.granularity"},start, end, new String[]{"impression_tbl.granularity", "impr_sum+click_sum"}, "");
                break;
            case "Click-Through-Rate":
                // To do
                String imp_tbl = builder.filter(new String[] {"impression_log"}, new String[] {"impression_log.date"}, start, end,
                        new String[] {granulStment + "date) as granularity", "COUNT(user_id) as impression_count"}, "GROUP BY granularity1");
                String cli_tbl = builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")},
                        new String[] {"click_log.date"}, start, end, new String[] {granulStment + "click_log.date) as granularity2", "COUNT(click_log.user_id) as click_count"}, "GROUP BY granularity2");
                query = builder.selectStatement(new String[] {"granularity1", "click_count*1.0 / impression_count*1.0"}, new String[] {builder.innerJoin("(" + imp_tbl + ")", "(" + cli_tbl + ")", "granularity1", "granularity2")}, "", "");
                break;
            case "Cost-Per-Acquisition":
                impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
                click_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"}, new String[]{"click_log"}, "", "GROUP BY granularity");
                String conv_tbl = builder.selectStatement(new String[]{granulStment + "entry_date) AS granularity", "COUNT(id) AS conv"}, new String[]{"server_log"}, builder.whereClause(new String[]{"conversion = 1"}), "GROUP BY granularity");
                String joins = builder.innerJoin(builder.innerJoin("("+impression_tbl+") AS impression_tbl", "("+click_tbl+") AS click_tbl", "impression_tbl.granularity", "click_tbl.granularity"), "("+conv_tbl+") AS conv_tbl","conv_tbl.granularity", "click_tbl.granularity");
                query = builder.filter(new String[]{joins}, new String[]{"impression_tbl.granularity"},start, end, new String[]{"impression_tbl.granularity", "impr_sum+click_sum / conv"}, "");
                break;
            case "Cost-Per-Click":
                // Do this
                query = builder.filter(new String[]{builder.innerJoin("click_log","impression_log","impression_log.user_id","click_log.user_id")}, new String[]{"date(impression_log.date)"},start, end, new String[] {granulStment + "impression_log.date) AS granularity", "SUM(click_cost) / COUNT(click_log.id)"}, "GROUP BY granularity");
                break;
            case "Cost-Per-Thousand Impressions":
                // To do
                impression_tbl = builder.filter(new String[] {"impression_log"}, new String[] {"impression_log.date"}, start, end,
                        new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum", "COUNT(id) as total_impressions"},  "GROUP BY granularity");

                click_tbl = builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")},
                        new String[] {"click_log.date"}, start, end, new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"},  "GROUP BY granularity");
                query = builder.selectStatement(new String[]{"impression_tbl.granularity", "1000*(impr_sum+click_sum)/total_impressions"}, new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, "", "");
                System.out.println(query);
                break;
            default:
                query = null;
                break;
        }

        return query;
    }

    // Create date filter query
    private String dateFilter(String metric, String start, String end) {
        String query;
        String impression_tbl;
        HashMap<String, String> extraQueries = new HashMap<>(); // Any additional queries for the metric
        switch (metric) {
            case "Impressions":
                query = builder.filter(new String[]{"impression_log"}, new String[]{"date"},start, end, new String[]{"COUNT(id)"}, "");
                break;
            case "Clicks":
                query = builder.filter(new String[]{"click_log"}, new String[]{"date"},start, end, new String[]{"COUNT(id)"}, "");
                extraQueries.put("age groups", builder.selectStatement(new String[]{"DISTINCT age AS uniq_age", "COUNT(DISTINCT click_log.id)"}, new String[]{builder.innerJoin("click_log", "impression_log","impression_log.user_id", "click_log.user_id")}, builder.whereClause(new String[]{"click_log.date BETWEEN '"+start+"' AND '"+end+"'"}), "GROUP BY uniq_age"));
                break;
            case "Unique Users":
                query = builder.filter(new String[]{"click_log"}, new String[]{"date"},start, end, new String[]{"COUNT(DISTINCT id)"}, "");
                break;
            case "Bounces":
                query = builder.filter(new String[]{builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")}, new String[]{"date"},start, end, new String[]{"COUNT(server_log.id)"}, "AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)");
                String srv_tbl = builder.selectStatement(new String[]{"COUNT(server_log.id) as srv_count"},new String[]{builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")}, builder.whereClause(new String[]{"viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30", "impression_log.date BETWEEN '"+start+"' AND '"+end+"'"}), "");
                String click_tbl = builder.selectStatement(new String[]{"COUNT(click_log.id) as click_count"},new String[]{builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")}, builder.whereClause(new String[]{"impression_log.date BETWEEN '"+start+"' AND '"+end+"'"}), "");
                extraQueries.put("bounce rate", builder.selectStatement(new String[]{"(CAST (srv_count AS float)) / (CAST (click_count AS float))"}, new String[]{"("+srv_tbl+")", "("+click_tbl+")"},"",""));
                break;
            case "Conversions":
                // To do
                query = null;
                break;
            case "Total Cost":
                query = builder.filter(new String[]{"impression_log"}, new String[]{"date"},start, end, new String[]{"ROUND(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"}, new String[]{"click_log"}, "", "")+"),2)"}, "");
                break;
            case "Click-Through-Rate":
                // To do
                String impr_tbl = builder.filter(new String[] {"impression_log"}, new String[] {"impression_log.date"}, start, end, new String[] {"COUNT(impression_log.user_id)"}, "");
                query = builder.filter(new String[] {"click_log"}, new String[] {"click_log.date"}, start, end, new String[] {"COUNT(click_log.user_id)*1.0 / (" + impr_tbl + ")*1.0"}, "");
                break;
            case "Cost-Per-Acquisition":
                query = builder.filter(new String[]{"impression_log"}, new String[]{"date"},start, end,
                        new String[]{"(ROUND(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"}, new String[]{"click_log"}, "", "")+"),2))"+" / ("+ builder.selectStatement(new String[]{"COUNT(id)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"conversion = 1"}), "")+")"}, "");
                break;
            case "Cost-Per-Click":
                query = builder.filter(new String[]{builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")}, new String[]{"impression_log.date"},start, end, new String[] {"SUM(click_cost) / COUNT(click_log.id)"}, "");
                break;
            case "Cost-Per-Thousand Impressions":
                // To do
                impression_tbl = builder.filter(new String[] {"impression_log"}, new String[] {"impression_log.date"}, start, end, new String[] {"SUM(impression_cost) AS impr_sum", "COUNT(id) as total_impressions"}, "");

                click_tbl = builder.selectStatement(new String[]{"sum(click_cost) as click_sum"}, new String[]{"(" +
                        builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")}, new String[] {"impression_log.date"}, start, end
                                , new String[] {"DISTINCT click_log.id", "click_cost"}, "") + ")"}, "", "");
                query = builder.selectStatement(new String[] {"1000*(impr_sum+click_sum)/total_impressions"}, new String[] {"(" + impression_tbl + ")", "(" + click_tbl + ")"}, "","");
                System.out.println(query);
                break;
            default:
                query = null;
                break;
        }
        this.extraQueries = extraQueries;
        return query;
    }

    private String filterGroup(String metric, String filter, String filterVal) {
        String query;
        String granulColumn;
        String[] columns;
        HashMap<String, String> extraQueries = new HashMap<>(); // Any additional queries for the metric
        switch (metric) {
            case "Impressions":
                granulColumn = granulStment+"impression_log.date) AS granularity";
                columns = new String[]{granulColumn,"COUNT(id)"};
                query = builder.filter(new String[]{"impression_log"},filter,"impression_log",filterVal, columns, "GROUP BY granularity");
                break;
            case "Clicks":
                granulColumn = granulStment+"impression_log.date) AS granularity";
                columns = new String[]{granulColumn,"COUNT(click_log.id)"};
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,
                        "impression_log",filterVal, columns, "GROUP BY granularity");
                break;
            case "Unique Users":
                granulColumn = granulStment+"impression_log.date) AS granularity";
                columns = new String[]{granulColumn,"COUNT(DISTINCT click_log.id)"};
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,
                        "impression_log",filterVal, columns , "GROUP BY granularity");
                break;
            case "Bounces":
                granulColumn = granulStment+"impression_log.date) AS granularity";
                columns = new String[]{granulColumn,"COUNT(server_log.id)"};
                query = builder.filter(new String[] {builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")},filter,
                        "impression_log",filterVal, columns, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30) GROUP BY granularity");
                break;
            case "Conversions":
                // To do
                query = null;
                break;
            case "Total Cost":
                String impression_tbl = builder.selectStatement(new String[]{granulStment+"impression_log.date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"},
                        builder.whereClause(new String[]{filter+" = "+filterVal}), "GROUP BY granularity");
                String click_tbl = builder.selectStatement(new String[]{granulStment + "click_log.date) AS granularity", "SUM(click_cost) AS click_sum"},
                        new String[]{builder.innerJoin("click_log","impression_log","click_log.user_id", "impression_log.user_id")},
                        builder.whereClause(new String[]{"impression_log."+filter+" = "+filterVal}), "GROUP BY granularity");
                String joins = builder.innerJoin("("+impression_tbl+") AS impression_tbl", "("+click_tbl+") AS click_tbl", "impression_tbl.granularity", "click_tbl.granularity");
                query = builder.selectStatement(new String[]{"impression_tbl.granularity", "impr_sum+click_sum"}, new String[]{joins},"" , "");
                break;
            case "Click-Through-Rate":
                String imp_tbl = builder.filter(new String[] {"impression_log"}, filter, "impression_log", filterVal,
                        new String[] {granulStment + "date) as granularity1", "COUNT(user_id) as impression_count"}, "GROUP BY granularity1");
                String cli_tbl = builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")}, filter,
                        "impression_log", filterVal, new String[] {granulStment + "click_log.date) as granularity2", "COUNT(click_log.user_id) as click_count"}, "GROUP BY granularity2");
                query = builder.selectStatement(new String[] {"granularity1", "click_count*1.0 / impression_count*1.0"}, new String[] {builder.innerJoin("(" + imp_tbl + ")", "(" + cli_tbl + ")", "granularity1", "granularity2")}, "", "");
                break;
            case "Cost-Per-Acquisition":
                impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"},
                        new String[]{"impression_log"}, builder.whereClause(new String[]{filter+" = "+filterVal}), "GROUP BY granularity");
                click_tbl = builder.selectStatement(new String[]{granulStment + "click_log.date) AS granularity", "SUM(click_cost) AS click_sum"},
                        new String[]{builder.innerJoin("click_log","impression_log","click_log.user_id", "impression_log.user_id")},
                        builder.whereClause(new String[]{"impression_log."+filter+" = "+filterVal}), "GROUP BY granularity");
                String conv_tbl = builder.selectStatement(new String[]{granulStment + "entry_date) AS granularity", "COUNT(server_log.id) AS conv"},
                        new String[]{builder.innerJoin("server_log","impression_log","server_log.user_id", "impression_log.user_id")},
                        builder.whereClause(new String[]{"impression_log."+filter+" = "+filterVal,"conversion = 1"}), "GROUP BY granularity");
                joins = builder.innerJoin(builder.innerJoin("("+impression_tbl+") AS impression_tbl", "("+click_tbl+") AS click_tbl", "impression_tbl.granularity",
                        "click_tbl.granularity"), "("+conv_tbl+") AS conv_tbl","conv_tbl.granularity", "click_tbl.granularity");
                query = builder.selectStatement(new String[]{"impression_tbl.granularity", "impr_sum+click_sum / conv"}, new String[]{joins},"" , "");
                break;
            case "Cost-Per-Click":
                columns = new String[]{granulStment+"impression_log.date) as granularity","SUM(click_cost) / COUNT(click_log.id)"};
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,
                        "impression_log", filterVal, columns, "GROUP BY granularity");
                break;
            case "Cost-Per-Thousand Impressions":
                // To do
                impression_tbl = builder.filter(new String[] {"impression_log"}, filter, "impression_log", filterVal,
                        new String[] {granulStment + "impression_log.date) AS granularity","SUM(impression_cost) AS impr_sum", "COUNT(id) as total_impressions"}, "GROUP BY granularity");

                String[] cols = new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"};
                String[] tables = new String[]{"(" + builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")},
                        filter, "impression_log", filterVal,
                        new String[] {"DISTINCT click_log.id", "click_cost", "click_log.date"}, "") + ")"};

                click_tbl = builder.selectStatement(cols, tables, "", "GROUP BY granularity");

                query = builder.selectStatement(new String[]{"impression_tbl.granularity", "1000*(impr_sum+click_sum)/total_impressions"}, new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, "", "");
                System.out.println(query);
                break;
            default:
                query = null;
                break;
        }

        return query;
    }

    // Create filter query
    private String filter(String metric, String filter, String filterVal) {
        String query;
        String impression_tbl;
        String click_tbl;

        HashMap<String, String> extraQueries = new HashMap<>(); // Any additional queries for the metric
        switch (metric) {
            case "Impressions":
                query = builder.filter(new String[]{"impression_log"},filter,"impression_log",filterVal,new String[]{"COUNT(id)"}, "");
                break;
            case "Clicks":
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,"impression_log",filterVal, new String[]{"COUNT(click_log.id)"}, "");
                extraQueries.put("age groups", builder.selectStatement(new String[]{"DISTINCT age AS uniq_age", "COUNT(DISTINCT click_log.id)"},
                        new String[]{builder.innerJoin("click_log", "impression_log","impression_log.user_id", "click_log.user_id")},
                        builder.whereClause(new String[]{"impression_log."+filter+" = "+filterVal}), "GROUP BY uniq_age"));
                break;
            case "Unique Users":
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,"impression_log",filterVal, new String[]{"COUNT(DISTINCT click_log.id)"}, "");
                break;
            case "Bounces":
                query = builder.filter(new String[] {builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")},filter,"impression_log",filterVal, new String[]{"COUNT(server_log.id)"}, " AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)");
                String srv_tbl = builder.selectStatement(new String[]{"COUNT(server_log.id) as srv_count"},new String[]{builder.innerJoin("server_log","impression_log","server_log.user_id","impression_log.user_id")}, builder.whereClause(new String[]{"viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30", "impression_log."+filter+" = "+filterVal}), "");
                click_tbl = builder.selectStatement(new String[]{"COUNT(click_log.id) as click_count"},new String[]{builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")}, builder.whereClause(new String[]{"impression_log."+filter+" = "+filterVal}), "");
                extraQueries.put("bounce rate", builder.selectStatement(new String[]{"(CAST (srv_count AS float)) / (CAST (click_count AS float))"}, new String[]{"("+srv_tbl+")", "("+click_tbl+")"},"",""));
                break;
            case "Conversions":
                // To do
                query = null;
                break;
            case "Total Cost":
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,"impression_log", filterVal, new String[]{"ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)"}, "");
                break;
            case "Click-Through-Rate":
                // To do
                String impressionsCount = builder.filter(new String[] {"impression_log"}, filter, "impression_log", filterVal, new String[] {"COUNT(impression_log.user_id) as impression_count"}, "");
                String clickCount = builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")}, filter, "impression_log", filterVal, new String[] {"COUNT(click_log.user_id) as click_count"}, "");
                query = builder.selectStatement(new String[] {"click_count*1.0 / impression_count*1.0"}, new String[] {"(" + impressionsCount + ")", "(" + clickCount + ")"}, "","");
                break;
            case "Cost-Per-Acquisition":
                query = builder.filter(new String[] {"impression_log"},filter,"impression_log", filterVal, new String[]{"(ROUND(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"}, new String[]{"click_log"}, "", "")+"),2)) / ("+ builder.selectStatement(new String[]{"COUNT(id)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"conversion = 1"}), "")+")"}, "");
                break;
            case "Cost-Per-Click":
                query = builder.filter(new String[] {builder.innerJoin("click_log","impression_log","click_log.user_id","impression_log.user_id")},filter,"impression_log", filterVal, new String[]{"SUM(click_cost) / COUNT(click_log.id)"}, "");
                break;
            case "Cost-Per-Thousand Impressions":
                // To do
                impression_tbl = builder.filter(new String[] {"impression_log"}, filter, "impression_log", filterVal, new String[] {"SUM(impression_cost) AS impr_sum", "COUNT(id) as total_impressions"}, "");
                click_tbl = builder.selectStatement(new String[]{"sum(click_cost) as click_sum"}, new String[]{"(" +
                        builder.filter(new String[] {builder.innerJoin("click_log", "impression_log", "click_log.user_id", "impression_log.user_id")}, filter, "impression_log", filterVal,
                    new String[] {"DISTINCT click_log.id", "click_cost"}, "") + ")"}, "", "");

                query = builder.selectStatement(new String[] {"1000*(impr_sum+click_sum)/total_impressions"}, new String[] {"(" + impression_tbl + ")", "(" + click_tbl + ")"}, "","");
                System.out.println(query);
                break;
            default:
                query = null;
                break;
        }
        this.extraQueries = extraQueries;
        return query;
    }

    private String[] buildCPTI() {
        String impression_tbl = builder.selectStatement(new String[]{granulStment + "impression_log.date) AS granularity", "SUM(impression_cost) AS impr_sum", "COUNT(id) as total_impressions"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
        String click_tbl = builder.selectStatement(new String[]{granulStment + "click_log.date) AS granularity", "SUM(click_cost) AS click_sum"}, new String[]{"click_log"}, "", "GROUP BY granularity");

        String a = builder.selectStatement(new String[]{"impression_tbl.granularity", "1000*(impr_sum+click_sum)/total_impressions"},
                new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, "", "");

        System.out.println("REE: " + a);
        return new String[]{
                builder.selectStatement(new String[]{"1000*(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"},
                        new String[]{"click_log"}, "", "")+"))/ COUNT(impression_log.id)"}, new String[]{"impression_log"}, "",""),
                a
                };

    }

    private String[] buildCPC() {
        String impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
        String click_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum", "COUNT(click_log.id) as total_clicks"}, new String[]{"click_log"}, "", "GROUP BY granularity");

        return new String[] {
                builder.selectStatement(new String[]{"(SUM(click_cost)+("+ builder.selectStatement(new String[]{"SUM(impression_cost)"},
                        new String[]{"impression_log"},"","") + "))/ COUNT(click_log.id)"}, new String[]{"click_log"}, "",""),
                builder.selectStatement(new String[]{"impression_tbl.granularity", "(impr_sum+click_sum)/total_clicks"}, new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, "", "")
        };
    }

    private String[] buildCPA() {
        String impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
        String click_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"}, new String[]{"click_log"}, "", "GROUP BY granularity");
        String conv_tbl = builder.selectStatement(new String[]{granulStment + "entry_date) AS granularity", "COUNT(id) AS conv"}, new String[]{"server_log"}, builder.whereClause(new String[]{"conversion = 1"}), "GROUP BY granularity");
        String joins = builder.innerJoin(builder.innerJoin("("+impression_tbl+") AS impression_tbl", "("+click_tbl+") AS click_tbl", "impression_tbl.granularity", "click_tbl.granularity"), "("+conv_tbl+") AS conv_tbl","conv_tbl.granularity", "click_tbl.granularity");
        return new String[] {
                builder.selectStatement(new String[]{"(ROUND(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"}, new String[]{"click_log"}, "", "")+"),2))"+" / ("+ builder.selectStatement(new String[]{"COUNT(id)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"conversion = 1"}), "")+")"},new String[]{"impression_log"},"",""),
                builder.selectStatement(new String[]{"impression_tbl.granularity", "impr_sum+click_sum / conv"}, new String[]{joins},"","")
        };
    }

    private String[] buildCTR() {
        String totalCTR = builder.selectStatement(new String[]{"COUNT(click_log.id)*1.0 / (" + builder.selectStatement(new String[]{"COUNT(impression_log.id)"}, new String[]{"impression_log"}, "","") + ")*1.0"}, new String[]{"click_log"}, "","");
        String dailyCTR = builder.selectStatement(new String[]{granulStment + "click_log.date) as granularity1", "COUNT(click_log.id)*1.0 / impression_count"}, new String[]{builder.innerJoin("click_log", "("
                + builder.selectStatement(new String[]{granulStment + "impression_log.date) as granularity2", "COUNT(impression_log.id) as impression_count"}, new String[]{"impression_log"}, "", "GROUP BY granularity2") + ")", "granularity1", "granularity2"),}, "", "GROUP BY granularity2");
        return new String[] {totalCTR,dailyCTR};
    }

    /* Note: Per day query does NOT do the cumulative total, this will have to be calculated manually */
    private String[] buildTotalCost() {
        String impression_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(impression_cost) AS impr_sum"}, new String[]{"impression_log"}, "", "GROUP BY granularity");
        String click_tbl = builder.selectStatement(new String[]{granulStment + "date) AS granularity", "SUM(click_cost) AS click_sum"}, new String[]{"click_log"}, "", "GROUP BY granularity");
        return new String[] {
                builder.selectStatement(new String[]{"ROUND(SUM(impression_cost)+("+ builder.selectStatement(new String[]{"SUM(click_cost)"}, new String[]{"click_log"}, "", "")+"),2)"}, new String[]{"impression_log"}, "",""),
                builder.selectStatement(new String[]{"impression_tbl.granularity", "impr_sum+click_sum"}, new String[]{builder.innerJoin("("+impression_tbl+") AS impression_tbl","("+click_tbl+") AS click_tbl","impression_tbl.granularity","click_tbl.granularity")}, "", "")
        };
    }

    private String[] buildConv() {
        return null;
    }

    private String[] buildBounces() {
        return new String[] {
                builder.selectStatement(new String[]{"COUNT(id)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30"}),""),
                builder.selectStatement(new String[]{granulStment + "server_log.entry_date) AS granularity", "COUNT(server_log.id)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30"}), "GROUP BY granularity"),
                builder.selectStatement(new String[]{"(CAST (COUNT(server_log.id) AS float)) / (SELECT (CAST (COUNT(click_log.id) AS float)) FROM click_log)"}, new String[]{"server_log"}, builder.whereClause(new String[]{"viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30"}),"")
        };
    }

    private String[] buildUnique() {
        return new String[] {
                builder.selectStatement(new String[]{"COUNT(DISTINCT user_id)"},new String[]{"click_log"},"",""),
                builder.selectStatement(new String[]{granulStment + "date) AS granularity", "COUNT(DISTINCT user_id)"}, new String[]{"click_log"},"","GROUP BY granularity")
        };
    }

    private String[] buildClicks() {
        return new String[] {
                builder.selectStatement(new String[]{"COUNT(id)"},new String[]{"click_log"},"",""),
                builder.selectStatement(new String[]{granulStment + "date) AS granularity", "COUNT(id)"}, new String[]{"click_log"},"","GROUP BY granularity"),
                builder.selectStatement(new String[]{"DISTINCT age AS uniq_age", "COUNT(DISTINCT click_log.id)"}, new String[]{builder.innerJoin("click_log", "impression_log","impression_log.user_id", "click_log.user_id")}, "", "GROUP BY uniq_age")
        };
    }

    private String[] buildImpressions() {
        return new String[] {
          builder.selectStatement(new String[]{"COUNT(id)"}, new String[]{"impression_log"},"",""),
          builder.selectStatement(new String[]{granulStment + "impression_log.date) AS granularity", "COUNT(id)"}, new String[]{"impression_log"},"","GROUP BY granularity")
        };
    }

    public String getQueryPerDay() {
        return queryPerDay;
    }

    public String getQuery() {
        return query;
    }

    // Overloaded for extra queries by name
    public String getQuery(String name){
        return extraQueries.get(name);
    }
}
