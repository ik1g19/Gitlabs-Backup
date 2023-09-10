package src;

public class SQLCommands {

    // Counts
    final static public String numberOfClicks = "SELECT COUNT(id) FROM click_log;";
    final static public String numberOfImpressions = "SELECT COUNT(id) FROM impression_log;";
    final static public String numberOfUniqueUsers = "SELECT COUNT(DISTINCT user_id) FROM click_log;";

    // check that these are 100% correct
    // a bounce is defined as a user viewing only 1 screen, or staying on that site for < 30 seconds
    final static public String numberOfBounces = "SELECT COUNT(id) FROM server_log WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30;";
    final static public String numberOfConversions = "SELECT SUM(conversion) FROM server_log;";
    final static public String totalCost = "SELECT SUM(click_cost) FROM click_log;";
    final static public String clickThroughRate = "SELECT COUNT(click_log.id) / COUNT(impression_log.id) FROM click_log, impression_log;";
    final static public String costPerAcquisition = "SELECT SUM(impression_cost) / SUM(conversion) FROM impression_log, server_log;";
    final static public String costPerClick = "SELECT SUM(click_cost) / COUNT(id) FROM click_log;";
    final static public String costPerThousandImpressions = "SELECT 1000 * SUM(impression_cost) / COUNT(id) FROM impression_log;";
    final static public String bounceRate = "SELECT (CAST (COUNT(server_log.id) AS float)) / (SELECT (CAST (COUNT(click_log.id) AS float)) FROM click_log) FROM server_log WHERE viewed = '1' OR (JULIANDAY(server_log.exit_date) - JULIANDAY(server_log.entry_date)) * 60 * 60 * 24 < 30;";

    // Values
    final static public String clickLogDates = "SELECT distinct date(date) AS uniq_date, COUNT(id) FROM click_log GROUP BY uniq_date"; // Number of clicks per date
    final static public String impressionLogDates = "SELECT distinct date(date) AS uniq_date, COUNT(id) FROM impression_log GROUP BY uniq_date";// Number of impressions per date
    final static public String uniqueUserDates ="SELECT distinct date(date) AS uniq_date, COUNT(DISTINCT user_id) FROM click_log GROUP BY uniq_date";// Number of unique users per date
    final static public String totalCostDates ="SELECT  distinct date(date) AS uniq_date, SUM(impression_cost) FROM impression_log GROUP BY uniq_date";//Total Cost per day
    final static public String bouncesPerDates = "SELECT distinct date(server_log.entry_date) AS uniq_date, COUNT(server_log.id) FROM server_log WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 GROUP BY uniq_date;";
    final static public String totalImpressionCostPerDates = "SELECT distinct date(date) AS uniq_date, SUM(impression_cost) FROM impression_log GROUP BY uniq_date;"; // total impression cost per day
    final static public String totalClickCostPerDates = "SELECT distinct date(date) AS uniq_date, SUM(click_cost) AS total_click_cost FROM click_log GROUP BY uniq_date;"; // total click cost per day
    final static public String totalConversionsPerDates = "SELECT distinct date(entry_date) AS uniq_date, SUM(conversion) FROM server_log GROUP BY uniq_date;";
    final static public String totalClickCostDates="SELECT distinct date(date) AS uniq_date, SUM(click_cost), COUNT(id) FROM click_log GROUP BY uniq_date";

    // Filters

    /*
     Filter a table between two dates
     @param - tableName - Name of table to filter
     @param - startDate - Start Date
     @param - endDate - End Date
     */
    public static String filterByDateRange (String tableName, String startDate, String endDate, String[] columns, String extraParameters){
        return "SELECT "+String.join(", ",columns)+" FROM "+tableName+" WHERE date BETWEEN '"+startDate+"' AND '"+endDate+"'"+extraParameters;
    }

    /*
     Filter table
     @param - tableName - Name of table to filter
     @param - filter - Metric to filter (e.g. income, age, context)
     @param - income - Category as string (e.g. Age: <25, 25-34, 35-44, 45-54, >54 or Income: High, Medium, Low)
     @param - columns - List of column names to get
     */
    public static String filter (String tableName, String filter, String parameter, String[] columns, String extraParameters){

        if (tableName.equals("impression_log")) {
            System.out.println("SELECT "+String.join(", ",columns)+" FROM "+tableName+" WHERE impression_log."+filter+" = '"+parameter+"'"+extraParameters);
            return "SELECT " + String.join(", ", columns) + " FROM " + tableName + " WHERE impression_log." + filter + " = '" + parameter + "'" + extraParameters;
        }else {
            System.out.println("SELECT "+String.join(", ",columns)+" FROM "+tableName+" INNER JOIN impression_log ON "+tableName+".user_id = impression_log.user_id WHERE impression_log."+filter+" = '"+parameter+"'"+extraParameters);
            return "SELECT " + String.join(", ", columns) + " FROM " + tableName + " INNER JOIN impression_log ON " + tableName + ".user_id = impression_log.user_id WHERE impression_log." + filter + " = '" + parameter + "'" + extraParameters;
        }
    }

    /*
     Number of clicks by gender
     @param - gender
     */
    public static String filterClicksByGender (String gender){
        return "SELECT COUNT(DISTINCT click_log.user_id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.gender = '"+ gender +"'";
    }



}
