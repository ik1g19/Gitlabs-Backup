package UnitTesting;

import SQLBuilder.QueryBuilder;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SQLBuildingTests {

    QueryBuilder qry;

    @AfterEach
    void tearDown() {
        qry = null;
    }

    @Test
    @DisplayName("Impressions")
    public void impressionQueryTest(){
        qry = new QueryBuilder("Impressions", "day");
        assertEquals(qry.getQuery(), "SELECT COUNT(id) FROM impression_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT distinct date(date) AS uniq_date, COUNT(id) FROM impression_log GROUP BY uniq_date");
    }

    @Test
    @DisplayName("Clicks")
    public void clicksQueryTest(){
        qry = new QueryBuilder("Clicks", "day");
        assertEquals(qry.getQuery(), "SELECT COUNT(id) FROM click_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT distinct date(date) AS uniq_date, COUNT(id) FROM click_log GROUP BY uniq_date");
        assertEquals(qry.getQuery("age groups"), "SELECT DISTINCT age AS uniq_age, COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON impression_log.user_id = click_log.user_id GROUP BY uniq_age");
    }

    @Test
    @DisplayName("Unique Users")
    public void uniqQueryTest(){
        qry = new QueryBuilder("Unique Users", "day");
        assertEquals(qry.getQuery(), "SELECT COUNT(DISTINCT user_id) FROM click_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT distinct date(date) AS uniq_date, COUNT(DISTINCT user_id) FROM click_log GROUP BY uniq_date");
    }

    @Test
    @DisplayName("Bounces")
    public void bouncesQueryTest(){
        qry = new QueryBuilder("Bounces", "day");
        assertEquals(qry.getQuery(), "SELECT COUNT(id) FROM server_log WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 ");
        assertEquals(qry.getQueryPerDay(), "SELECT distinct date(server_log.entry_date) AS uniq_date, COUNT(server_log.id) FROM server_log WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 GROUP BY uniq_date");
        assertEquals(qry.getQuery("bounce rate"), "SELECT (CAST (COUNT(server_log.id) AS float)) / (SELECT (CAST (COUNT(click_log.id) AS float)) FROM click_log) FROM server_log WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 ");
    }

    @Test
    @DisplayName("Total Cost")
    public void totalCostQueryTest(){
        qry = new QueryBuilder("Total Cost", "day");
        assertEquals(qry.getQuery(), "SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM impression_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT impression_tbl.uniq_date, impr_sum+click_sum FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date ");
    }

    @Test
    @DisplayName("Cost-per-click")
    public void CPCQueryTest(){
        qry = new QueryBuilder("Cost-Per-Click", "day");
        assertEquals(qry.getQuery(), "SELECT SUM(click_cost) / COUNT(id) FROM click_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT distinct date(date) AS uniq_date, SUM(click_cost) / COUNT(click_log.id) FROM click_log GROUP BY uniq_date");
    }

    @Test
    @DisplayName("Cost-per-acquisition")
    public void CPAQueryTest(){
        qry = new QueryBuilder("Cost-Per-Acquisition", "day");
        assertEquals(qry.getQuery(), "SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log ");
        assertEquals(qry.getQueryPerDay(), "SELECT impression_tbl.uniq_date, impr_sum+click_sum / conv FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date INNER JOIN (SELECT distinct date(entry_date) AS uniq_date, COUNT(id) AS conv FROM server_log WHERE conversion = 1 GROUP BY uniq_date) AS conv_tbl ON conv_tbl.uniq_date = click_tbl.uniq_date ");
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("imprCorrectQueries")
    @DisplayName("Impression - Filter")
    void impressionFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Impressions", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream imprCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT COUNT(id) FROM impression_log WHERE impression_log.age = '35-44' "),
                Arguments.of("income","High",0,"SELECT COUNT(id) FROM impression_log WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT COUNT(id) FROM impression_log WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT COUNT(id) FROM impression_log WHERE date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT COUNT(id) FROM impression_log WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT DISTINCT date(date) AS uniq_date, COUNT(id) FROM impression_log WHERE impression_log.age = '35-44' GROUP BY uniq_date"),
                Arguments.of("date","2015-01-01,2015-01-10",1, "SELECT DISTINCT date(impression_log.date) AS uniq_date, COUNT(id) FROM impression_log WHERE uniq_date BETWEEN '2015-01-01' AND '2015-01-10' GROUP BY uniq_date")
        );
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("clickCorrectQueries")
    @DisplayName("Clicks - Filter")
    void clickFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Clicks", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream clickCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' "),
                Arguments.of("income","High",0,"SELECT COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT COUNT(id) FROM click_log WHERE date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT DISTINCT date(impression_log.date) AS uniq_date, COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' GROUP BY uniq_date"),
                Arguments.of("date","2015-01-01,2015-01-10",1, "SELECT DISTINCT date(date) AS uniq_date, COUNT(id) FROM click_log WHERE uniq_date BETWEEN '2015-01-01' AND '2015-01-10' GROUP BY uniq_date")
        );
    }

    @Test
    @DisplayName("Clicks - Filter Age Groups")
    void clickFilterAgesTest() {
        qry = new QueryBuilder("Clicks", "income", "High",0, "day");
        assertEquals(qry.getQuery("age groups"), "SELECT DISTINCT age AS uniq_age, COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON impression_log.user_id = click_log.user_id WHERE impression_log.income = 'High' GROUP BY uniq_age");
        qry = new QueryBuilder("Clicks", "date", "2015-01-01,2015-01-10",0, "day");
        assertEquals(qry.getQuery("age groups"), "SELECT DISTINCT age AS uniq_age, COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON impression_log.user_id = click_log.user_id WHERE click_log.date BETWEEN '2015-01-01' AND '2015-01-10' GROUP BY uniq_age");

    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("uniqCorrectQueries")
    @DisplayName("Unique Users - Filter")
    void uniqFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Unique Users", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream uniqCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' "),
                Arguments.of("income", "High",0,"SELECT COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT COUNT(DISTINCT id) FROM click_log WHERE date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT DISTINCT date(impression_log.date) AS uniq_date, COUNT(DISTINCT click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' GROUP BY uniq_date"),
                Arguments.of("date","2015-01-01,2015-01-10",1, "SELECT DISTINCT date(date) AS uniq_date, COUNT(DISTINCT id) FROM click_log WHERE uniq_date BETWEEN '2015-01-01' AND '2015-01-10' GROUP BY uniq_date")
        );
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("bouncesCorrectQueries")
    @DisplayName("Bounces - Filter")
    void bounceFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Bounces", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
   }

    private static Stream bouncesCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44'  AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),
                Arguments.of("income", "High",0,"SELECT COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.income = 'High'  AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),
                Arguments.of("context","Shopping",0, "SELECT COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.context = 'Shopping'  AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE date BETWEEN '2015-01-01' AND '2015-01-10' AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),
                Arguments.of("gender","Male",0, "SELECT COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.gender = 'Male'  AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30)"),
                Arguments.of("age","35-44",1,"SELECT DISTINCT date(impression_log.date) AS uniq_date, COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44'  AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30) GROUP BY uniq_date"),
                Arguments.of("date","2015-01-01,2015-01-10",1, "SELECT DISTINCT date(impression_log.date) AS uniq_date, COUNT(server_log.id) FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE uniq_date BETWEEN '2015-01-01' AND '2015-01-10' AND (viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30) GROUP BY uniq_date")
        );
    }

    @Test
    @DisplayName("Bounce Rate - Filter")
    void bounceRateFilterTest() {
        qry = new QueryBuilder("Bounces", "age", "25-34",0, "day");
        assertEquals(qry.getQuery("bounce rate"), "SELECT (CAST (srv_count AS float)) / (CAST (click_count AS float)) FROM (SELECT COUNT(server_log.id) as srv_count FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 AND impression_log.age = '25-34' ), (SELECT COUNT(click_log.id) as click_count FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '25-34' ) ");
        qry = new QueryBuilder("Bounces", "income", "High",0, "day");
        assertEquals(qry.getQuery("bounce rate"), "SELECT (CAST (srv_count AS float)) / (CAST (click_count AS float)) FROM (SELECT COUNT(server_log.id) as srv_count FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 AND impression_log.income = 'High' ), (SELECT COUNT(click_log.id) as click_count FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.income = 'High' ) ");
        qry = new QueryBuilder("Bounces", "date", "2015-01-01,2015=01-10",0, "day");
        assertEquals(qry.getQuery("bounce rate"), "SELECT (CAST (srv_count AS float)) / (CAST (click_count AS float)) FROM (SELECT COUNT(server_log.id) as srv_count FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE viewed = '1' OR (JULIANDAY(exit_date) - JULIANDAY(entry_date)) * 60 * 60 * 24 < 30 AND impression_log.date BETWEEN '2015-01-01' AND '2015=01-10' ), (SELECT COUNT(click_log.id) as click_count FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.date BETWEEN '2015-01-01' AND '2015=01-10' ) ");
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("totalCostCorrectQueries")
    @DisplayName("Total Cost - Filter")
    void totalCostFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Total Cost", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream totalCostCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' "),
                Arguments.of("income", "High",0,"SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM impression_log WHERE date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT impression_tbl.uniq_date, impr_sum+click_sum FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log WHERE age = '35-44' GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(click_log.date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date "),
                Arguments.of("date","2015-01-01,2015-01-10",1, "SELECT impression_tbl.uniq_date, impr_sum+click_sum FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date WHERE impression_tbl.uniq_date BETWEEN '2015-01-01' AND '2015-01-10' ")
        );
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("CPACorrectQueries")
    @DisplayName("Cost-Per-Acquisition - Filter")
    void CPAFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Cost-Per-Acquisition", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream CPACorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44",0,"SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log WHERE impression_log.age = '35-44' "),
                Arguments.of("income", "High",0,"SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log WHERE date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT (ROUND(SUM(impression_cost)+(SELECT SUM(click_cost) FROM click_log ),2)) / (SELECT COUNT(id) FROM server_log WHERE conversion = 1 ) FROM impression_log WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT impression_tbl.uniq_date, impr_sum+click_sum / conv FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log WHERE age = '35-44' GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(click_log.date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date INNER JOIN (SELECT distinct date(entry_date) AS uniq_date, COUNT(server_log.id) AS conv FROM server_log INNER JOIN impression_log ON server_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' AND conversion = 1 GROUP BY uniq_date) AS conv_tbl ON conv_tbl.uniq_date = click_tbl.uniq_date "),
                Arguments.of("date","2015-01-01,2015-01-10",1,"SELECT impression_tbl.uniq_date, impr_sum+click_sum / conv FROM (SELECT distinct date(date) AS uniq_date, SUM(impression_cost) AS impr_sum FROM impression_log GROUP BY uniq_date) AS impression_tbl INNER JOIN (SELECT distinct date(date) AS uniq_date, SUM(click_cost) AS click_sum FROM click_log GROUP BY uniq_date) AS click_tbl ON impression_tbl.uniq_date = click_tbl.uniq_date INNER JOIN (SELECT distinct date(entry_date) AS uniq_date, COUNT(id) AS conv FROM server_log WHERE conversion = 1 GROUP BY uniq_date) AS conv_tbl ON conv_tbl.uniq_date = click_tbl.uniq_date WHERE impression_tbl.uniq_date BETWEEN '2015-01-01' AND '2015-01-10' ")
        );
    }

    @ParameterizedTest(name = "{index} => Filter: {0} , Value: {1}, Grouped: {2}")
    @MethodSource("CPCCorrectQueries")
    @DisplayName("Cost-Per-Click - Filter")
    void CPCFilterTest(String filter , String filterVal, int grouped, String query) {
        qry = new QueryBuilder("Cost-Per-Click", filter, filterVal,grouped, "day");
        assertEquals(qry.getQuery(), query);
    }

    private static Stream CPCCorrectQueries() {
        return Stream.of(
                Arguments.of("age","35-44", 0, "SELECT SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' "),
                Arguments.of("income", "High",0,"SELECT SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.income = 'High' "),
                Arguments.of("context","Shopping",0, "SELECT SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.context = 'Shopping' "),
                Arguments.of("date","2015-01-01,2015-01-10",0, "SELECT SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.date BETWEEN '2015-01-01' AND '2015-01-10' "),
                Arguments.of("gender","Male",0, "SELECT SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.gender = 'Male' "),
                Arguments.of("age","35-44",1,"SELECT DISTINCT date(impression_log.date) AS uniq_date, SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON click_log.user_id = impression_log.user_id WHERE impression_log.age = '35-44' GROUP BY uniq_date"),
                Arguments.of("date","2015-01-01,2015-01-10",1,"SELECT distinct date(impression_log.date) AS uniq_date, SUM(click_cost) / COUNT(click_log.id) FROM click_log INNER JOIN impression_log ON impression_log.user_id = click_log.user_id WHERE uniq_date BETWEEN '2015-01-01' AND '2015-01-10' GROUP BY uniq_date")
        );
    }
}
