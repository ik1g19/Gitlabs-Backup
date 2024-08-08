package SQLBuilder;

/*
 * Build SQL statements
 */

public class SQLBuilder {

    /* Produce select statement */
    public String selectStatement(String[] cols, String[] tbls, String whereClause, String extraParameters){
        String columns = String.join(", ", cols);
        String tables = String.join(", ", tbls);

        if(whereClause.isBlank())
            return "SELECT "+columns+" FROM "+tables+" "+extraParameters;
        else
            return "SELECT "+columns+" FROM "+tables+" "+whereClause+" "+extraParameters;
    }

    /* Produce where clause */
    public String whereClause(String[] parameters){
        return "WHERE "+String.join(" AND ", parameters);
    }

    /* Produce inner join */
    public String innerJoin(String tbl1, String tbl2, String tbl1Val, String tbl2Val){
        return tbl1+" INNER JOIN "+tbl2+" ON "+tbl1Val+" = "+tbl2Val;
    }

    /* Filter by date range */
    public String filter(String[] tbls, String[] tblDate, String startDate, String endDate, String[] columns, String extraParameters){
        return this.selectStatement(columns, tbls, whereClause(new String[]{tblDate[0]+" BETWEEN '"+startDate+"'", "'"+endDate+"'"}), extraParameters);
    }

    /* Filter by single field */
    public String filter(String[] tbls, String filter, String filterTbl, String parameter, String[] columns, String extraParameters){
        return this.selectStatement(columns, tbls, whereClause(new String[]{filterTbl+"."+filter+" = "+parameter}), extraParameters);
    }

}
