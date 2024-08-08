.mode csv
.output data.csv

SELECT date.dateRep,
       sum(virus.deaths) OVER (
           ORDER BY date.year, date.month, date.day
           ROWS BETWEEN
               UNBOUNDED PRECEDING
               AND CURRENT ROW
           ) AS 'cumulative UK deaths'
FROM Date
INNER JOIN Virus ON date.dateRep = virus.dateRep AND virus.geoId = 'UK'
ORDER BY date.year, date.month, date.day ASC;

.quit