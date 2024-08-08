#!/bin/bash

sqlite3 coronavirus.db
.output countries.txt

SELECT country.countriesAndTerritories AS "country name"
FROM Virus
INNER JOIN country ON virus.geoId = country.geoId
INNER JOIN date ON virus.dateRep = date.dateRep
GROUP BY country.countriesAndTerritories
ORDER BY sum(virus.deaths) DESC
LIMIT 10;

.mode csv
count = 1

while IFS= read -r line; do
	.output country$count.csv

	SELECT date.dateRep,
       sum(virus.deaths) OVER (
           ORDER BY date.year, date.month, date.day
           ROWS BETWEEN
               UNBOUNDED PRECEDING
               AND CURRENT ROW
           ) AS 'cumulative deaths'
	FROM Date
	INNER JOIN Virus ON date.dateRep = virus.dateRep AND virus.geoId = '$line'
	ORDER BY date.year, date.month, date.day ASC;
done < countries.txt