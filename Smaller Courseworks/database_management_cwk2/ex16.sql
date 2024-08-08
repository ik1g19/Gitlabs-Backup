SELECT country.continentExp AS continent,
       virus.dateRep AS date,
       sum(virus.cases) AS "number of cases",
       sum(virus.deaths) AS "number of deaths"
FROM Virus
INNER JOIN country ON virus.geoId = country.geoId
INNER JOIN date ON virus.dateRep = date.dateRep
GROUP BY country.continentExp, virus.dateRep
ORDER BY date.year, date.month, date.day ASC;