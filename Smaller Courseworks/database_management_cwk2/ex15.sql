SELECT virus.dateRep AS date,
       virus.cases AS "number of cases"
FROM Virus INNER JOIN date ON virus.dateRep = date.dateRep
WHERE virus.geoId = 'UK'
ORDER BY date.year, date.month, date.day ASC;