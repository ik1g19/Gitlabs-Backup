SELECT country.countriesAndTerritories AS "country name"
FROM Virus
INNER JOIN country ON virus.geoId = country.geoId
INNER JOIN date ON virus.dateRep = date.dateRep
GROUP BY country.countriesAndTerritories
ORDER BY sum(virus.deaths) DESC
LIMIT 10;