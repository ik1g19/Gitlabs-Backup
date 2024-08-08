SELECT country.countriesAndTerritories AS "country name",
       round((sum(virus.deaths) * 1.0 / sum(virus.cases)) * 100, 2) AS "% cases of deaths"
FROM Virus
INNER JOIN Country ON virus.geoId = country.geoId
GROUP BY country.countriesAndTerritories, country.popData2018
ORDER BY "% cases of deaths" DESC
LIMIT 10;