SELECT country.countriesAndTerritories AS country,
       round((sum(virus.cases) * 1.0 / country.popData2018) * 100, 2) AS "% cases of population",
       round((sum(virus.deaths) * 1.0 / country.popData2018) * 100, 2) AS "% cases of deaths"
FROM Virus
INNER JOIN Country ON virus.geoId = country.geoId
GROUP BY country.countriesAndTerritories, country.popData2018;
