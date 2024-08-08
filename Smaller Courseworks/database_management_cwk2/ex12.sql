INSERT INTO Date (dateRep, day, month, year)
SELECT DISTINCT dateRep, day, month, year FROM dataset;

INSERT INTO Country (geoId, countriesAndTerritories, countryterritoryCode, continentExp, popData2018)
SELECT DISTINCT geoId, countriesAndTerritories, countryterritoryCode, continentExp, popData2018 FROM dataset;

INSERT INTO Virus (dateRep, geoId, cases, deaths)
SELECT DISTINCT dateRep, geoId, cases, deaths FROM dataset;