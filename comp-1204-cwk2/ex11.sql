CREATE TABLE Date (
dateRep TEXT NOT NULL PRIMARY KEY, day INTEGER, month INTEGER, year INTEGER
);

CREATE TABLE Country (
geoId TEXT NOT NULL PRIMARY KEY, countriesAndTerritories TEXT, countryterritoryCode TEXT,
continentExp TEXT, popData2018 INTEGER
);

CREATE TABLE [Virus] (
dateRep TEXT, geoId TEXT, cases TEXT, deaths INTEGER,
PRIMARY KEY(dateRep, geoId),
CONSTRAINT fk_date
	FOREIGN KEY (dateRep)
	REFERENCES date(dateRep),
CONSTRAINT fk_country_id
	FOREIGN KEY (geoId)
	REFERENCES country(geoId)
);

CREATE INDEX dateIndex ON Date(dateRep);
CREATE INDEX dateLocation ON Virus(dateRep, geoId);
CREATE INDEX locationName ON Country(countriesAndTerritories);
CREATE INDEX locationId ON Country(geoId);