 
- Script_Site_Catchment_schleife.py ist der Abfragecode für die Umkreisanalysen / *site catchment*-Analyse in QPy -- Python für QGIS 
  - die grundlegende Funktion wird unter <https://archaeoinformatics.net/python-for-site-catchment-qgis/> erläutert
  - darum wurde eine weitere Schleife gelegt, die, wenn in einem *shape*-file mehrere Informationen zur Verfügung standen, diese gleichzeitig abfragen kann

- unter *geodata* liegen die notwendigen Shapefiles, Herausgeber: Landesvermessung und Geobasisinformation Brandenburg (LGB), copyright beim LGBR und LfU (siehe Projekt-README)
      - BB_BÜK300_Grundkarte, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?url=https://geoportal.brandenburg.de/gs-json/xml?fileid=f916fd97-f1e4-4516-a95c-7e9af9f98521> 
      - landwirtschaftliches_ertragspotential, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?view=gdibb&url=https%3A%2F%2Fgeoportal.brandenburg.de%2Fgs-json%2Fxml%3Ffileid%3Dbfafc655-9fa0-4b42-9c9b-43d00342e7ca> 
      - Mittelmßst_landwirtsch_Standortkart_MMK100, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?url=https://geoportal.brandenburg.de/gs-json/xml?fileid=f916fd97-f1e4-4516-a95c-7e9af9f98521> 
      - schwarzerden_bb_admin3 ist eine Verschneidung der Schwarzerdenkartierung des Landesamts für Umwelt Brandenburg (<https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=BB07A209-E253-413D-B285-7B14671585DF>) und der Karten von Natural Earth Data (<https://www.naturalearthdata.com/>)
      - seen_bb_admin3 ist eine Verschneidung der Kartierung von Seeflächen des Landesamts für Umwelt Brandenburg (<https://geoportal.brandenburg.de/detailansichtdienst/render?view=gdibb&url=https://geoportal.brandenburg.de/gs-json/xml?fileid=D9C4E283-00C3-42A2-9F1F-15BFD6A40B55>)
 
- nicht zur Verfügung gestellt: SGDB_PTR_arbeitsgebiet_epsg25833 ist eine in das EPSG 25833 transformierte und auf das Arbeitsgebiet zugeschnittene Karte der *Soil Georgraphic Database of Europe* der Euroäpischen Kommission (© European Soil Bureau Network und European Commission <https://esdac.jrc.ec.europa.eu/content/european-soil-database-v20-vector-and-attribute-data> , siehe auch Panagos Panos. The European soil database (2006) GEO: connexion, 5 (7), pp. 32-33.)

- Die Koordinaten der Fundstellen sind unter NEO_BB_5Jt/analysis/geodata abgelegt, allerdings auf nur zwei Nachkommastellen genau, weshalb es hier zu etwas anderen Abfragen kommen kann. Sie liegen dort nur als csv-Datei vor.
