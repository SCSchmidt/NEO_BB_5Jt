# README

Dieser Ordner enthält den Analysecode und die veröffentlichbaren Daten der Dissertation von Sophie C. Schmidt 
"Zwischen West und Ost, Nord und Süd: Brandenburg im 5. Jahrtausend v. Chr. als Interaktionsraum für Gruppen unterschiedlicher Subsistenz"

Hier wird in etwa der Struktur Ben Marwicks Paket {rrtools} (*reproducible research tools*) gefolgt: Auf der ersten Ebene befinden sich zwei Ordner, 
*R_functions* und *analysis*. Eine csv-Tabelle auf dieser Ebene hält die dependencies vor und ein kleines R-Skript *setup_dependencies.R* detailliert eine Möglichkeit 
zur automatischen Nachinstallation der notwendigen Pakete.
Unter *R_functions* befinden sich kleinere R-Scripte, die mehrmals im Verlauf der 
Analyse eingesetzt wurden. 

Der Ordner *py_code* enthält unter *QGIS* 
  - den Abfragecode für die Umkreisanalysen / *site catchment*-Analyse in Python (ein Derivat von <https://github.com/PyQGISCologne/PyQGIS_Site_Catchment>, MIT Lizenz) und 
  - die notwendigen Shapefiles unter *geodata*, Herausgeber: Landesvermessung und Geobasisinformation Brandenburg (LGB)
      - BB_BÜK300_Grundkarte, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?url=https://geoportal.brandenburg.de/gs-json/xml?fileid=f916fd97-f1e4-4516-a95c-7e9af9f98521> 
      - landwirtschaftliches_ertragspotential, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?view=gdibb&url=https%3A%2F%2Fgeoportal.brandenburg.de%2Fgs-json%2Fxml%3Ffileid%3Dbfafc655-9fa0-4b42-9c9b-43d00342e7ca> 
      - Mittelmßst_landwirtsch_Standortkart_MMK100, steht als *open data* zur Verfügung unter <https://geoportal.brandenburg.de/detailansichtdienst/render?url=https://geoportal.brandenburg.de/gs-json/xml?fileid=f916fd97-f1e4-4516-a95c-7e9af9f98521> 
      - schwarzerden_bb_admin3 ist eine Verschneidung der Schwarzerdenkartierung des Landesamts für Umwelt Brandenburg (<https://geobroker.geobasis-bb.de/gbss.php?MODE=GetProductInformation&PRODUCTID=BB07A209-E253-413D-B285-7B14671585DF>) und der Karten von Natural Earth Data (<https://www.naturalearthdata.com/>)
      - seen_bb_admin3 ist eine Verschneidung der Natural Earth Data mit der *open data* Kartierung von Seeflächen des Landesamts für Umwelt Brandenburg (<https://geoportal.brandenburg.de/detailansichtdienst/render?view=gdibb&url=https://geoportal.brandenburg.de/gs-json/xml?fileid=D9C4E283-00C3-42A2-9F1F-15BFD6A40B55>)
      - SGDB_PTR_arbeitsgebiet_epsg25833 ist eine in das EPSG 25833 transformierte und auf das Arbeitsgebiet zugeschnittene Karte der *Soil Georgraphic Database of Europe* der Euroäischen Kommission (<https://esdac.jrc.ec.europa.eu/content/european-soil-database-v20-vector-and-attribute-data> , siehe auch Panagos Panos. The European soil database (2006) GEO: connexion, 5 (7), pp. 32-33.)

Unter *analysis* liegen die Ordner *data*, *figures* und *paper*:

- *data* beherbergt 
  - alle Datensätze zur Keramik als csv-Daten im Ordner *raw_data*
  - die Geodaten im Ordner *geodata*: 
      - csv-Tabellen mit X- und Y-Koordinaten im *EPSG 25833* für die Lokalisierung von Fundstellen. Die Fundpunktkoordinaten wurden auf zwei Nachkommastellen reduziert, wodurch leichte Abweichungen entstehen könnten, sollte die *site catchment* - Analyse nachvollzogen werden. 
      - Stichprobenpunkte mit X- und Y-Koordinaten im *EPSG 25833* für die Umweltanalyse, 
      - sowie die Ergebnisse der Abfrage von den Umkreisanalysen / *site catchment* nach Radius getrennt (s. o.)
  - der Ordner *derived_data* beinhaltet Daten, die während der statistischen Analyse erstellt wurden und nicht jedesmal neu generiert werden sollten, wenn zu einem späteren Zeitpunkt weiter gearbeitet wurde

- im Ordner *figures* (derzeit leer) werden Abbildungen abgelegt, die während der Analyse durch den Code entstehen. Sie sind unterteilt in die Ordner:
  - *Gef-Form*: Abbildungen zur Analyse der Gefäßformen
  - *Magerung*: Abbildungen zur Analyse der Machart der Gefäße
  - *Muster*: Abbildungen zur Analyse der Verzierungen der Gefäße
  - *Standortanalyse*: Abbildungen zur Analyse der Standortmerkmale der Fundstellen
  
- im Ordner *paper* befinden sich die R-Skripte für die Ausführung der Analyse im Unterordner  *scripts*. 

Die Skripte sind nach vier Analyseschritten benannt und durchnummeriert, 01 ist dabei stets die Datenaufbereitung:

- keramik_machart: Skripte, die die Machart untersuchen
- keramik_verz: Skripte, die die Verzierungsanalyse der Keramik beinhalten
- keramik_gefform: Skripte, die die Gefäßform analysieren
- standortanalyse: Analyse der Lageeigenschaften: 1-17 innerhalb Brandenburgs, 20-22 die übergreifende Analyse

- 14C-daten erstellt Abbildungen für die Datierungen
- keramik_zwischen_fundstellen_vgl erstellt Abbildungen, welche die Ähnlichkeitsberechnungen von Fundstellen zu Fundstellen (fst-fst) aufgrund unterschiedlicher Merkmale an der Keramik miteinander vergleicht

In der von Marwick vorgeschlagenen Struktur würde im Ordner *paper* der Text des Artikels abgelegt werden. 
Dies wurde im Zuge der Abgabe der Dissertation nicht getan, um diesen nicht vorzuveröffentlichen.

Das Paket enthält nur Analyse-Code und tabellarische Daten, keine Bilder, da diese 
- a) z. T. an anderer Stelle veröffentlicht werden und 
- b) nicht alle für eine Veröffentlichung im Internet lizenziert sind.


## Die Daten stehen als CC-BY 4.0 zur Verfügung, der Code unter der MIT-Lizenz 
Um Benachrichtigung wird gebeten, sollte das bereitgestellte Material weitere Verwendung finden.
