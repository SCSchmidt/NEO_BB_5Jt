# README

Dieser Ordner enthält den Analysecode und die veröffentlichbaren Daten der Dissertation von Sophie C. Schmidt zum 5. Jt. v. Chr. im Bundesland Brandenburg

Hier wird in etwa der Struktur Ben Marwicks Paket {rrtools} (*reproducible research tools*) gefolgt: Auf der ersten Ebene befinden sich zwei Ordner, 
*R_functions* und *analysis*. Unter *R_functions* befinden sich kleinere R-Scripte, die mehrmals im Verlauf der Analyse eingesetzt wurden. 
Unter *analysis* liegen die Ordner *data*, *figures* und *paper*:

- *data* beherbergt 
  - alle Datensätze zur Keramik als csv-Daten im Ordner *raw_data*
  - die Geodaten im Ordner *geodata*: shape-files (TODO umformen!!) für die Lokalisierung von Fundstellen und Stichprobenpunkte für die Umweltanalyse, sowie die Ergebnisse der Abfrage von den Umkreisanalysen nach Radius getrennt
  - der Ordner *derived_data* beinhaltet Daten, die während der Analyse erstellt wurden und nicht jedesmal neu generiert werden sollten, wenn zu einem späteren Zeitpunkt weiter gearbeitet wurde

- im Ordner *figures* liegen Abbildungen, die während der Analyse durch den Code entstanden. Sie sind unterteilt in die Ordner:
  - *Gef-Form*: Abbildungen zur Analyse der Gefäßformen
  - *Magerung*: Abbildungen zur Analyse der Machart der Gefäße
  - *Muster*: Abbildungen zur Analyse der Verzierungen der Gefäße
  - *Standortanalyse*: Abbildungen zur Analyse der Standortmerkmale der Fundstellen
  
- im Ordner *paper* befinden sich die Dateien für die Ausführung der Analyse. In der von Marwick vorgeschlagenen Struktur würde hier auch der Text des Artikels abgelegt werden. Dies wurde im Zuge der Abgabe der Dissertation nicht getan, um diese nicht vorzuveröffentlichen. 

Das Paket enthält nur Analyse-Code und tabellarische Daten, keine Bilder, da diese a) z. T. an anderer Stelle veröffentlicht werden und b) nicht alle für eine Veröffentlichung im Internet lizenziert sind.

