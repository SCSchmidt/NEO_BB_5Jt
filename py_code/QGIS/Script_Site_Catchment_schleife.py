# Title: Site Catchment Analysis - A scripted PyQGIS solution
# Authors: Sophie Schmidt, Sara Schiesberg, Kai Vogl, Sandra Rung
# License: MIT (https://opensource.org/licenses/mit-license.php)
# Objective: Takes a point and a polygon layer. Returns a table giving percentages for polygon types around each point.  

# Import libraries 
from qgis.PyQt.QtCore import QVariant
from qgis.core import QgsVectorFileWriter
import numpy as np # für das List-Management?


# Bodenkarten und die genutztn Merkmale in Brandenburg:
## /geodata/BB_BÜK300_Grundkarte.shp ->  "hauptgru_1", "gruppe"

## /geodata/Mittelmaßst_landwirtsch_Standortkart_MMK100.shp  -> MMK geolog, mesorel, 'untertyp', 'leitbofo', 'gefuege', 'domlil', 'steinob', 'hangnei', 'nft', 'sft', 'hft', 'relpos1', 'boform1', 'grundwas', 'stauwas', 'raeumhet'

## /geodata/landwirtschaftliches_ertragspotential.shp -> ertrag_bez

## /geodata/seen_bb_admin3.shp -> über layer ist die Seen und Land - Fläche definiert

## /geodata/schwarzerden_bbadmin3.shp -> schwarzerden_bbadmin3 definiert Schwarzerde und nicht-Schwarzerde

# Bodenkarten und die genutztn Merkmale über Brandenburg hinaus:

## /geodata/SGDB_PTR_arbeitsgebiet_epsg25833.shp
### var =  ["AGLI1NNI","AGLI2NNI","AWC_SUB","AWC_TOP","BS_SUB","BS_TOP","DGH","EAWC_TOP","ERODI","FAO85FU","HG","OC_TOP","PARMADO","PARMASE","SLOPEDO","TXSRFDO","TXSRFSE","WRBFU","ZMAX","ZMIN"] # als liste, über die iteriert wird. wirklich genutzt: FAO85FU, parmado, TXSRFDO, OC_TOP, AGLI1NNI, AWC_TOP


## Punktkoordinaten für Abfragen

### alle Fst: /../analysis/data/geodata/Fst_Standortanalysen_2.shp #

## mesolithische und linearbandkeramische Fst von Maha Ismail-Weber übernommen

## für meso: /media/sophie/Volume/R/Diss/analysis/data/geodata/maha_meso_arbeitsgebiet.shp, ID = field_1

#(/media/sophie/Volume/Nextcloud/NEO/Daten/GeoDaten/nachtrag_Fst_SBK_Rös_Stettin.shp) ## Nachtrag von ein paar Fst um Stetin
#(/media/sophie/Volume/Nextcloud/NEO/Daten/GeoDaten/fst_shps/LBK-Dechseleinzelfunde_MIW.shp)
#(/media/sophie/Volume/Nextcloud/NEO/Daten/GeoDaten/fst_shps/LBK-Siedll_MIW.shp)







# Insert values
land =  '/media/sophie/Volume/Nextcloud/NEO/Daten/GeoDaten/LBGR/Mittelmaßst_landwirtsch_Standortkart_MMK100.shp'
samp = '/media/sophie/Volume/R/Diss/analysis/data/geodata/Fst_Standortanalysen_BB_LBK_Guhrau.shp'
samp_id = 'FID' #'fid'
radius = 2000 # this needs to be a number, therefore don't use ' ' !

output_path = '/media/sophie/Volume/R/Diss/analysis/data/geodata/standortanalyse_2000_BB/'


# Add point and polygon layers to QGIS
landuse = iface.addVectorLayer(land, 'landuse', 'ogr')
samples = iface.addVectorLayer(samp, 'sample', 'ogr')

# loop around several parameters in the land-shape
var =  ['srt', 'untertyp', 'leitbofo', 'gefuege', 'domlil', 'steinob', 'sft', 'hft','stauwas', 'raeumhet'] #liste der parameter 
for i in var:
    land_id = i
    output = output_path + i + ".csv" # output abhängig von input-variable
    # Create a new layer for results
    result = QgsVectorLayer('Point?crs=' + samples.crs().authid(), 'Result', 'memory')
    prov = result.dataProvider()
    prov.addAttributes([QgsField('id', QVariant.Int), QgsField('obj', QVariant.String), QgsField('area', QVariant.Double), QgsField('area%', QVariant.Double)])
    result.updateFields()
    fields = prov.fields()
    # Nested loop iterating for every point over all polygons, inspired by: https://www.researchgate.net/post/How-to-calculate-the-intersected-area-from-a-layer-with-overlapping-buffers-in-qgis , see answer by Detlev Neumann
    feats = []
    for sample in samples.getFeatures():
        puffer = sample.geometry().buffer(radius, 25) # buffer with 25 segments boundary
        for poly in landuse.getFeatures():
            part = puffer.intersection(poly.geometry())
            feat = QgsFeature(fields) 
            feat.setGeometry(sample.geometry())
            feat['id'] = sample[samp_id]
            feat ['obj'] = poly[land_id]
            feat['area'] = part.area()
            if feat ['area'] > 0:
                feat['area%'] = (part.area())/(puffer.area())
                feats.append(feat)
    # Add the feat
    prov.addFeatures(feats)
    QgsProject.instance().addMapLayer(result)
    # Save the results as .csv
    QgsVectorFileWriter.writeAsVectorFormat(result, output,'UTF-8',
    result.crs(), 'CSV')
