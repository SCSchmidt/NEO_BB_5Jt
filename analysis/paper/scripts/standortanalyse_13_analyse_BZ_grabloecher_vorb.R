## Bodenschätzung nach Bohrlochdaten: Abfrage. Achtung, dauert! 

## 1. Daten einladen
## Kreis ziehen um jeden Punkt und schauen, welche Bohrlöcher mit Bodenzahlen darin liegen

## (bereits heruntergeladen) mit und ohne Schichten zusammenführen

library(sf) # simple features packages for handling vector GIS data
library(dplyr)

bs_o_sch <- read_sf(dsn = "./analysis/data/geodata/grabloecher_BS_ohneSchichten.shp")

bs_m_sch <- read_sf(dsn = "./analysis/data/geodata/grabloecher_BS_mitSchichten.shp")

bs_o_sch_w <- bs_o_sch |> select(wertzahlen)

bs_m_sch_w <- bs_m_sch |> select(wertzahlen)

bs <- rbind(bs_o_sch_w, bs_m_sch_w)

load(file = "./analysis/data/derived_data/sites.RData")

sites <- st_as_sf(sites, coords = c("X","Y"), crs = 25833 )


# für jeden Radius puffern und welche Punkte sind drin im Polygon
## Achtung dauert!

# Puffer um sites -> polygone mit sites Nr / Name 

radius <- c(250, 500, 1000, 2000)

ls_buffer <- list()

for (i in radius) {
  
  sites_buffer <- st_buffer(sites, i)
  
  sites_buffer$radius <- i
  
  ls_buffer[[as.character(i)]] <- sites_buffer
  
}

# Punkte in Polygonen -> filter auf max-Wert
## Achtung, dauert.


bs_ohne_NA <- bs |> filter(!is.na(wertzahlen))

fst_bs <- data.frame(FST = as.character(),
                     FID = as.character(),
                     BS = as.numeric(),
                     r = as.numeric() )

for (d in names(ls_buffer) ) {
  a <-  ls_buffer[[d]]
  for (fst in a$FST) {
    fst_poly <- a |> filter(FST == fst)
    bs_punkte <- st_filter(bs_ohne_NA, fst_poly)
    if (length(bs_punkte) > 0 ){
      max_bs <- max(bs_punkte$wertzahlen)
      if (max_bs != -Inf) {
        fst_o_poly <- st_drop_geometry(fst_poly)
        ergebnis <- data.frame(FST = fst_o_poly$FST,
                               FID = fst_o_poly$FID,
                               BS = max_bs, 
                               r = as.numeric(d) )
        
        fst_bs <- rbind(fst_bs,
                        ergebnis)
      }
    }
  }
}

save(fst_bs, file = "./analysis/data/derived_data/fst_bs_punktabfrage_maxwerte.RData")

