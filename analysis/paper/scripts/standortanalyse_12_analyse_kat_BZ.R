### Bodengüte: kategorisiertes Ertragspotential

## Daten: Ertragspotential / Ackerzahl in Radien

load(file = "./analysis/data/derived_data/sites.RData")


AZ5000 <- read.csv2("./analysis/data/geodata/standortanalyse_BB_2025-02-03/ertrag_bez.csv", sep = ",", dec = ".")

AZ1000 <- read.csv2("./analysis/data/geodata/standortanalyse_1000_BB/ertrag_bez.csv", sep = ",", dec = ".")

AZ2000 <- read.csv2("./analysis/data/geodata/standortanalyse_2000_BB/ertrag_bez.csv", sep = ",", dec = ".")

AZ250 <- read.csv2("./analysis/data/geodata/standortanalyse_250_BB/ertrag_bez.csv", sep = ",", dec = ".")

AZ500 <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/ertrag_bez.csv", sep = ",", dec = ".")


AZ5000$r  <- 5000

AZ1000$r <- 1000

AZ2000$r <- 2000

AZ250$r <- 250

AZ500$r <- 500

AZ_all <- rbind(AZ1000, AZ2000, AZ500, AZ250, AZ5000) #AZ5000 - 5000 dazu, falls nötig


AZ_all$id <- as.character(AZ_all$id)

standort_sites <- left_join(sites, AZ_all, by = c("FID" = "id") )


library(viridis)
library(ggplot2)
library(ggh4x)
library(stringr)

standort_sites <- standort_sites |>
  filter(!is.na(KREIS)) |>
  filter(kreislabel != "CB")


standort_sites <-standort_sites |>
  mutate(kultur2 = case_when(
    FST %in% 
      c("Frankfurt 108",
        "Wernsdorf 6",
        "Beeskow 5",
        "Kossenblatt 3",
        "Bad Freienwalde 8",
        "Blankenburg 26",
        "Blankenburg 8",
        "Brüssow 67",
        "Niederlandin 12/5 (13)",
        "Prenzlau 23",
        "Ribbeck 9/21 (9)",
        "Altfriesack 6", 
        "Altranft 6",
        "Treuenbrietzen 39",
        "Potsdam 58",
        "Bornim 8") ~ "Rös. Beil/Axt",
    TRUE ~ kultur2
  )) |>
  filter(FST != "Zeestow 14/4 (14)")

standort_sites$kultur2 <- factor(standort_sites$kultur2, ordered = TRUE,
                                 levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Guhrau", "Rös", "Rös. Beil/Axt"))


## Korrekturen von mehrperiodigen Fundstellen:
bredow_roes <- standort_sites |> filter(FID == "3007")

bredow_roes$FID <- "3007.2"
bredow_roes$kultur2 <- "Rös"

standort_sites <- rbind(standort_sites, bredow_roes)

standort_sites$kultur2[standort_sites$FST == "Potsdam 16"] <- "Rös"
standort_sites$FST[standort_sites$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <-standort_sites |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$FID <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

standort_sites <- rbind(standort_sites, potsdam_sbk)

#### plot 


fo_per_kr <- standort_sites |>   
  filter( !is.na(r)) |>
  group_by(kreislabel, kultur2) |>
  summarize(count = n_distinct(FST))

fo_per_kr <- st_drop_geometry(fo_per_kr) |> ungroup()

k_per_kr <- standort_sites |>    
  filter( !is.na(r)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

x <- standort_sites |>
  filter( !is.na(r)) |>
  group_by(kreislabel, obj, kultur2, r) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()

### plot

x$obj <- factor(x$obj, ordered = TRUE,
                levels = c("Bodenzahlen vorherrschend >50",
                           "Bodenzahlen überwiegend >50 und verbreitet 30 - 50",
                           "Bodenzahlen überwiegend 30 - 50 und verbreitet >50" ,
                           "Bodenzahlen vorherrschend 30 - 50" ,
                           "Bodenzahlen überwiegend 30 - 50 und verbreitet <30" ,
                           "Bodenzahlen überwiegend <30 und verbreitet 30 - 50" ,
                           "Bodenzahlen vorherrschend <30" ,
                           "überwiegend versiegelt und verbreitet Bodenzahlen 30 - 50",
                           "überwiegend versiegelt und verbreitet Bodenzahlen <30",
                           "Bodenzahlen überwiegend <30 und verbreitet versiegelt",
                           "Bodenzahlen überwiegend 30 -50 und verbreitet versiegelt") )



farbenAZ <- c("Bodenzahlen vorherrschend >50" = "#822d19",
              "Bodenzahlen überwiegend >50 und verbreitet 30 - 50" = "#af5d31",
              "Bodenzahlen überwiegend 30 - 50 und verbreitet >50" = "#e49d3b",
              "Bodenzahlen vorherrschend 30 - 50" = "#fee527",
              "Bodenzahlen überwiegend 30 - 50 und verbreitet <30" = "#b3de4a",
              "Bodenzahlen überwiegend <30 und verbreitet 30 - 50" = "#73ca4a",
              "Bodenzahlen vorherrschend <30" = "#37ad53",
              "überwiegend versiegelt und verbreitet Bodenzahlen 30 - 50" = "#f5759d" ,
              "überwiegend versiegelt und verbreitet Bodenzahlen <30" = "#c2559b",
              "Bodenzahlen überwiegend <30 und verbreitet versiegelt" = "#97b19d" ,
              "Bodenzahlen überwiegend 30 -50 und verbreitet versiegelt" = "#efd2aa" )

farbenAZ <- c("#822d19",
              "#af5d31",
              "#e49d3b",
              "#fee527",
              "#b3de4a",
              "#73ca4a",
              "#37ad53",
              "#f5759d" ,
              "#c2559b",
              "#97b19d" ,
              "#efd2aa" )


x |>
  ggplot(na.rm = TRUE)+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj),
           position = "fill", stat = "identity")+
  geom_text(data = fo_per_kr ,
            aes(x = kultur2,
                y = 1,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  labs(title = "Bezug zu Bodengüte",
       subtitle = "Anteile an Bodenzahl-Kategorien im Umkreis von 250 bis 5000 m",
       x = "",
       y = "% von Fläche im Umkreis",
       fill = "",
       caption = "Datengrundlage: Bodenschätzung \nQuelle: © LGB dl-by-de/2.0")+
  scale_fill_manual(values = farbenAZ,
                    na.value = "grey80") +
  coord_flip()+
  facet_grid(kreislabel ~ r, scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kr$count)+
  theme_bw()+
  theme(legend.position = "bottom" )+
  guides(fill=guide_legend(nrow=4))


ggsave("./analysis/figures/Standortanalyse/Bodenzahl_Kreis-Kultur_rad250-5000.png", dpi = 300, height = 40, width = 35, units = "cm")

