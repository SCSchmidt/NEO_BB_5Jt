### Machart Keramik: Magerung

## 1. Menge und Größe
## 2. Magerungsart
## 3. Sys Czerniak
## 4. Elemente

library(dplyr)
library(stringr)
library(ggplot2)
library(ggh4x)

load(file = "./analysis/data/derived_data/scherben.RData")

s_mag <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(magerungsmenge)) |>
  filter(!is.na(magerungsgr)) |>
  count(gefäßnummer, kreis, fundort, kultur_gr, magerungsmenge, magerungsgr, kultur2, verz_n) |>  # group by gefäßnummer vergessen! aber sieht genau gleich aus....
  group_by(gefäßnummer, fundort, kultur_gr, kreis, magerungsmenge, magerungsgr, kultur2, verz_n)



col  <- c(
  "GG" = "darkgrey",
  "Meso" =  "#117733",
  "FBG" =  "#61D04F",
  "LBK Siedl." = "#882255", 
  "LBK Dechsel" = "#CC6677",
  "SBK" = "black",
  "SRK" = "#CD0BBC",
  "Rössen" = "#332288",
  "BKK" = "darkviolet",
  "Guhrau" = "#DDCC77"
)

## 1. Magerungsmenge zu Größe

scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  group_by(gefäßnummer) |>          # now required with changes to dplyr::count()
  slice_max(as.numeric(magerungsgr)) |> #nimmt größte der Werte aus der Gruppierung (egal, müssten alle gleich sein, aber so wird ein Wert ausgewählt)
  slice_max(as.numeric(magerungsmenge)) |>
  filter(!is.na(magerungsgr),
         !is.na(magerungsmenge)) |> 
  ggplot()+
  geom_jitter(aes(x = magerungsgr,
                  y = magerungsmenge,
                  col = kultur2,
                  shape = verz_n,),
              size = 2,
              width = 0.3,
              height = 0.3)+
  labs(x = "Magerungsgröße",
       y = "Magerungsmenge",
       title = "Größe zu Menge der Magerungsbestandteile",
       col = "Kultur",
       caption = paste0("Funde reduziert auf sicher bestimmbare Brüche, n = ", length(unique(s_mag$gefäßnummer) ), "\nPunkte zittern um 0.3 um Überlappungen zu vermindern" ) )+
  scale_shape_manual(name = "",
                     breaks = c("unverz", "verz"),
                     labels = c("unverziert", "verziert"),
                     values = c(3,16))+
  scale_color_manual(breaks = c("SBK", "SRK","Rössen", "Guhrau", "BKK", "FBG"),
                     values = col )+
  theme_bw()+
  facet_grid(kreislabel ~ .) +
  theme(text = element_text(size = 11))+
  guides(fill=guide_legend(ncol =1))

ggsave("./analysis/figures/Magerung/2026-04-14_Magerungsgröße-Menge_Kreis-Kultur3.png", dpi = 300, height = 20, width = 16, units = "cm")


## Magerungsart

fo_per_kr <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

library(ggh4x)

scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, magerungsart) |>  # auszählen, gefnummer und magerungsart.
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel) |>   # now required with changes to dplyr::count()
  mutate(n = case_when( # wenn n vorher über 0 -> egal wie viele scherben pro gefäß -> 1
    n > 0 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(prop = prop.table(n)) |>
  ungroup() |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = magerungsart),
           position = "fill", stat = "identity")+
  scale_fill_manual(values = c( "orange", "grey", "lightblue", "darkgreen"),
                    labels = c("anorg.", "anorg. & evtl. organ.", "anorg. & organ.", "organ."))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100) )+
  geom_text(data = gef_per_fo  %>% slice_head(n=1), aes(x = fundort, y = 1, 
                                                        label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            position = position_fill(vjust = .9),
            col = "grey10" )  +
  labs(title = "Magerungsart",
       fill = "",
       y = "Prozent von bestimmbaren Gefäßeinheiten",
       x = "Fundort",
       caption = paste0("Funde reduziert auf n = ", length(unique(scherben$gefäßnummer[as.numeric(scherben$bruchpotential) < 7])), "\nLabel zeigen n pro Fundort") )+
  theme_bw(base_size = 11)+
  coord_flip() +
  facet_grid(kreislabel~kultur_gr, scales = "free") +
  force_panelsizes(rows = fo_per_kr$count)+
  theme(legend.position = "bottom")

ggsave("./analysis/figures/Magerung/2026-14_26_Magerungsart_Kreis-Kultur3.png", dpi = 300, height = 26, width = 21, units = "cm")


### Sys Czerniak


scherben_syscz <- scherben |>
  filter(as.integer(bruchpotential) < 7) |> ## scherben nur auf die gefiltert, die gutes bruchpotential haben!!!!
  filter(!is.na(sys_czerniak)) |>
  filter(!str_detect(sys_czerniak, "[/]")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus |>
  mutate(sys_cz_gr = case_when(
    sys_czerniak == "III B2" ~ "III B",
    sys_czerniak == "III C2" ~ "III C",
    sys_czerniak == "III D2" ~ "III D",
    str_detect(sys_czerniak, "IV A1|IVA A2") ~ "IV A",
    str_detect(sys_czerniak, "IV B1|IV B2") ~ "IV B",
    str_detect(sys_czerniak, "V A") ~ "V A",
    str_detect(sys_czerniak, "V C") ~ "V C",
    str_detect(sys_czerniak, "VI A|VI B") ~ "VI A-B",
    str_detect(sys_czerniak, "VI B2|VI C") ~ "VI C",
    str_detect(sys_czerniak, "VIII A|VIII B1") ~ "VIII A",
    str_detect(sys_czerniak, "VI E|VIII B2") ~ "VIII B",
    sys_czerniak == "VIII C2|VIII C" ~ "VIII C",
    str_detect(sys_czerniak, "VII A1|VII A2|VII A3") ~ "VII A",
    str_detect(sys_czerniak, "VII B") ~ "VII B",
    str_detect(sys_czerniak, "VII C") ~ "VII C",
    sys_czerniak == "VII D|VII D4" ~ "VII D",
    sys_czerniak == "VII A4|VII D3" ~ "VII A4-D3",
    str_detect(sys_czerniak, "VIII D") ~ "VIII D",
    str_detect(sys_czerniak, "VIII E|VIII E2") ~ "VIII E",
    str_detect(sys_czerniak, "VIII E3|VIII E4") ~ "VIII E3",
    str_detect(sys_czerniak, "VIII F") ~ "VIII F",
    sys_czerniak == "VIII D5|VIII D6" ~ "VIII D5-6",
    TRUE ~ sys_czerniak
  ))

save(scherben_syscz,  file = "./analysis/data/derived_data/scherben_syscz.RData")

### plot

library(ggh4x) # nach https://stackoverflow.com/questions/61601428/adjust-the-size-of-panels-plotted-through-ggplot-and-facet-grid

fo_per_kr <- scherben_syscz |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(sys_czerniak)) |>
  filter(!str_detect(sys_czerniak, "/")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus 
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <-  scherben_syscz |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(sys_czerniak)) |>
  filter(!str_detect(sys_czerniak, "/")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus 
  group_by(fundort, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))

scherben_syscz |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(sys_cz_gr)) |>
  filter(sys_cz_gr != "NA") |>
  filter(sys_cz_gr != "") |>
  filter(!str_detect(sys_czerniak, "/")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, sys_cz_gr) |>  
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, sys_cz_gr) |>          
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = sys_cz_gr),
           position = "fill", stat = "identity",
           na.rm = TRUE)+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100))+
  labs(x = "Fundort",
       y = "Prozent der bestimmbaren Gefäßeinheiten",
       title = "Magerungsgruppen",
       fill = "",
       caption = paste0("Funde reduziert auf gut erkennbare Brüche und sicher bestimmte Gruppen, n = ", sum(gef_per_fo$count) ) )+
  theme_bw(base_size = 11)+
  coord_flip() +
  theme(legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        legend.text =  element_text(size = 10),
        legend.title = element_text(size = 10)) +
  guides(fill=guide_legend(ncol = 1))+
  facet_grid(kreislabel~kultur_gr, scales = "free") +
  force_panelsizes(rows = fo_per_kr$count+2)


ggsave("./analysis/figures/Magerung/2026-04_14_sys_cz_gruppiert_Kreis-Kultur.png", dpi = 300, height =  20, width = 21, units = "cm")    


### Magerungselemente


library(stringr)
library(tidyr)

scherben_syscz$magerungsbestandteile_gr <- tolower(scherben_syscz$magerungsbestandteile_gr)
#scherben$magerungsbestandteile <-   str_replace_all(scherben$magerungsbestandteile, fixed(" "), "")

scherben_syscz <- scherben_syscz |>
  separate_wider_delim(cols = magerungsbestandteile_gr, delim = ",",
                       names = c("magerungsel_1", "magerungsel_2", "magerungsel_3", "magerungsel_4", "magerungsel_5", "magerungsel_6", "magerungsel_7"),
                       too_few = "align_start") # wenn nicht genug magerungselemente drin, um alle felder zu füllen


scherben_gef_magerung <- scherben_syscz |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(sys_czerniak)) |>
  filter(!str_detect(sys_czerniak, "/")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus |>
  distinct_at(vars(gefäßnummer, sys_czerniak), .keep_all = TRUE) |> 
  pivot_longer(c(magerungsel_1:magerungsel_7),
               names_to = "magerungselement_stellung",
               values_to = "magerungselement",
               values_drop_na = TRUE)

scherben_gef_magerung$magerungselement <- str_replace_all(scherben_gef_magerung$magerungselement, pattern = fixed(" "), replacement = "")

scherben_gef_magerung <- scherben_gef_magerung |>
  mutate(magerungsel_gr = case_when(
    magerungselement ==  "muschelo.stein?" ~ NA,
    magerungselement ==  "schamottoderocker?" ~ NA,
    magerungselement ==  "" ~ NA,
    magerungselement ==  "glimmer?" ~ "glimmer",
    magerungselement ==  "wenigglimmer" ~ "glimmer",
    magerungselement ==  "schwarzerstein" ~ "stein",
    magerungselement ==  "weißerstein" ~ "stein",
    magerungselement ==  "quarzit" ~ "stein",
    magerungselement ==  "roterstein" ~ "rosastein",
    magerungselement ==  "kalkstein?" ~ "stein",
    magerungselement ==  "kleinelücken(blasig?)" ~ "organik?",
    magerungselement ==  "kleinelücken?" ~ "organik?",
    magerungselement ==  "kleinelücken(blasig)" ~ "organik?",
    magerungselement ==  "kleinelücken" ~ "organik",
    magerungselement ==  "sand?" ~ "sand",
    magerungselement ==  "schamott?" ~ "schamott",
    magerungselement ==  "stein?" ~ "stein",
    TRUE ~ magerungselement
  ))



scherben_gef_magerung <- scherben_gef_magerung |>  mutate_if(is.character,as.factor)

save(scherben_gef_magerung, file = "./analysis/data/derived_data/scherben_gef_magerung.RData")

# Magerungselemente plotten: alle

library(ggh4x) # nach https://stackoverflow.com/questions/61601428/adjust-the-size-of-panels-plotted-through-ggplot-and-facet-grid

fo_per_kr <- scherben_gef_magerung |> group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- scherben_gef_magerung |>
  filter(magerungsel_gr != "NA") |>
  group_by(fundort, kultur_gr, kreislabel)|> summarize(count = n_distinct(gefäßnummer))

scherben_gef_magerung |>
  filter(magerungsel_gr != "NA") |>
  count(gefäßnummer, magerungsel_gr, fundort, kultur_gr, kreislabel) |>
  group_by(gefäßnummer, magerungsel_gr, fundort, kultur_gr, kreislabel) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = magerungsel_gr,
               y = prop),
           position = "fill", stat = "identity")+
  scale_fill_manual(name = "",
                    breaks = c("sand", "glimmer", "stein", "rosastein", "kiesel", "schamott", "organik", "organik?", "kalk"),
                    labels= c("Sand", "Glimmer", "Stein", "rosa Stein", "Kiesel", "Schamott", "Organik", "Organik?", "Kalk?"),
                    values = c("yellow", "lightblue", "grey", "pink", "mintcream", "brown", "green4", "green", "black" ))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100)) +
  geom_text(data = gef_per_fo  %>% slice_head(n=1), 
            aes(x = fundort, y = 1, 
                label  = paste0(count)  ), 
            size = 3,
            hjust = 1, 
            angle = 0, 
            position = position_fill(vjust = .95),
            col = "grey10" )  +
  labs(x = "",
       y = "Prozent der bestimmbaren Gefäßeinheiten",
       title = "Verteilung der Magerungselemente auf Region und Kultur",
       caption = paste0("Funde reduziert auf gut erkennbare Brüche, n = ", sum(gef_per_fo$count) ) )+
  coord_flip() +
  theme_bw()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free") +
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))+
  guides( fill = guide_legend(nrow = 2)) +
  force_panelsizes(rows = fo_per_kr$count) # Höhe der Panels dadurch festgelegt, dass Anzahl der Reihen festgelegt wird

ggsave("./analysis/figures/Magerung/2026-04-14_Magerungselemente_Kreis-Kultur2.png", dpi = 300, width = 16, height = 25, units = "cm")


## save magerung_gef

magerung_gef <- scherben_gef_magerung |>
  filter(!is.na(magerungsel_gr) ) |>
  filter(magerungsel_gr != "") |>
  filter(magerungsel_gr != "NA") |>
  mutate(magerungsel_gr = str_trim(magerungsel_gr, side = "left") ) |>
  pivot_wider(id_cols = fotonr, names_from = magerungsel_gr, values_from = "gefäßnummer", values_fn = list(gefäßnummer = length) ) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # verändere alle Spalten, die numeric sind und wende Funktion replace_na auf Spalte (.x) an 
  mutate(across(where(is.numeric),  ~ifelse(. > 0, 1, 0) ) ) # verändere alle Spalten, die numeric sind und wende ifelse auf werte (.) an. wenn über 0 -> 1, alles andere 0.


magerung_gef <- right_join(magerung_gef, scherben_gef_magerung[, c("gefäßnummer", "max_dicke", "magerungsgr", "magerungsart" ,"magerungsmenge", "sys_czerniak", "ofl", "erhaltung_ofl_außen", "fotonr")], by = join_by(fotonr))

magerung_gef <- magerung_gef |>
  mutate(ofl = case_when(
    str_detect(erhaltung_ofl_außen, "gut") ~ ofl,
    str_detect(erhaltung_ofl_außen, "leicht") ~ofl,
    str_detect(erhaltung_ofl_außen, "etwas") ~ofl,
    TRUE ~ NA  )) |>
  distinct() 


magerung_gef$magerungsart <- as.factor(magerung_gef$magerungsart)

save(magerung_gef, file = "./analysis/data/derived_data/magerung_gef.RData")
