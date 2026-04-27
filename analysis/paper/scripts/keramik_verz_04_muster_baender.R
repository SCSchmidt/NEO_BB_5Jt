### Keramikanalyse: Verzierungen -- Muster: waagerechte und senkrechte Bänder

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)

load(file = "./analysis/data/derived_data/verzierungen.RData")

load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2

safe_colorblind_palette13 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888","black")



kult_per_kr <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "band") & str_detect(Muster_neu_gr, "senkrecht")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "band") & str_detect(Muster_neu_gr, "senkrecht")) |>
  group_by(kreislabel, kultur_gr, fundort) |> summarize(count = n_distinct(gefäßnummer))


vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup()

verzierungen |>
  filter(str_detect(Muster_neu_gr, "band") & str_detect(Muster_neu_gr, "senkrecht")) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Muster_neu_gr) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Muster_neu_gr) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = Muster_neu_gr,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  
  labs(title = "Senkrechte Bänder",
       x = "Fundort",
       y = "Prozent an Gefäßen mit senkrechten Verzierungen",
       fill = "",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  scale_fill_manual(breaks = c("senkrechtes Band",                # ist alles tolower??
                               "senkrechtes band auf hals",
                               "senkrechtes band auf hals und schulter" ,
                               "senkrechtes band auf hals, schulter, bauch",
                               "senkrechtes band auf schulter",
                               "senkrechtes band auf spitze des winkels zu auf schulter",
                               "senkrechtes band mit begleitstiche auf schulter" ,
                               "senkrechtes band auf schulter und bauch" ,
                               "senkrechtes band auf spitze des winkels zu auf schulter und bauch",
                               "senkrechtes band auf bauch",
                               "senkrechtes band auf bauch und fuß" ,
                               "senkrechtes Band auf hohem Fuß"      
  ),
  labels =  c("senkrechtes Band",
              "senkrechtes Band auf Hals",
              "senkrechtes Band auf Hals und Schulter" ,
              "senkrechtes Band auf Hals, Schulter, Bauch",
              "senkrechtes Band auf Schulter",
              "senkrechtes Band auf Spitze des Winkels zu \nauf Schulter",
              "senkrechtes Band mit Begleitstiche auf Schulter" ,
              "senkrechtes Band auf Schulter und Bauch" ,
              "senkrechtes Band auf Spitze des Winkels zu \nauf Schulter und Bauch",
              "senkrechtes Band auf Bauch",
              "senkrechtes Band auf Bauch \nund Fuß" ,
              "senkrechtes Band auf hohem Fuß"    ),
  values = safe_colorblind_palette13
  )+
  coord_flip()+
  theme_bw(base_size = 11)+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = kult_per_kr$count)+
  theme(text = element_text(size = 11),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 10))+
  guides(fill = guide_legend(nrow = 4)) 



ggsave("./analysis/figures/Muster/2024-10-23_senkrechte_Bänder_Kultur--Kreis2.jpg", height = 18, width = 25, units = "cm")  


### waagerechte Bänder und ihre Lage

verzierungen$Muster_neu <- tolower(verzierungen$Muster_neu)
verzierungen$Muster_neu_gr <- as.factor(tolower(verzierungen$Muster_neu_gr))


verzierungen_mRverz <- verzierungen |> 
  filter(str_detect(Muster_neu_gr, "randband|halsband") )
# alle Gefäße mit Hals oder Randband!


gef_oRverz <- gef_umbruch |>
  filter(erh_r > 0) |>
  filter(erh_s > 0) |>
  filter(!(gefäßnummer %in% verzierungen_mRverz$gefäßnummer)) |>
  mutate(Muster_neu_gr = "kein Rand- oder Halsband") |>
  mutate(ort_k = NA) |>
  select(gefäßnummer, Muster_neu_gr, kultur_gr, kreislabel, fundort, ort_k, kultur2)

verz_flächig <- verzierungen |>
  filter(muster == "flächige füllung") |>
  filter(ausr.zu.rand == 0) |>
  mutate(Muster_neu_gr = paste("flächige Füllung mit waagerechten Reihen auf", ort_k) ) |>
  mutate(Muster_neu_gr = case_when(
    str_detect(Muster_neu_gr, "NA") ~ "flächige Füllung mit waagerechten Reihen",
    str_detect(Muster_neu_gr,  "flächige Füllung mit waagerechten Reihen auf unter dem rand, hals" ) ~ "flächige Füllung mit waagerechten Reihen auf hals",
    TRUE ~ Muster_neu_gr
  )) |>
  select(gefäßnummer, Muster_neu_gr, kultur_gr, kreislabel, fundort, ort_k, kultur2)

verzierungen_wB <- verzierungen   |>
  select(gefäßnummer, Muster_neu_gr, kultur_gr, kreislabel, fundort, ort_k, kultur2) |>
  rbind(gef_oRverz) |>
  rbind(verz_flächig) 
#  rbind(gef_oSverz) |>
#  rbind(gef_oBverz)

verzierungen_wB$Muster_neu_gr <- tolower(verzierungen_wB$Muster_neu_gr)


reihenfolge_gefaessform <-  tolower(c("kein Rand- oder Halsband", "Randband", "Halsband", 
                                      "flächige Füllung mit waagerechten Reihen auf hals" , "Band im Hals-Schulter-Umbruch", 
                                      "Band auf Hals oder Schulter", "flächige Füllung mit waagerechten Reihen auf hals und schulter", 
                                      "Band direkt unterhalb des Hals-Schulter-Umbruchs", "Band auf Schulter", 
                                      "flächige Füllung mit waagerechten Reihen auf schulter"   , 
                                      "Band direkt oberhalb des Bauchumbruchs", 
                                      "flächige Füllung mit waagerechten Reihen auf schulter oder bauch" , 
                                      "Band auf Bauchumbruch" , "Band direkt unterhalb des Bauchumbruchs", "Band auf Bauch",  
                                      "flächige Füllung mit waagerechten Reihen auf bauch"     , "Band auf Bauch und Fuß", 
                                      "Band direkt oberhalb des Bodens", 
                                      "flächige Füllung mit waagerechten Reihen auf hals, schulter, bauch", 
                                      "flächige Füllung mit waagerechten Reihen"  ) )


###  waagerechte Bänder auf Hals bis Schulter plotten

fo_per_kr <- verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "NA") |>
  filter(str_detect(ort_k, "hals") | is.na(ort_k) ) |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "NA") |>
  filter(str_detect(ort_k, "hals") | is.na(ort_k) ) |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  group_by(fundort, kreislabel, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))


vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup() |>
  mutate(count = gsub("NA", "1", count)) #k.A. warum für Hohenfinow ein NA entsteht. Einziger Fall, dort ist Rand erhalten, aber kein Halsband.


safe_colorblind_palette12 <- c("grey", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC",  "#AA4499") 


safe_colorblind_palette16 <- c("grey", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                               "#44AA99", "#999933", "grey50" , "#882255", "#661100", "#6699CC",  "#AA4499", "black","lightgrey") 


safe_colorblind_palette20 <- c("grey", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288","yellow", "lightgreen", "lightblue", "pink",
                               "#44AA99", "#999933", "grey50" , "#882255", "#661100", "#6699CC",  "#AA4499", "black","lightgrey") 


verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "NA") |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  filter(str_detect(ort_k, "hals") | is.na(ort_k) ) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Muster_neu_gr, ort_k) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Muster_neu_gr, ort_k) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot(aes(y = prop,
             x = fundort) ) +
  geom_bar(aes(fill = Muster_neu_gr),
           position = "fill", stat = "identity") +
  labs(title = "Waagerechte Bänder und Reihen auf dem Hals",
       x = "",
       y = "Prozent der verzierten Gefäße mit erhaltenem Hals o. Schulter",
       fill = "",
       caption = paste0("Anzahl Gefäße n = ", sum(gef_per_fo$count), "\n Gefäße mehrfach genannt, wenn Hals und Schulter erhalten") ) +
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,50,100))+
  geom_text(data = vgl_gef_per_fo , aes(x = fundort, y = 1, 
                                        label  = paste0(count)  ), 
            size = 2,
            hjust = 1, 
            angle = 0, 
            colour = "black",
            position = position_fill(vjust = .9) )+
  scale_fill_manual(values = safe_colorblind_palette12, 
                    breaks = c("kein rand- oder halsband", "randband", "halsband","flächige füllung mit waagerechten reihen auf hals" , "band im hals-schulter-umbruch", "band auf hals oder schulter", "flächige füllung mit waagerechten reihen auf hals und schulter", "flächige füllung mit waagerechten reihen auf hals, schulter, bauch", "flächige füllung mit waagerechten reihen" ),
                    labels = c("kein Rand-/ Halsband", "Randband", "Halsband","flächig: waagerechte Reihen auf Hals" , "Band im Hals-Schulter-Umbruch", "Band auf Hals oder Schulter", "flächig: waagerechte Reihen auf Hals und Schulter", "flächig: waagerechte Reihen auf Hals, Schulter, Bauch", "flächig: waagerechten Reihe, Verortung unsicher" )  ) +
  coord_flip() +
  theme_bw() +
  facet_grid(kreislabel ~ kultur_gr, drop = TRUE, scales = "free_y") +
  force_panelsizes(rows = fo_per_kr$count)+
  theme(text = element_text(size = 10),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 3)) +
  theme(strip.text.y = element_text(angle = 0, size = 8),
        legend.key.size = unit(0.25, "cm"))

ggsave("./analysis/figures/Muster/2025-09-25_waagerechteBänder_Hals2.png", dpi = 300, width = 21, height = 24, units = "cm")

###  waagerechte Bänder auf Schulter bis Boden plotten

verzierungen_wB$ort_k <- tolower(verzierungen_wB$ort_k)

save(verzierungen_wB, file = "./analysis/data/derived_data/vezierungen_wB.RData")

fo_per_kr <- verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "NA") |>
  filter(Muster_neu_gr != "halsband") |>
  filter(!str_detect(ort_k, "rand")) |>
  filter(!is.na(ort_k)) |>
  filter(ort_k != "hals") |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "halsband") |>
  filter(!str_detect(ort_k, "rand")) |>
  filter(!is.na(ort_k)) |>
  filter(ort_k != "hals") |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  group_by(fundort, kreislabel, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))



vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup()


safe_colorblind_palette11 <- c( "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                                "#44AA99", "#999933", "#882255", "#661100", "#6699CC",  "#AA4499") 
verzierungen_wB   |>
  filter(!is.na(Muster_neu_gr)) |>
  filter(Muster_neu_gr != "NA") |>
  filter(Muster_neu_gr != "halsband") |>
  mutate(Muster_neu_gr = factor(Muster_neu_gr, levels = reihenfolge_gefaessform) ) |>
  filter(Muster_neu_gr != "NA") |>
  filter(!str_detect(ort_k, "rand")) |>
  filter(!is.na(ort_k)) |>
  filter(ort_k != "hals") |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Muster_neu_gr, ort_k) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Muster_neu_gr, ort_k) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot(aes(y = prop,
             x = fundort) ) +
  geom_bar(aes(fill = Muster_neu_gr),
           position = "fill", stat = "identity") +
  labs(title = "Waagerechte Bänder und Reihen auf dem Gefäßkörper",
       x = "",
       y = "Prozent der verzierten Gefäßkörper",
       fill = "",
       caption = paste0("Anzahl Gefäße n = ", sum(gef_per_fo$count), "\n Gefäße mehrfach genannt, wenn mehrere Gefäßteile erhalten \nLabel gibt dargestellte Anzahl von verzierten Gefäße am Fundort") ) +
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, y = 1, 
                label  = paste0(count)  ), 
            size = 2,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9) )+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,50,100))+
  scale_fill_manual(values = safe_colorblind_palette16, 
                    breaks = tolower(c("Band im Hals-Schulter-Umbruch", "Band auf Hals oder Schulter", "flächige Füllung mit waagerechten Reihen auf hals und schulter", "Band direkt unterhalb des Hals-Schulter-Umbruchs", "Band auf Schulter", "flächige Füllung mit waagerechten Reihen auf schulter"   , "Band direkt oberhalb des Bauchumbruchs", "flächige Füllung mit waagerechten Reihen auf schulter oder bauch" , "Band auf Bauchumbruch" , "Band direkt unterhalb des Bauchumbruchs", "Band auf Bauch",    "flächige Füllung mit waagerechten Reihen auf bauch"     , "Band auf Bauch und Fuß", "Band direkt oberhalb des Bodens", "flächige Füllung mit waagerechten Reihen auf hals, schulter, bauch", "flächige Füllung mit waagerechten Reihen"  ) ),
                    labels = c("Band im Hals-Schulter-Umbruch", "Band auf Hals oder Schulter", "flächig: waagerechte Reihen \nauf Hals und Schulter", "Band direkt unterhalb des\nHals-Schulter-Umbruchs", "Band auf Schulter", "flächig: waagerechte Reihen \nauf Schulter"   , "Band direkt oberhalb des \nBauchumbruchs", "flächig: waagerechte Reihen \nauf Schulter oder Bauch" , "Band auf Bauchumbruch" , "Band direkt unterhalb des \nBauchumbruchs", "Band auf Bauch",    "flächig: waagerechte Reihen \nauf Bauch"     , "Band auf Bauch und Fuß", "Band direkt oberhalb des \nBodens", "flächig: auf Hals, Schulter, \nBauch", "flächig: waagerechte Reihe\n(Ort unklar)"  )   ) +
  coord_flip() +
  theme_bw(base_size = 11) +
  facet_grid(kreislabel ~ kultur_gr, drop = TRUE, scales = "free_y") +
  force_panelsizes(rows = fo_per_kr$count)+
  theme(text = element_text(size = 10),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 6)) +
  theme(strip.text.y = element_text(angle = 0, size = 8),
        legend.key.size = unit(0.5, "cm"))

ggsave("./analysis/figures/Muster/2025-09-25_waagerechteBänder_Gefäßkörper2.png",  dpi = 300, width = 20, height = 26, units = "cm")
