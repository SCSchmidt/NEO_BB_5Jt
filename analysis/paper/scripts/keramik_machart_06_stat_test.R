## Keramik Machart: Testen aller Variablen

library(dplyr)

source("./R_functions/get_p-adj_chiperm.R")

load(file = "./analysis/data/derived_data/scherben.RData")

## Oberfläche

load(file = "./analysis/data/derived_data/s_ofl.RData")

s_ofl2 <- s_ofl |>
  ungroup()

s_ofl2_verz <- s_ofl2 |>
  filter(verz_n == "verz") |>
  select(gefäßnummer, ofl, kultur2)|>
  rename(ofl_verz = ofl )

s_ofl2_unverz <- s_ofl2 |>
  filter(verz_n == "unverz") |>
  select(gefäßnummer, ofl, kultur2)|>
  rename(ofl_unverz = ofl )

## Magerung
source("analysis/paper/scripts/keramik_machart_03_magerung.R")


s_mag <- s_mag |>
  ungroup() 

mag_m_unverz <- s_mag |>
  filter(verz_n == "unverz") |>
  select(gefäßnummer, magerungsmenge, kultur2) |>
  rename(unv_magerungsmenge = magerungsmenge )

verz_magerungsmenge <- s_mag |>
  filter(verz_n == "verz") |>
  select(gefäßnummer, magerungsmenge, kultur2)|>
  rename(verz_magerungsmenge = magerungsmenge )

mag_gr_verz <- s_mag |>
  filter(verz_n == "verz") |>
  select(gefäßnummer, magerungsgr, kultur2) |>
  rename(verz_mag_gr = magerungsgr )

mag_gr_unverz <- s_mag |>
  filter(verz_n == "unverz") |>
  select(gefäßnummer, magerungsgr, kultur2) |>
  rename(unverz_mag_gr = magerungsgr )

s_mag_art <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(magerungsart != "anorganisch und evtl. organisch") |>
  filter(magerungsart != "organisch") |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, kultur2, magerungsart) |> 
  group_by(gefäßnummer, fundort, kultur_gr, kultur2, kreislabel) |>  # now required with changes to dplyr::count()
  ungroup()

magerungsart <- s_mag_art |>
  ungroup() |>
  select(gefäßnummer, magerungsart, kultur2)


load("./analysis/data/derived_data/scherben_syscz.RData")

k_sys_cz <- scherben_syscz |>
  filter(as.integer(bruchpotential) < 7) |>
  filter(!is.na(sys_cz_gr)) |>
  filter(sys_cz_gr != "NA") |>
  filter(!str_detect(sys_czerniak, "/")) |> # Unsicherheiten raus
  filter(!str_detect(sys_czerniak, "[?]")) |> # Unsicherheiten raus |>
  distinct(gefäßnummer, kultur_gr, sys_cz_gr, kultur2)  |>
  ungroup()

load(file = "./analysis/data/derived_data/scherben_gef_magerung.RData")

magerungsel_gr <- scherben_gef_magerung |>
  filter(magerungsel_gr != "NA") |>
  filter(magerungsel_gr != "organik?") |> # zu selten und nur da wo eh organik
  filter(kultur_gr != "FBG") |> # zwei 0
  select(magerungsel_gr, gefäßnummer, kultur2) |>
  unique()|>
  mutate(magerungsel_gr = as.character(magerungsel_gr)) |>
  unique()|>
  mutate(kultur2 = as.character(kultur2))


## Bruchfarbe

source("analysis/paper/scripts/keramik_machart_04_bruch.R")

s_bruchfarbe <- scherben_bf |>
  select(gefäßnummer, bruchfarbe_gr, kultur2) |>
  unique() # hatt ich das vergessen?

## Wandungsstärke

source("analysis/paper/scripts/keramik_machart_05_wandung.R")

### Zusammenfassung von allen und Zuweisung, welche Eigenschaften ordinal und welche nominal sind

chi_machart_ls <- list(sys_cz_gr = k_sys_cz, 
                       magerungsel_gr = magerungsel_gr,
                       bruchfarbe_gr = s_bruchfarbe,
                       magerungsart = magerungsart
)



wilcox_machart_ls <- list(ofl_unverz = s_ofl2_unverz, 
                          ofl_verz = s_ofl2_verz, 
                          unv_magerungsmenge = mag_m_unverz, 
                          verz_magerungsmenge = verz_magerungsmenge,  
                          unverz_mag_gr = mag_gr_unverz, 
                          verz_mag_gr = mag_gr_verz,
                          maxdicke = scherben_d_max,
                          mindicke = scherben_d_min)


#### TESTEN

source("./R_functions/get_p-adj_chiperm.R")


## CHI schleife über liste legen: alle Parameter werden für alle einzelnen Kulturen gegeneinander getestet

ls <- list()

kult <- "kultur2"

kulturen <- unique(s_bruchfarbe$kultur2)

vars <- c("sys_cz_gr",
          "magerungsel_gr",
          "bruchfarbe_gr",
          "magerungsart")



for (l in c(1:length(vars)) ) { 
  
  res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "P.value","variable", "method"))
  
  res_df$k <- as.character(res_df$k)
  res_df$k1 <- as.character(res_df$k1)
  res_df$k2 <- as.character(res_df$k2)
  res_df$n_note <- as.character(res_df$n_note)
  res_df$p.value <- as.numeric(res_df$p.value)
  res_df$variable <- as.character(res_df$variable)
  res_df$method <- as.character(res_df$method)
  
  
  varname <- paste(vars[l])
  
  df <- chi_machart_ls[[varname]]
  
  data <- data.frame(kultur = df[["kultur2"]], variable = df[[varname]], id = df[["gefäßnummer"]]) # select rel cols
  
  data$variable <- tolower(data$variable)
  
  data <-  data |> filter(variable != "NA") |>
    filter(!is.na(variable))
  
  
  for (i in 1:(length(kulturen)-1 ) ) {
    
    k1 <- kulturen[i] 
    
    for (j in 2:(length(kulturen) ) ) {
      
      k2 <- kulturen[j]
      
      data2 <- data |> 
        group_by(id) |>
        filter(kultur %in% c(k1, k2)  ) |>         # filter for two cultures
        filter(!is.na(variable)) |>
        filter(variable != "NA") |>
        unique() # dopplungen pro gefäß vermeiden
      
      
      # check if there are enough infos: 
      
      if (nrow(data2 |> dplyr::filter(kultur == k1)) < 2) {
        print(paste("Datensatz für ", k1, " unter 2 in ", varname) )
      } 
      else if (nrow(data2 |> dplyr::filter(kultur == k2)) < 2) {
        print( paste("Datensatz für ", k2, " unter 2 in ", varname))
      }  
      else if (length(unique(data2$variable)) < 2) {
        print(paste("Variable ", varname, " unter 2 in ", k1, k2))
      }
      else if (length(unique(data2$kultur)) < 2) {
        print(paste("gleiche Kultur", k1) ) } #
      
      else if (paste0(k2, " - ", k1) %in% res_df$k) {
        print(paste("Paar", k1, k2, "schon besprochen") ) } #  
      else {
        
        # alle Voraussetzungen erfüllt:
        
        print(paste("Untersuche Paar", k1, k2, "zur Variable", varname)    )   
        
        
        res <- wPerm::perm.ind.test(data2,       # chi quadrat
                                    type = "raw",    # df mit var1 var2 und freq
                                    var.names = c("Var1", "Var2"),
                                    R = 9999) 
        res$var <- varname
        res$K1 <- k1
        res$K2 <- k2
        res$k <- paste0(k1, " - ", k2)
        res$K1n <- n_distinct(data2 |> dplyr::filter(kultur == k1) |> select(id) ) # n soll gefäßanzahl entsprechen
        res$K2n <- n_distinct(data2 |> dplyr::filter(kultur == k2) |> select(id) )
        res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
        
        res_zusammen <- data.frame("k" = res$k, 
                                   "k1" = res$K1, 
                                   "k2" = res$K2, 
                                   "n_note" = res$n_note, 
                                   "p.value" = res$p.value, 
                                   "P.value"  = res$P.value,
                                   "variable" = varname,
                                   "method" = "Chi-Quadrat, Permutationstest" )  
        
        
        
        res_df <-rbind(res_df, res_zusammen)
        
      }
      
    }
  }
  ls[[varname]] <- res_df
  
}



chi_machart_perm <- adjust_p(ls) # für multiples testen

### WILCOXON

source("./R_functions/get_p_wilcoxon.R")

# wie oben: alle Parameter werden bei allen Kulturen gegeneinander getestet

ls <- list()

kult <- "kultur2"

kulturen <- unique(s_bruchfarbe$kultur2)

vars <- c("ofl_unverz",
          "ofl_verz",
          "unv_magerungsmenge",
          "verz_magerungsmenge", 
          "unverz_mag_gr",
          "verz_mag_gr",
          "maxdicke",
          "mindicke")

ls <- list()

p.adjust.method <- "none"

for (i in (1:length(vars)) ) {    
  
  varname <- vars[i]
  
  df <- wilcox_machart_ls[[varname]]
  
  data <- data.frame(df[[varname]], df[[kult]]) # to be able to filter for NAs later
  
  colnames(data) <- c("var1", "var2") # makes reuse of old function easier
  
  data$var1 <- as.numeric(data$var1)
  
  data <- data |>
    dplyr::filter(!is.na(var1)) # remove NAs 
  
  d <-  pairwise.wilcox.test(data$var1, data$var2, 
                             p.adjust.method =  p.adjust.method) # calc wilcoxon
  res <- reshape2::melt(d$p.value)                                                 # aus p-value matrix Distanzenliste  
  res$p.value <- round(res$value, 3)      # p-value gerundet auf 3-Nachkommastellen
  
  res <- res |> dplyr::filter(!is.na(p.value))
  
  colnames(res) <- c("k1", "k2", "value", "p.value")      # colnames ändern
  
  res$k <- paste0(res$k1, " - ", res$k2)
  
  anzahlen <- as.data.frame(table(data$var2) ) # for n_note
  
  colnames(anzahlen) <- c("k1", "nK1")
  
  res <- merge(res, anzahlen, by.x = "k1", by.y = "k1", all.x = TRUE )
  
  colnames(anzahlen) <- c("k2", "nK2")
  
  res <-  merge(res, anzahlen, by.x = "k2", by.y = "k2", all.x = TRUE )
  
  res$n_note <- paste(res$k1, "n = ", res$nK1, "\n", res$k2, "n = ", res$nK2)
  
  res$variable <- paste(varname)
  
  res <- res |> select(k, k1, k2, n_note, p.value, variable) # compatible with the chi square function
  
  res$method <- "paarweiser Wilcoxon"
  
  ls[[varname]] <- res       
  
}

wilcox_machart_p <- adjust_p(ls)



## erst beide p-Wert-Sammlungen zusammenführen
df_list <- list(wilcox_machart_p, chi_machart_perm)

p_values <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)


# labels vereinheitlichen

p_values$label <- paste0(p_values$variable, "\n", p_values$method) 

p_values$n_note <- gsub(" \n ", "\n", p_values$n_note)

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")


p_values$label <- factor(p_values$label,
                         ordered = TRUE,
                         levels = c( "ofl_unverz\npaarweiser Wilcoxon" ,
                                     "ofl_verz\npaarweiser Wilcoxon" ,
                                     "unverz_mag_gr\npaarweiser Wilcoxon" ,
                                     "verz_mag_gr\npaarweiser Wilcoxon",
                                     "unv_magerungsmenge\npaarweiser Wilcoxon" ,
                                     "verz_magerungsmenge\npaarweiser Wilcoxon",
                                     "magerungsart\nChi-Quadrat, Permutationstest",
                                     "sys_cz_gr\nChi-Quadrat, Permutationstest" ,
                                     "magerungsel_gr\nChi-Quadrat, Permutationstest",
                                     "bruchfarbe_gr\nChi-Quadrat, Permutationstest",    
                                     "maxdicke\npaarweiser Wilcoxon",
                                     "mindicke\npaarweiser Wilcoxon"  ) )


p_values <- p_values |>
  mutate(k = case_when(
    k == "Rös - SRK" ~ "SRK - Rös" ,
    k == "Rös - SBK" ~ "SBK - Rös" ,
    k == "Guhrau - Rös" ~ "Rös - Guhrau" ,
    k == "Guhrau - SBK" ~ "SBK - Guhrau" ,
    k == "FBG - SBK" ~ "SBK - FBG" ,
    k == "FBG - Rös" ~ "Rös - FBG" ,
    k == "FBG - Guhrau" ~ "Guhrau - FBG" ,
    k == "BKK - Rös" ~ "Rös - BKK",
    k == "Rössen - SBK" ~ "SBK - Rössen",
    TRUE ~ k))

save(p_values, file = "./analysis/data/derived_data/alle_p_machart_2026-04-23.RData")

### plotten der verzierten

library(ggplot2)
library(ggh4x)
library(stringr)

load(file = "./analysis/data/derived_data/alle_p_machart_2026-04-23.RData") 

p_values |>
  filter(!(str_detect(label, "unv"))) |>
  ggplot(aes(x = label,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white", size = 1 )+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  scale_x_discrete(labels =  c(#"Oberflächengestaltung \nunverzierte Gef. \nChi-Quadrat,\n Permutationstest",
    "Oberflächengestaltung \npaarweiser\n Wilcoxon",
    # "Magerungsgröße\n\ unverz. Gef.\npaarweiser\n Wilcoxon" ,
    "Magerungsgröße\n verz. Gef.\npaarweiser\n Wilcoxon",
    # "Magerungsmenge\n unverz. Gef.\npaarweiser\n Wilcoxon" ,
    "Magerungsmenge\n verz. Gef.\npaarweiser\n Wilcoxon",
    "Magerungsart\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",
    "Magerungsgruppen \n alle Gef.\nChi-Quadrat,\n  Permutationstest" ,
    "Magerungselemente\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",
    "Bruchfarbe\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",    
    "max. Wandstärke\n alle Gef.\n paarweiser\n Wilcoxon",
    "min. Wandstärke\n alle Gef.\n paarweiser\n Wilcoxon") )+
  scale_fill_gradientn(colours = farben ,
                       values =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign.", "0.2","0.8", "1"),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "white")+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Gruppenunterschiede in der Machart",
       caption = "n variiert je nach Erhaltungszustand und Belegung der Kreuztabellen und ist als label angegeben \n p-Werte mit dem Chi-Quadrat Permutationstest mit 9999 Wiederholungen erhalten, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995) .")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("./analysis/figures/Magerung/2026-04-23_verzierte_p-machart_perm.png", dpi = 300, width = 40, height = 25, units = "cm")


#### plotten der unverzierten

p_values |>
  filter(str_detect(label, "unv")) |>
  ggplot(aes(x = label,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white", size = 1 )+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  scale_x_discrete(labels =  c("Oberflächengestaltung \nunverzierte Gef. \nChi-Quadrat,\n Permutationstest",
                               #    "Oberflächengestaltung \nverz. Gef.\nChi-Quadrat,\n Permutationstest",
                               "Magerungsgröße\n\ unverz. Gef.\npaarweiser\n Wilcoxon" ,
                               #  "Magerungsgröße\n verz. Gef.\npaarweiser\n Wilcoxon",
                               "Magerungsmenge\n unverz. Gef.\npaarweiser\n Wilcoxon" #,
                               #  "Magerungsmenge\n verz. Gef.\npaarweiser\n Wilcoxon",
                               #  "Magerungsart\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",
                               #   "Magerungsgruppen \n alle Gef.\nChi-Quadrat,\n  Permutationstest" ,
                               #  "Magerungselemente\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",
                               # "Bruchfarbe\n alle Gef.\n Chi-Quadrat,\n  Permutationstest",    
                               #  "max. Wandungdsdicke\n alle Gef.\n paarweiser\n Wilcoxon",
                               #  "min. Wandungsdicke\n alle Gef.\n paarweiser\n Wilcoxon"
  ) )+
  scale_fill_gradientn(colours = farben ,
                       values =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign.", "0.2","0.8", "1"),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "white")+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Gruppenunterschiede in der Machart",
       caption = "n variiert je nach Erhaltungszustand und Belegung der Kreuztabellen und ist als label angegeben \n p-Werte mit dem Chi-Quadrat Permutationstest mit 9999 Wiederholungen erhalten, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995) .")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))


ggsave("./analysis/figures/Magerung/2026-04-14_unverzierte_p-machart_perm.png", dpi = 300, width = 21, height = 21, units = "cm")

