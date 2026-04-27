### TESTEN

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)

load(file = "./analysis/data/derived_data/verzierungen.RData")

# Datenvorbereitung

## Techniken
verz_technik <- verzierungen |>
  select(gefäßnummer, technik_gr, kultur2) |>
  distinct(gefäßnummer, technik_gr, kultur2) |>
  mutate(kultur2 = as.character(kultur2)) |> 
  filter(technik_gr != "Einzelstich o. Rössener Doppelstich") |> # zu selten
  mutate(technik_gr = case_when(
    technik_gr == "geritzt" | technik_gr == "geschnitten" ~ "geritzt o. geschnitten", 
    technik_gr == "Fünffachstich" | technik_gr == "Vierfachstich" ~ "4-5fachstich",
    TRUE ~ technik_gr
  )) |>
  filter(technik_gr != "Inkrustation")  |> # erhaltungsbedingt
  filter(technik_gr != "Meißelstich, tremolierend") |> # zu selten
  filter(technik_gr != "plastisch verziert") # eher Grobkeramik -> weniger im Datensatz

## Elemente

verz_elem <- verzierungen |> 
  group_by(gefäßnummer, kultur2, element) |>
  select(gefäßnummer, kultur2, element) |>
  filter(element != "NA") |>
  filter(!is.na(element)) |>
  unique() |>
  ungroup() |>
  group_by(element) |>
  filter(n() > 1) |>
  ungroup()


## Bänder

verzierungen$Muster_neu_gr <- tolower(verzierungen$Muster_neu_gr)

verz_snk_b <- verzierungen |>
  mutate(s_b_bin = case_when(
    str_detect(Muster_neu_gr, "band") & str_detect(Muster_neu_gr, "senkrecht") ~ "senkrechtes band ja"
  )) |>
  select(s_b_bin, kultur2, gefäßnummer) |>
  unique() |>
  filter(!is.na(s_b_bin) )

verz_baender_s <- verzierungen |>
  mutate(s_baender = case_when(
    gefäßnummer %in% verz_snk_b$gefäßnummer ~ "senkrechtes band",
    !(gefäßnummer %in% verz_snk_b$gefäßnummer) ~ "kein senkrechtes band"
  )) |>
  select(s_baender, kultur2, gefäßnummer)

baender_s <- verz_baender_s

verz_baender_s <- verz_baender_s |>
  group_by(s_baender, kultur2) |>
  mutate(count = n_distinct(gefäßnummer)) |>
  select(-gefäßnummer) |>
  unique() 

verz_baender_s <- verz_baender_s |>
  pivot_wider(names_from = kultur2,
              values_from = count)

colnames(verz_baender_s)[1] <- "bandinfo"


verz_baender_w <- verzierungen |>
  mutate(w_bin = case_when(
    ausr.zu.rand == 0 ~ "waagerechtes Band ja"  )) |>
  select(w_bin, kultur2, gefäßnummer) |>
  unique() |>
  filter(!is.na(w_bin) )


verz_w_baender <- verzierungen |>
  mutate(w_baender = case_when(
    gefäßnummer %in% verz_baender_w$gefäßnummer ~ "waagerecht band",
    !(gefäßnummer %in% verz_baender_w$gefäßnummer) ~ "kein waagerecht band"
  )) |>
  select(w_baender, kultur2, gefäßnummer)

baender_w <- verz_w_baender

verz_baender_w <- verz_w_baender |>
  group_by(w_baender, kultur2) |>
  mutate(count = n_distinct(gefäßnummer)) |>
  select(-gefäßnummer) |>
  unique() 

verz_w_baender <- verz_baender_w |>
  pivot_wider(names_from = c(kultur2),
              values_from = count)

colnames(verz_w_baender)[1] <- "bandinfo"

verz_baender <-rbind(verz_baender_s, verz_w_baender)

verz_baender[is.na(verz_baender)] <- 0

verz_baender <- as.data.frame(verz_baender)

rownames(verz_baender) <- verz_baender$bandinfo

verz_baender <- verz_baender[,-1]

verz_baender_m <- as.matrix(verz_baender)

#verz_baender <- verz_baender |> select(-"NA")

save(verz_baender, file = "./analysis/data/derived_data/senkr_waager_baender_vgl.RData")

# für chi perm
colnames(baender_s) <- c("bandausr", "kultur2", "gefäßnummer")

colnames(baender_w) <- c("bandausr", "kultur2", "gefäßnummer")


verz_baenderausr <- rbind(baender_s, baender_w)

## Lage waagerechter Bänder

##waagerechte bänder statistisch

load("./analysis/data/derived_data/vezierungen_wB.RData")

verzierungen_wB <- verzierungen_wB |> filter(Muster_neu_gr %in% tolower(c("kein rand- oder halsband", "randband", "halsband","flächige füllung mit waagerechten reihen auf hals" , "band im hals-schulter-umbruch", "band auf hals oder schulter", "flächige füllung mit waagerechten reihen auf hals und schulter", "flächige füllung mit waagerechten reihen auf hals, schulter, bauch", "flächige füllung mit waagerechten reihen", "Band im Hals-Schulter-Umbruch", "Band auf Hals oder Schulter", "flächige Füllung mit waagerechten Reihen auf hals und schulter", "Band direkt unterhalb des Hals-Schulter-Umbruchs", "Band auf Schulter", "flächige Füllung mit waagerechten Reihen auf schulter"   , "Band direkt oberhalb des Bauchumbruchs", "flächige Füllung mit waagerechten Reihen auf schulter oder bauch" , "Band auf Bauchumbruch" , "Band direkt unterhalb des Bauchumbruchs", "Band auf Bauch",    "flächige Füllung mit waagerechten Reihen auf bauch"     , "Band auf Bauch und Fuß", "Band direkt oberhalb des Bodens", "flächige Füllung mit waagerechten Reihen auf hals, schulter, bauch", "flächige Füllung mit waagerechten Reihen"  ) ))

save(verzierungen_wB, file = "./analysis/data/derived_data/verzierungen_waagerechteBaender.RData")


### Winkelgrößen

### Gruppenunterschiede Winkelwert Vorb. für Wilcox

load( "./analysis/data/derived_data/verzierungen_ww")

verz_winkelbr <- verzierungen_ww |>
  select(gefäßnummer, kreislabel, kultur2, kultur_gr, winkelwert) |>
  filter(!is.na(winkelwert)) |>
  unique()

###  chi-quadrat_winkelart_SBK-SRK

load("./analysis/data/derived_data/verz_dreieckig.RData")

verz_dreieck_SBK <- verz_dreieckig |> filter(kultur2 == "SBK" | kultur2 == "SRK") |> 
  filter(gr_muster != "Y-Anhänge") # zu selten

verz_dreieck_SBK_n <- verz_dreieck_SBK |>
  group_by(kultur2, gr_muster) |>
  summarize(count = n_distinct(gefäßnummer)) |>
  ungroup()

verz_dreieck_SBK_n <- verz_dreieck_SBK_n |> pivot_wider(names_from = kultur2, values_from = count)

as.matrix(verz_dreieck_SBK_n[2:3])

chi_winkel <- chisq.test(as.matrix(verz_dreieck_SBK_n[2:3]))           #table(verz_dreieck_SBK$gr_muster, as.character(verz_dreieck_SBK$kultur2) ) )

winkel_dreieck <- data.frame(verh = "SBK - SRK", winkel.dreieck_p = chi_winkel$p.val)

library(rcompanion)

cramerV(as.matrix(verz_dreieck_SBK_n[2:3])) 


### muster

verz_muster_neu_gr <- verzierungen |>
  select(Muster_neu_gr, kultur2, gefäßnummer) |>
  unique() |>
  group_by(Muster_neu_gr) |>
  filter(n() > 1) |>
  filter(Muster_neu_gr != "NA")|>
  filter(!is.na(Muster_neu_gr)) |>
  ungroup()


verz_muster <- verzierungen |>
  select(muster, kultur2, gefäßnummer) |>
  unique() |>
  group_by(muster) |>
  filter(n() > 1) |>
  filter(muster != "NA")|>
  filter(!is.na(muster)) |>
  ungroup()


### testen


verz_tiefe <- verzierungen |>
  filter(!is.na(ausfuehrung)) |>
  select(gefäßnummer, ausfuehrung, kultur2) |>
  unique()

verz_gr <- verzierungen |>
  filter(!is.na(groeße)) |>
  select(gefäßnummer, groeße, kultur2) |>
  unique()

load( "./analysis/data/derived_data/verzierungen_waagerechteBaender.RData")


verzierungen_wB <- verzierungen_wB |>
  select(gefäßnummer, Muster_neu_gr, kultur2)


verz_dreieckig <- verz_dreieckig |>
  select(gefäßnummer, gr_muster, kultur2)

load("./analysis/data/derived_data/winkelb_breit.RData")

winkelb_breit <- winkelb_breit |>
  ungroup() |>
  select(gefäßnummer, Muster_neu_gr2, kultur2)

load( "analysis/data/derived_data/winkelb_schmal.RData")

winkelb_schmal <- winkelb_schmal |>
  ungroup() |>
  select(gefäßnummer, Muster_neu_gr2, kultur2)

winkelbaender <- rbind(winkelb_breit, winkelb_schmal)

load("analysis/data/derived_data/verz_rs.RData")

liste_verz_chi <- list(technik_gr = verz_technik, element = verz_elem, Muster_neu_gr = verzierungen_wB, bandausr = verz_baenderausr, 
                       gr_muster= verz_dreieckig, Muster_neu_gr2 = winkelbaender, muster.zusatz = verz_rs)





## schleife über liste legen Chi --> dauert, weil permutierend mit 9999 Wiederholungen

ls <- list()

kult <- "kultur2"

kulturen <- unique(verzierungen$kultur2)

vars <- c("technik_gr", "element", "Muster_neu_gr", "bandausr", "gr_muster", "Muster_neu_gr2", "muster.zusatz")


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
  
  df <- liste_verz_chi[[varname]]
  
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


source("./R_functions/get_p-adj_chiperm.R")

chi_verzierungen_3 <- adjust_p(ls) ## für mutiples testen


#### ordinale Variablen

wdh_elem <- verzierungen |>
  filter(kultur2 != "FBG" &
           gebrochen == 0) |>
  select(gefäßnummer, kultur2, wdhlng_elem_neu) |>
  unique() |> 
  mutate(wdhlng_elem_neu = as.numeric(wdhlng_elem_neu)) |>
  filter(!is.na(wdhlng_elem_neu)) |>
  group_by(kultur2) |>
  summarize(m = mean(wdhlng_elem_neu),
            sd = sd(wdhlng_elem_neu))

save(wdh_elem, file = "./analysis/data/derived_data/wiederholung_element.RData")

bandbr <- verzierungen |>
  filter(kultur2 != "FBG" &
           gebrochen == 0) |>
  select(gefäßnummer, kultur2, Bandbreite) |>
  unique() |> 
  mutate(Bandbreite = as.numeric(Bandbreite)) |>
  filter(!is.na(Bandbreite)) |>
  group_by(kultur2) |>
  summarize(m = mean(Bandbreite),
            sd = sd(Bandbreite))


save(bandbr, file = "./analysis/data/derived_data/Bandbreite.RData")



Wdh_Band <- verzierungen |>
  filter(kultur2 != "FBG" &
           gebrochen == 0) |>
  select(gefäßnummer, kultur2, Wdh_Band) |>
  unique() |> 
  mutate(Bandbreite = as.numeric(Wdh_Band)) |>
  filter(!is.na(Wdh_Band)) |>
  group_by(kultur2) |>
  summarize(m = mean(Wdh_Band),
            sd = sd(Wdh_Band))
save(Wdh_Band, file = "./analysis/data/derived_data/Wdh_Band.RData")

## Wilcoxon




df <- verzierungen |> 
  select(gefäßnummer, groeße, ausfuehrung, kultur2, Bandbreite, wdhlng_elem_neu, Wdh_Band) |>
  unique()

ls <- list()

vars <- c("groeße", "ausfuehrung", "Bandbreite", "wdhlng_elem_neu", "Wdh_Band")


for (i in (1:length(vars)) ) {    
  
  varname <- vars[i]
  
  data <- data.frame(df[[varname]], df[[kult]], id = df[["gefäßnummer"]]) # to be able to filter for NAs later
  
  colnames(data) <- c("var1", "var2", "gefnr") # makes reuse of old function easier
  
  
  data <- data |>
    group_by(gefnr, var1, var2) |>
    unique() |>
    ungroup()
  
  
  data$var1 <- as.numeric(data$var1)
  
  data <- data |>
    dplyr::filter(!is.na(var1)) # remove NAs 
  
  d <-  pairwise.wilcox.test(data$var1, data$var2, 
                             p.adjust.method = "none") # calc wilcoxon
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

verz_wilcox <- adjust_p(ls)  


## alle p-Werte plotten

df_list <- list(chi_verzierungen_3, verz_wilcox) #verzierungen_wilcox 

p_values <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

p_values <- p_values |>
  filter(variable != "Muster_neu_gr2") # hier sind die Muster im Detail aufgenommen: alle stat signifikant



### zusammenführen der P-WErte

p_values$label <- paste0(p_values$variable, "\n", p_values$method) 

p_values$n_note <- gsub(" \n ", "\n", p_values$n_note)

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")


p_values$label <- factor(p_values$label,
                         ordered = TRUE,
                         levels = c("technik_gr\nChi-Quadrat, Permutationstest", 
                                    "groeße\npaarweiser Wilcoxon" , 
                                    "ausfuehrung\npaarweiser Wilcoxon",  
                                    "element\nChi-Quadrat, Permutationstest"  ,
                                    "wdhlng_elem_neu\npaarweiser Wilcoxon",  
                                    "bandausr\nChi-Quadrat, Permutationstest" ,
                                    "Bandbreite\npaarweiser Wilcoxon" ,
                                    "Wdh_Band\npaarweiser Wilcoxon" ,
                                    "Muster_neu_gr\nChi-Quadrat, Permutationstest" ,
                                    "gr_muster\nChi-Quadrat, Permutationstest",
                                    "muster.zusatz\nChi-Quadrat, Permutationstest") )



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
    TRUE ~ k))

save(p_values, file = "./analysis/data/derived_data/alle_p_verzierung.RData")


#p_values$label

p_values |>
  ggplot(aes(x = label,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white", linewidth = 1 )+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  scale_x_discrete(labels =  c("Technik (gr.)\nChi-Quadrat, \nPermutationstest", 
                               "Stichgroeße\npaarweiser \nWilcoxon" , 
                               "Stichausfuehrung\npaarweiser \nWilcoxon",  
                               "Element\nChi-Quadrat, \nPermutationstest"  ,
                               "Anz. Wdhlng. \nd. Elements\npaarweiser\n Wilcoxon",
                               "waager.-senkr. Bänder \nChi-Quadrat, \nPermutationstest",
                               "Bandbreite\npaarweiser Wilcoxon" ,
                               "Anz. Wdhlng. \nd. Band\nChi-Quadrat, \nPermutationstest" ,
                               "Lage waager.\n Bänder u. Reihen\nChi-Quadrat, \nPermutationstest",
                               "Winkel- zu \nDreiecksmuster\nChi-Quadrat, \nPermutationstest",
                               "Ausrichtung \nRandstiche\nChi-Quadrat, \nPermutationstest") )+
  
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
       title = "P-Werte für Gruppenunterschiede in der Verzierung",
       caption = "n variiert je nach Erhaltungszustand und Belegung der Kreuztabellen und ist als label angegeben \n p-Werte mit dem Chi-Quadrat Permutationstest mit 9999 Wiederholungen erhalten, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995) .")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("./analysis/figures/Muster/2024-10-23_alle_p-verz_permutiert3.png", dpi = 300, width = 43, height = 25, units = "cm")
