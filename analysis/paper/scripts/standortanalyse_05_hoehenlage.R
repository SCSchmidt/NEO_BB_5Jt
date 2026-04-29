#### Standortanalyse: Höhenwerte mit DGM 1

source("analysis/paper/scripts/standortanalyse_13_datenvorbereitung_rasterdaten.R")

r_info_HG |>
  filter(kr_group != "NA") |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = DGM,
                  col = kultur2),
              alpha = 0.25)+
  geom_boxplot(aes(x = kultur2,
                   y = DGM,
                   col = kultur2),
               alpha = 0) +
    scale_colour_manual(values = col )+
  labs(title = "Höhenwerte der Fundstellen in Brandenburg",
       subtitle = "nach Region und Kulturgruppe",
       caption = "Daten als Punktabfrage der Fundstellen auf das DGM 1x1 Raster \nQuelle: Landesvermessung und Geobasisinformation Brandenburg (LGB)",
       y = "Höhe m ü NN",
       x = "",
       col = "")+
  theme_bw() +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        legend.position = "none")+ #not tested
  facet_grid(kr_group ~ .)

ggsave("./analysis/figures/Standortanalyse/Höhenwerte_DGM1.png", dpi = 300, height = 23, width = 20, units = "cm")


source("R_functions/farbzuweisungen.R")
### KS-Test DGM über ganz BB 


df2 <- data.frame(kultur = r_info_HG[["kultur2"]],       # select rel cols
                  variable = r_info_HG[["DGM"]])

ls <- list()

kulturen <- unique(df2$kultur)

res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "gr", "method"))
res_df$k <- as.character(res_df$k)
res_df$k1 <- as.character(res_df$k1)
res_df$k2 <- as.character(res_df$k2)
res_df$n_note <- as.character(res_df$n_note)
res_df$p.value <- as.numeric(res_df$p.value)
res_df$gr <- as.character(res_df$gr)
res_df$method <- as.character(res_df$method)

varname <- "DGM1"

data <- df2 

for (i in 1:(length(kulturen)-1 ) ) {
  
  k1 <- kulturen[i] 
  
  for (j in 2:(length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    data2 <- data |>
      filter(kultur %in% c(k1, k2) ) # filter for two cultures
    
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
      d1 <- data2 |> 
        dplyr::filter(kultur == k1)
      
      d2 <- data2 |> 
        dplyr::filter(kultur == k2)
      
      res <- ks.test(d1$variable, d2$variable )
      
      res$gr <- varname
      res$K1 <- k1
      res$K2 <- k2
      res$k <- paste0(k1, " - ", k2)
      res$K1n <- nrow(data2 |> dplyr::filter(kultur == k1)  ) # n soll gefäßanzahl entsprechen
      res$K2n <- nrow(data2 |> dplyr::filter(kultur == k2)  )
      res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
      
      res_zusammen <- data.frame("k" = res$k, 
                                 "k1" = res$K1, 
                                 "k2" = res$K2, 
                                 "n_note" = res$n_note, 
                                 "p.value" = res$p.value, 
                                 "gr" = "BB insg.",
                                 "method" = res$method )  
      
      
      
      res_df <-rbind(res_df, res_zusammen)
      
    }
    
  }
  
}

res_df$p.adj <- p.adjust(res_df$p.value, method = "fdr")


#  plot KS test DGM über ganz BB

library(tidyr)

p_values <- res_df 

p_values_2 <- p_values |>
  pivot_longer(cols = c("k1", "k2"),
               names_to = "pos",
               values_to = "kultur")

#p_values$label <- paste0(p_values$gr)

p_values$k1 <- factor(p_values$k1, ordered = TRUE,
                      levels = c("GG alle 1500m", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))

p_values$k2 <- factor(p_values$k2, ordered = TRUE,
                      levels = c("GG alle 1500m", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))

p_values |>
  filter(p.adj < 0.1) |>
  ggplot() +
  geom_tile(aes(x = k1,
                y = k2,
                fill = round(p.adj, 2 )),
            color = "white" , linewidth = 1.5)+
  geom_tile(aes(x = k2,
                y = k1,
                fill = round(p.adj, 2 )),
            color = "white", linewidth = 1.5 )+
  geom_text(aes(x = k1,
                y = k2 ,
                label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 1,
            nudge_x = -0.3) +
  geom_text(aes(x = k1,
                y = k2 ,
                label = n_note),
            size = 3,
            col = "black",
            hjust = 0.3,
            vjust = 0.5,
            nudge_x = -0.03) +
  geom_text(aes(x = k2,
                y = k1,
                label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 1,
            nudge_x = -0.3) +
  geom_text(aes(x = k2,
                y = k1,
                label = n_note),
            size = 3,
            col = "black",
            hjust = 0.3,
            vjust = 0.5,
            nudge_x = -0.03) +
  scale_fill_gradientn(colours = farben[1:5] ,
                       values =  c(0.00, 0.01, 0.05, 0.1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign."),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "grey")+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Gruppenunterschiede in der Höhenlage",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n p-Werte eines zweiseiten Kolmogoroff-Smirnoff-Test, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("./analysis/figures/Standortanalyse/DGM_BB_KS-Testergebnis_ganzBB_2.png", dpi = 300, width = 42, units = "cm")



########## Nicht benutzt: nach Kreis getrennt


### KS-Test zwischen Kulturgruppen innerhalb eines Kreises

# KS-Test über DGM1
## nach Region getrennt:

kultur <- unique(r_info$kultur2)

# nach: https://stackoverflow.com/questions/73743765/running-ks-test-on-multiple-groups-in-r

source("./R_functions/get_ks.R")

ks_DGM <- get_ks(df = r_info, vars = "DGM", kult = "kultur2", gruppe = "kreislabel")

ks_DGM["BAR"] <- NULL # nur 1 rel. Fst

ks_p <- adjust_p(ks_DGM)


# plot p values of ks test for dgm


farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")


#p_values$label <- factor(p_values$label,
#                          ordered = TRUE,
#                     levels = c(  ) )


p_values <- ks_p |>
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

p_values$label <- paste0(p_values$gr)

p_values |>
  filter(p.adj < 0.1) |>
  ggplot(aes(x = label,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white" )+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
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
       title = "P-Werte für Gruppenunterschiede in der Höhenlage",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n p-Werte eines zweiseiten Kolmogoroff-Smirnoff-Test n, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995), \n gefiltert auf p < 0.1, da n häufig zu niedrig für stat. Signifikanz.")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 12))

#ggsave("./analysis/figures/Standortanalyse/DGM_BB_KS-Testergebnis_kreis.png", dpi = 300, width = 25, units = "cm")

