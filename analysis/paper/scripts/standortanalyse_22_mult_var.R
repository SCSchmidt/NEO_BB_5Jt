### multivariate Analyse der Fundstellen in PL, MittelD und BB anhand der SGDBE Daten

load(file = "./analysis/data/derived_data/all_standortparameter_2025-01-07.RData")

# aitchinson distance

library(compositions)

all_standortpara$kultur2[all_standortpara$kultur2 == "SRK?"] <- "SRK"
all_standortpara$LAND[is.na(all_standortpara$LAND)] <- "Deutschland"

## NAs raus
all_standortpara <- all_standortpara |>
  select(-c("AGLI1NNI_NA","AGLI2NNI_NA","AWC_SUB_#", "AWC_SUB_NA","AWC_TOP_#",  "AWC_TOP_NA", "PARMADO_NA", "PARMASE_NA", "BS_SUB_NA" ,"BS_TOP_NA" ,"DGH_NA" , "PARMASE_0"  , "PARMADO_0" ,  "TXSRFDO_NA" ,"TXSRFDO_0",   "WRBFU_1 1 1"  ,"WRBFU_3 3 33 3",  "WRBFU_NA","WRBFU_" ))


all_standortpara_n0 <- all_standortpara

all_standortpara_n0[all_standortpara_n0 == 0] <- 0.0001


all_standortpara_n0 <- all_standortpara_n0 |>
  select(-c(starts_with("zmin"), starts_with("zmax"), starts_with("HG"), starts_with("EAWC_TOP"), starts_with("SLOPEDO") ) )


standortpara_comps <- compositions::acomp(all_standortpara_n0[,c(9:137)]) #alle Parameter zusammen müssen zu 15 x 100% zusammenrechenbar sein -> zwar sind eigentlich immer nur ein paar der Parameter direkt voneinander abhängig, aber hmhm. :-/

# frage: das immer getrennt? und danach erst zusammenfügen? 

a_dist_standort <- compositions::dist(standortpara_comps)

m_dist_standort <- compositions::dist(standortpara_comps, method = "manhattan")



### reduzierte Variablen

### reduzierte var PCA berechnen

x <- all_standortpara_n0 |>
  select(matches("AGL|PARMA|WRBFU|TX|AWC" ) )

standortpara_comps <- compositions::acomp(x)

pcx <- princomp(standortpara_comps) # # problem mit den 0  durch eingabe von 0.0001 gelöst

l <- as.data.frame(pcx$loadings[,1:3])

pcd <- as.data.frame(pcx$scores[,1:3])

pcd <- merge(pcd, all_standortpara, by = 0) # merge by rownumber


scale <- min(max(abs(pcd$Comp.1))/max(abs(l$Comp.1)),
             max(abs(pcd$Comp.2))/max(abs(l$Comp.2))) * 0.8
## get  proportion of variance explained by each principal component (following: https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Multivariate-approaches/Principal-Component-Analysis/PCA-in-R/index.html)
pcx_var <- pcx$sdev^2

#To compute the proportion of variance explained by each principal component, we simply divide the variance explained by each principal component by the total variance explained by all principal components:

pcx_ve <- pcx_var * 100 / sum(pcx_var)

### plot allg Übersicht über Bundesländer

ggplot()+
  geom_segment(data = l * scale,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               color = "black", arrow = arrow(angle = 25, length = unit(4, "mm")))+
  geom_text(data = l * scale,
            aes(x=Comp.1, 
                y=Comp.2,
                label = rownames(l) ), colour = "black"  ) +
  geom_text(data = pcd, aes(x=Comp.1, 
                            y=Comp.2,
                            col = BUNDESLAND,
                            label = fid),
            nudge_x = 0.2,
            nudge_y = -0.2,
            size = 3) +
  geom_point(data = pcd, aes(x=Comp.1, y=Comp.2,
                             col = BUNDESLAND),
             alpha = 0.5) +
  labs(x = paste("Comp.1 (", round(pcx_ve[1], 2), "%)" ),
       y = paste("Comp.2 (", round(pcx_ve[2],2), "%)" ),
       title = "Compositional PCA") +
  coord_fixed()+
  theme_bw()

ggsave("./analysis/figures/Standortanalyse/compPCA_red_para_Bundesland.png", dpi = 300, width = 25, units = "cm")



ggplot()+
  #  geom_segment(data = l * scale,
  #               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
  #               color = "black", arrow = arrow(angle = 25, length = unit(4, "mm")))+
  #    geom_text(data = l * scale,
  #            aes(x=Comp.1, 
  #                y=Comp.2,
  #                label = rownames(l) ), colour = "black"  ) +
  #  geom_text(data = pcd, aes(x=Comp.1, 
  #                            y=Comp.2,
  #                             col = BUNDESLAND,
  #                            label = fid),
  #                          nudge_x = 0.2,
  #            nudge_y = -0.2,
  #            size = 3) +
  geom_point(data = pcd, aes(x=Comp.1, y=Comp.2,
                             col = BUNDESLAND),
             alpha = 0.5) +
  labs(x = paste("Comp.1 (", round(pcx_ve[1], 2), "%)" ),
       y = paste("Comp.2 (", round(pcx_ve[2],2), "%)" ),
       title = "Compositional PCA") +
  stat_ellipse(data = pcd, aes(x=Comp.1, y=Comp.2,
                               col = BUNDESLAND,
                               group = BUNDESLAND)) +
  #stat_ellipse(data = pcd, aes(x=Comp.1, y=Comp.2,
  #                             col = kultur2,
  #                 group = kultur2))+
  coord_fixed()+
  theme_bw()

ggsave("./analysis/figures/Standortanalyse/compPCA_red_para_Bundesland_nurpunkte_ellipse.png", dpi = 300, width = 25, units = "cm")

### plot nach Kulturgruppe

source("./R_functions/farbzuweisungen.R")

ggplot()+
  geom_segment(data = l * scale,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               color = "black", arrow = arrow(angle = 25, length = unit(4, "mm")))+
  geom_text(data = l * scale,
            aes(x=Comp.1, 
                y=Comp.2,
                label = rownames(l) ), colour = "black"  ) +
  geom_text(data = pcd, aes(x=Comp.1, 
                            y=Comp.2,
                            col = kultur2,
                            label = fid),
            nudge_x = 0.2,
            nudge_y = -0.2,
            size = 3) +
  geom_point(data = pcd, aes(x=Comp.1, y=Comp.2,
                             col = kultur2),
             alpha = 0.5) +
  scale_color_manual(values = col)+
  labs(x = paste("Comp.1 (", round(pcx_ve[1], 2), "%)" ),
       y = paste("Comp.2 (", round(pcx_ve[2],2), "%)" ),
       title = "Compositional PCA") +
  coord_fixed()+
  theme_bw()

ggsave("./analysis/figures/Standortanalyse/compPCA_red_para_kultur.png", dpi = 300, width = 25, units = "cm")



pcd_filter_for_plot <- pcd |>
  filter(BUNDESLAND %in% c("Brandenburg", "Woj. Kujawsko-Pomorskie", "Sachsen-Anhalt", "Thüringen", "Zachodniopomorskie",
                           "Woj. Wielkopolskie" , "Woj. Śląskie", "Woj. Pomorskie","Woj. Dolnośląskie"   ))

ggplot()+
  geom_point(data = pcd, aes(x=Comp.1, y=Comp.2,
                             col = kultur2),
             alpha = 0.5) +
  labs(x = paste("Comp.1 (", round(pcx_ve[1], 2), "%)" ),
       y = paste("Comp.2 (", round(pcx_ve[2],2), "%)" ),
       title = "Compositional PCA") +
  stat_ellipse(data = pcd_filter_for_plot, aes(x=Comp.1, y=Comp.2,
                                               fill = BUNDESLAND,
                                               group = BUNDESLAND),
               geom="polygon", alpha=0.2) +
  stat_ellipse(data = pcd, aes(x=Comp.1, y=Comp.2,
                               col = kultur2,
                               group = kultur2))+
  scale_color_manual("",
                     values = col)+
  coord_fixed()+
  theme_bw()

ggsave("./analysis/figures/Standortanalyse/compPCA_red_para_kultur_bundesland_zusammen.png", dpi = 300, width = 25, units = "cm")
