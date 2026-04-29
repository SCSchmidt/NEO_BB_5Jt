
# "Netzwerke" -> einfaches Distanzmaß zwischen Fundstellen im Vgl

## SBK zu SRK

load("./analysis/data/derived_data/gefform_distance_site_to_site_xy.RData")
load("./analysis/data/derived_data/verz_distance_site_to_site_xy.RData")
load("./analysis/data/derived_data/machart_distance_site_to_site_xy.RData")

verz_site3_xy$parameter <- "verz"

verz_S <- verz_site3_xy |>
  select(FO, FO_from, S, parameter)

machart_site3_xy$parameter <- "mach"

mach_s <- machart_site3_xy |>
  select(FO, FO_from, S, parameter)

gefform_site3_xy$parameter <- "gefform"

gefform_S <- gefform_site3_xy |>
  select(FO, FO_from, S, parameter)


vm <- rbind(verz_S, mach_s, gefform_S)


vm |>
  ggplot()+
  geom_density(aes(x = S,
                   colour = parameter),
               linewidth = 2)+
  labs(x = "S (gemittelte Ähnlichkeitswerte)",
       title = "Verteilung der gemittelten Ähnlichkeitswerte",
       y = "Dichte",
       col = "")+
  scale_colour_discrete(labels = c("Gefäßform", "Machart", "Verzierungs-\ntechniken & -elemente") )+
  theme_bw(base_size = 12)

ggsave("./analysis/figures/Dichteverteilung_gemittelte_S.png", dpi = 300, width = 16, height = 16, units = "cm")

save(vm, file ="./analysis/data/derived_data/gemittelte_S_alleParameter.RData")



############### BKK - SRK - Guhrau - Rössen nach Fst

load("./analysis/data/derived_data/machart_distance_site_to_site_RGBxy.RData")
load("./analysis/data/derived_data/verz_distance_site_to_site_RGBxy.RData")
load("./analysis/data/derived_data/gefform_distance_site_to_site_RGBxy.RData")



### alle RGB gemeinsam

m_RGB <- machart_site3_xy |>
  select(S, FO_from, FO, kultur2 = k, kultur2_from = k_from) |>
  unique()

m_RGB$parameter <- "mach"

v_RGB <- verz_site_s2 |>
  select(S, FO_from, FO, kultur2, kultur2_from) |>
  unique()
v_RGB$parameter <- "verz"


g_RGB <- gefform_site_s2 |>
  select(S, FO_from, FO, kultur2, kultur2_from) |>
  unique()

g_RGB$parameter <- "gefform"

vmg_RGB <- rbind(g_RGB, v_RGB, m_RGB)

vmg_RGB$kultur2[vmg_RGB$kultur2 == "Rös"] <- "Rössen"
vmg_RGB$kultur2_from[vmg_RGB$kultur2_from == "Rös"] <- "Rössen"


save(vmg_RGB, file = "./analysis/data/derived_data/gemittelte_S_alleParameter_RGB.RData")


vmg_RGB |>
  ggplot()+
  geom_density(aes(x = S,
                   colour = parameter),
               linewidth = 1)+
  labs(x = "S (gemittelte Ähnlichkeitswerte)",
       title = "Verteilung der gemittelten Ähnlichkeitswerte",
       y = "Dichte",
       col = "")+
  scale_x_continuous(breaks = c(0,0.5,1),
                     labels = c(0,0.5,1))+
  scale_colour_discrete(labels = c("Gefäßform", "Machart", "Verzierungs-\ntechnik & -elemente") )+
  facet_grid(kultur2 ~ kultur2_from,
             scales = "free")+
  theme_bw()

ggsave("../figures/Dichteverteilung_gemittelte_S_RGB.png", dpi = 300, width = 21, height = 16, units = "cm")


