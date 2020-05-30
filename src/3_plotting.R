#===============================================================================
# 2020-03-20 -- covid19-dem
# Produce plots for the paper
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# Review: Mark Verhagen
#===============================================================================

library(tidyverse)
library(magrittr)
library(sf)
library(patchwork)
library(biscale)
# theming packages
library(hrbrthemes)
library(cowplot)
library(showtext)
library(ggplot2)
library(ggthemes)
library(leaflet)
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()

dir.create("figs_final")
# define own theme
own_theme <- cowplot::theme_map(font_size = 14, font_family = font_rc)+
  ggplot2::theme(
    legend.position = c(.1, .75),
    plot.title = element_blank()
  )

bi_theme <- cowplot::theme_map(font_size = 14, font_family = font_rc)+
  ggplot2::theme(
    legend.position = "none",
    plot.title = element_blank()
  )

own_plot_grid <- function(a, b, ...) {
  require(patchwork)
  (a + b) +
    plot_layout(ncol = 2)+
    plot_annotation(
      tag_levels = "A",
      theme = theme(
        plot.title = element_blank(),
        plot.caption = element_text(family = font_rc, size = 12)
      )
    )& 
    theme(plot.tag = element_text(family = "Roboto Slab", size = 24, face = 2))
}

# load the prepared data

load("data/ready.rda")
load("data/final_eco.rda")

# Fig 1 ------------------------------------------------------------

## ------ Capacity plots ------ ##
# general hospital bed capacity, regional level
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf(
    data = cities, size = 2, shape = 21, stroke = 1, 
    color = "#444444", fill = "#ffffff"
  )+
  geom_sf_text(
    aes(label = pc_capacity %>%  round(1)), 
    size = 5,  color = "#333333", 
    family = font_rc, fontface = 2, 
    nudge_y = -2e4
  )+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Blues', direction = 1,
    breaks = seq(1.6, 2.2, .2)
  ) + 
  own_theme

region_general_capacity <- last_plot()

# acute hospital bed capacity, regional level
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  geom_sf(
    data = cities, size = 2, shape = 21, stroke = 1, 
    color = "#444444", fill = "#ffffff"
  )+
  geom_sf_text(
    aes(label = pc_capacity_acute %>%  round(2)), 
    size = 5,  color = "#333333", 
    family = font_rc, fontface = 2, 
    nudge_y = -2e4
  )+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Reds', direction = 1,
    breaks = seq(.06, .12, .02)
  ) + 
  own_theme 

region_acute_capacity <- last_plot()

# one
fig_01 <-  own_plot_grid(
  region_general_capacity, 
  region_acute_capacity
)

ggsave(filename = "figs_final/fig-01.svg", 
       fig_01, 
       width = 10, height = 7)

# Fig 2 -------------------------------------------------------------------

## ------ Expected Hospitalisation plots ------ ##
# Expected Hospitalisation general care, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = pc_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Hosp. per\n1,000",
    palette = 'YlGnBu', direction = 1
  ) + 
  own_theme

ccounty_expected_hosp_demand <- last_plot()


# Expected Hospitalisation acute care, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = pc_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Hosp. per\n1,000",
    palette = 'RdPu', direction = 1
  ) + 
  own_theme 

ccounty_expected_hosp_acute_demand <- last_plot()

fig_02 <- own_plot_grid(
  ccounty_expected_hosp_demand, 
  ccounty_expected_hosp_acute_demand
)

ggsave(filename = "figs_final/fig-02.svg", 
       fig_02,
       width = 10, height = 7)


# Fig 3  ------------------------------------------------------------------
## ------ Excess demand per 1,000 plots based on 10% ------ ##

# Excess general care demand given 10% infection rate, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = abs_excess_demand_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Excess Need\nper 1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme

ccounty_abs_diff_hosp_demand <- last_plot()

# Excess acute care demand given 10% infection rate, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = abs_excess_demand_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Excess Need\nper 1,000",
    palette = 'BuPu', direction = 1
  ) + 
  own_theme 

ccounty_abs_diff_hosp_acute_demand <- last_plot()

fig_03 <- own_plot_grid(
  ccounty_abs_diff_hosp_demand, 
  ccounty_abs_diff_hosp_acute_demand
)

ggsave(filename = "figs_final//fig-03.svg", 
       fig_03,
       width = 10, height = 7)

# Fig 4 -------------------------------------------------------------------
## LSOA expected hospitalization (per 1,000) for 10% infection, London

# generate custom palette
pal <- RColorBrewer::brewer.pal(11, "BrBG")[c(11,3)]
# load highlight data
london_highlight_df <- readRDS("data/london_highlight.rds")

# hack to fix geom_step aligning 
repeat_last_obs <- function(df) bind_rows(df, df %>% filter(age == "90+") %>% mutate(age = NULL))

# set labels
age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                "70-74", "75-79", "80-84", "85-89", "90+", NULL)

# age distribution graph
london_highlight_df %>% 
  mutate(name = LSOA) %>% 
  dplyr::select(-3:-1) %>% 
  pivot_longer(cols = contains("Age_"), names_to = "age") %>% 
  mutate(
    age = age %>% 
      str_remove("Age_") %>% 
      str_replace("_plus", "+") %>% 
      fct_inorder()
  ) %>% 
  repeat_last_obs() %>% 
  ggplot(aes(age, value * 100, color = name, group = name))+
  geom_hline(yintercept = 0, color = "#666666", size = .5)+
  geom_step(size = 1, position = position_nudge(-.5))+
  coord_flip(xlim = c(.5, 19.5), # need to get rig of the hack row, which is 20
             ylim = c(26.5, 0),
             expand = FALSE)+ 
  scale_x_discrete(position = "top", breaks = age_labels[-20],  labels = age_labels)+
  scale_y_reverse(position = "right")+
  scale_color_manual(NULL, values = pal)+
  theme_minimal(base_family = font_rc, base_size = 15)+
  theme(legend.position = c(.25, .75))+
  labs(y = "Proportion of the population, %",
       x = "Age group")

london_highlight <- last_plot()

# Hospitalization risk general care, London graph
agg_lsoa_s_5 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(
    data = . %>% 
      filter(AreaCodes %in% c("E01000969", 	"E01002225")) %>% 
      st_centroid(), 
    aes(color = AreaCodes),
    shape = 1, size = 10, stroke = .9
  )+
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  scale_color_manual(NULL, values = pal, guide = NULL)+
  scale_fill_fermenter(
    "Hosp. \nper 1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  theme(legend.position = c(.05, .15))

london_pc_hosp <- last_plot()

# overlay graphs
fig_04 <- ggdraw()+
  draw_plot(london_pc_hosp, x =0, y = 0, width = .8, height = 1)+
  draw_plot(london_highlight, x = .67, y = 0, width = .33, height = .5)
  
ggsave(filename = "figs_final/fig-04.pdf", 
       fig_04,
       width = 10, height = 7)


# Including population-level variables in addition to hospitalization risk
# legends

# Fig 5--------------------------------------------------------------
## -- Population Bivariate Plots -- ##
ccg_depriv_df %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = bi_class), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  biscale::bi_scale_fill(pal = "DkBlue", dim=3) +
  bi_theme

ccg_depriv <- last_plot()

ggdraw() +
  draw_plot(ccg_depriv, 0, 0, 1, 1) +
  draw_plot(legend_depriv, .01, 0.45, .35, .35)
ccg_depriv_f <- last_plot()

ccg_dens_df %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = bi_class), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  biscale::bi_scale_fill(pal = "DkViolet", dim=3) +
  bi_theme

ccg_dens <- last_plot()

ggdraw() +
  draw_plot(ccg_dens, 0, 0, 1, 1) +
  draw_plot(legend_dens, .01, 0.45, .35, .35)
ccg_dens_f <- last_plot()

a <- (ccg_depriv_f + ccg_dens_f) +
  plot_layout(ncol = 2)+
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
      plot.caption = element_text(family = font_rc, size = 12)
    ))& 
  theme(plot.tag = element_text(family = "Roboto Slab", size = 24, face = 2))

ggsave(filename = "figs_final/fig-05.svg", 
       a,
       width = 10, height = 7)

# Fig 6--------------------------------------------------------------
## -- Population Bivariate Plots -- ##
# Zoom-in on London - social deprivation and hospitalization risk

b_londen_depriv %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = bi_class, color = NA)) +
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  # geom_sf(data = agg_lsoa_5_b, size = .15, color = "#CAC9C9")+
  scale_color_manual(NULL, values = pal, guide = NULL)+
  biscale::bi_scale_fill(pal = "DkBlue", dim=3) + 
  bi_theme

london_depriv <- last_plot()

ggdraw() +
  draw_plot(london_depriv, 0, 0, 1, 1) +
  draw_plot(legend_depriv, .75, 0.05, .25, .25)
london_depriv_f <- last_plot()

ggsave(filename = "figs_final/fig-06.png", 
       london_depriv_f,
       width = 10, height = 7)

# Fig 7--------------------------------------------------------------
# Zoom-in on London - ethnicity and hospitalization risk

b_londen_eth %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = bi_class, color = NA)) +
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  # geom_sf(data = agg_lsoa_5_b, size = .15, color = "#CAC9C9")+
  scale_color_manual(NULL, values = pal, guide = NULL)+
  biscale::bi_scale_fill(pal = "DkCyan", dim=3) + 
  bi_theme

london_eth <- last_plot()

ggdraw() +
  draw_plot(london_eth, 0, 0, 1, 1) +
  draw_plot(legend_eth, .75, 0.05, .25, .25)
london_eth_f <- last_plot()

ggsave(filename = "figs_final/fig-07.png", 
       london_eth_f,
       width = 10, height = 7)

# Fig 8--------------------------------------------------------------
# Zoom-in on Manchester - social deprivation and hospitalization risk

man_highlight_df <- readRDS("data/man_highlight.rds")

man_highlight_df %>% 
  mutate(name = LSOA) %>% 
  dplyr::select(-3:-1) %>% 
  pivot_longer(cols = contains("Age_"), names_to = "age") %>% 
  mutate(
    age = age %>% 
      str_remove("Age_") %>% 
      str_replace("_plus", "+") %>% 
      fct_inorder()
  ) %>% 
  repeat_last_obs() %>% 
  ggplot(aes(age, value * 100, color = name, group = name))+
  geom_hline(yintercept = 0, color = "#666666", size = .5)+
  geom_step(size = 1, position = position_nudge(-.5))+
  coord_flip(xlim = c(.5, 19.5), # need to get rig of the hack row, which is 20
             ylim = c(26.5, 0),
             expand = FALSE)+ 
  scale_x_discrete(position = "top", breaks = age_labels[-20],  labels = age_labels)+
  scale_y_reverse(position = "right")+
  scale_color_manual(NULL, values = pal)+
  theme_minimal(base_family = font_rc, base_size = 15)+
  theme(legend.position = c(.25, .75))+
  labs(y = "Proportion of the population, %",
       x = "Age group")

man_highlight <- last_plot()

b_man <- agg_lsoa_man %>%
  left_join(LSOA_eco_vars)
b_man_depriv <- biscale::bi_class(b_man, x=pc_hosp, y=depriv)

b_man_depriv %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = bi_class, color = NA)) +
  coord_sf(datum = NA)+
  geom_sf(
    data = . %>% 
      filter(AreaCodes %in% c("E01005654", "E01006084")) %>% 
      st_centroid(), 
    aes(color = AreaCodes),
    shape = 1, size = 10, stroke = .9
  )+
  scale_size_area("Beds", max_size = 10)+
  # geom_sf(data = agg_lsoa_5_b, size = .15, color = "#CAC9C9")+
  scale_color_manual(NULL, values = pal, guide = NULL)+
  biscale::bi_scale_fill(pal = "DkBlue", dim=3) + 
  bi_theme

man_depriv <- last_plot()

ggdraw() +
  draw_plot(man_depriv, x=0, y=0.1, width=.75, height=0.9) +
  draw_plot(legend_depriv, .05, 0.05, .25, .25)+
  draw_plot(man_highlight, x = 0.7, y = 0, width = .33, height = .42)

man_depriv_f <- last_plot()

ggsave(filename = "figs_final/fig-08.pdf", 
       man_depriv_f,
       width = 10, height = 7)

### ---------- SUPPLEMENTARY GRAPHS ---------- ###

# Fig S1 -------------------------------------------------------------------

## ------ Capacity plots ------ ##
# general hospital bed capacity, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = pc_capacity %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Blues', direction = 1
  ) + 
  own_theme 

ccounty_general_capacity <- last_plot()

# acute hospital bed capacity, ccounty level
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  geom_sf_text(aes(label = pc_capacity_acute %>% 
                     round(2) %>% str_replace("0.", ".")), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Reds', direction = 1
  ) + 
  own_theme 

ccounty_acute_capacity <- last_plot()

# s01
fig_s_01 <- own_plot_grid(
  ccounty_general_capacity, 
  ccounty_acute_capacity
)

ggsave(filename = "figs_final/fig-s01.svg", 
       fig_s_01,
       width = 10, height = 7)

# Fig s02 -------------------------------------------------------------------

## ------ Capacity plots ------ ##
# general hospital bed capacity, CCG level

agg_ccg_s %>% 
  ggplot() +
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    breaks  = c(1e-8, .5, 1, 2, 5),
    labels  = c(0, .5, 1, 2, 5),
    palette = 'Blues', direction = 1
  ) + 
  own_theme 

ccg_general_capacity <- last_plot()

# acute hospital bed capacity, CCG level
agg_ccg_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    breaks  = c(1e-8, .1, .25, .5, 1),
    labels  = c(0, .1, .25, .5, 1),
    palette = 'Reds', direction = 1
  ) + 
  own_theme 

ccg_acute_capacity <- last_plot()

fig_s02 <- own_plot_grid(
  ccg_general_capacity, 
  ccg_acute_capacity
)

ggsave(filename = "figs_final/fig-s02.svg", 
       fig_s02,
       width = 10, height = 7)


# Fig s03 -------------------------------------------------------------------
## ------ Expected Hospitalisation plots ------ ##

# Expected general care hospitalisation, ccg level
agg_ccg_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Hosp. per\n1,000",
    palette = 'YlGnBu', direction = 1
  ) + 
  own_theme

ccg_expected_hosp_demand <- last_plot()


# Expected acute care hospitalisation, ccg level
agg_ccg_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Hosp. per\n1,000",
    palette = 'RdPu', direction = 1
  ) + 
  own_theme 

ccg_expected_hosp_acute_demand <- last_plot()

fig_s03 <- own_plot_grid(
  ccg_expected_hosp_demand, 
  ccg_expected_hosp_acute_demand
)

ggsave(filename = "figs_final/fig-s03.svg", 
       fig_s03,
       width = 10, height = 7)

# Fig s04 -----------------------------------------------------------------

## -- Excess demand plots -- ##
# Excess general care hospital demand (per 1,000), ccg level
agg_ccg_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Excess Need\nper 1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme

ccg_abs_diff_hosp_demand <- last_plot()

# Excess acute care hospital demand (per 1,000), ccg level
agg_ccg_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities %>% filter(!name=="Cardiff"), 
          size = 10, shape = 1, stroke = 0.6, color = "#373737")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Excess Need\nper 1,000",
    palette = 'BuPu', direction = 1
  ) + 
  own_theme 

ccg_abs_diff_hosp_acute_demand <- last_plot()

fig_s04 <- own_plot_grid(
  ccg_abs_diff_hosp_demand, 
  ccg_abs_diff_hosp_acute_demand
)

ggsave(filename = "figs_final/fig-s04.svg", 
       fig_s04,
       width = 10, height = 7)

# Fig s05 -------------------------------------------------------------------

# local zoom-in on Wales
agg_lsoa_s_s5 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b_s5, size = .25, color = "#fafafa")+
  geom_sf(data = wales_h, aes(size = beds), 
          shape = 1, stroke = .9, color = "#eec21f")+
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  scale_fill_fermenter(
    "Hosp. per\n1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  theme(legend.position = c(0, .6))

wales_pc_hosp <- last_plot()

# filter out the hospitals with intensive care beds
wales_h_ic <- wales_h %>% filter(intensive_care_beds > 0)

# Excess demand acute LSOA Wales
agg_lsoa_s_s5 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b_s5, size = .25, color = "#fafafa")+
  geom_sf(data = wales_h_ic, aes(size = intensive_care_beds), 
          shape = 1, stroke = .9, color = "#df356b")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Hops. per\n1,000",
    palette = 'RdPu', direction = 1
  ) + 
  scale_size_area("IC Beds", max_size = 10)+
  own_theme +
  theme(legend.position = c(0, .6))

wales_pc_hosp_acute <- last_plot()

fig_s05 <- own_plot_grid(
  wales_pc_hosp, 
  wales_pc_hosp_acute
)

ggsave(filename = "figs_final/fig-s05.png", 
       fig_s05,
       width = 10, height = 7)