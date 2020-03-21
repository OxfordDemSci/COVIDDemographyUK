#===============================================================================
# 2020-03-20 -- covid19-dem
# Produce plots for the paper
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(sf)
library(patchwork)
# theming packages
library(hrbrthemes)
library(cowplot)
library(showtext)
library(ggplot2)
library(ggthemes)
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()

# define own theme
own_theme <- cowplot::theme_map(font_size = 14, font_family = font_rc)+
  ggplot2::theme(
    legend.position = c(.1, .75),
    plot.title = element_text(family = "Roboto Slab", size = 20,  face = 2)
  )


own_plot_grid <- function(a, b, title, ...) {
  require(patchwork)
  (a + b) +
    plot_layout(ncol = 2)+
    plot_annotation(
      tag_levels = "A",
      title = title %>% str_wrap(width = 70),
      caption = caption,
      theme = theme(
        plot.title = element_text(family = "Roboto Slab", size = 20, face = 2),
        plot.caption = element_text(family = font_rc, size = 12)
      )
    )& 
    theme(plot.tag = element_text(family = "Roboto Slab", size = 24, face = 2))
}



# load the prepared data

load("data/for graphs/ready.rda")
load("data/for graphs/labs.rda")


# Fig 1 ------------------------------------------------------------

## ------ Capacity plots ------ ##
# general -- regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  # geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(aes(label = pc_capacity %>%  round(1)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Blues', direction = 1,
    breaks = seq(1.6, 2.2, .2)
  ) + 
  own_theme

region_general_capacity <- last_plot()


# acute -- regions
agg_region_s %>% 
  ggplot() + 
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
  # geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(aes(label = pc_capacity_acute %>%  round(2)), 
               size = 5,  color = "#333333", 
               family = font_rc, fontface = 2)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Beds per\n1,000",
    palette = 'Reds', direction = 1,
    breaks = seq(.06, .12, .02)
  ) + 
  own_theme 

region_acute_capacity <- last_plot()

# one
fig_one <-  own_plot_grid(
  region_general_capacity, 
  region_acute_capacity,
  plot_title1
)

ggsave(filename = "figs_paper/one.pdf", 
       fig_one,
       width = 10, height = 7)

# Fig 2 -------------------------------------------------------------------
# general ccounty

agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
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

# acute -- ccounty
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_capacity_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
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

# two
fig_two <- own_plot_grid(
  ccounty_general_capacity, 
  ccounty_acute_capacity,
  plot_title2
)

ggsave(filename = "figs_paper/two.pdf", 
       fig_two,
       width = 10, height = 7)



# Fig 3 -------------------------------------------------------------------
## ------ Expected Hospitalisation plots ------ ##

# # Expected Hospitalisation regions
# agg_region_s %>% 
#   ggplot() + 
#   geom_sf(aes(fill = pc_hosp), color = NA)+
#   geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
#   geom_sf_text(aes(label = pc_hosp %>%  round(1)), 
#                size = 5,  color = "#333333", 
#                family = font_rc, fontface = 2)+
#   coord_sf(datum = NA)+
#   scale_fill_fermenter(
#     "Cases per\n1,000",
#     palette = 'YlGnBu', direction = 1,
#     breaks = seq(6.5, 8.5, .5)
#   ) + 
#   own_theme 
# 
# region_expected_hosp_demand <- last_plot()
# 
# 
# # Expected Hospitalisation acute regions
# agg_region_s %>% 
#   ggplot() + 
#   geom_sf(aes(fill = pc_hosp_acute), color = NA)+
#   geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
#   geom_sf_text(aes(label = pc_hosp_acute %>%  round(1)), 
#                size = 5,  color = "#333333", 
#                family = font_rc, fontface = 2)+
#   coord_sf(datum = NA)+
#   scale_fill_fermenter(
#     "Cases per\n1,000",
#     palette = 'RdPu', direction = 1,
#     breaks = seq(1.5, 3.5, .5)
#   ) + 
#   own_theme 
# 
# region_expected_hosp_acute_demand <- last_plot()


# Expected Hospitalisation ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(aes(label = pc_hosp %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Cases per\n1,000",
    palette = 'YlGnBu', direction = 1
  ) + 
  own_theme

ccounty_expected_hosp_demand <- last_plot()


# Expected Hospitalisation acute ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(aes(label = pc_hosp_acute %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Cases per\n1,000",
    palette = 'RdPu', direction = 1
  ) + 
  own_theme 

ccounty_expected_hosp_acute_demand <- last_plot()


# three
fig_three <- own_plot_grid(
  ccounty_expected_hosp_demand, 
  ccounty_expected_hosp_acute_demand,
  plot_title3
)

ggsave(filename = "figs_paper/three.pdf", 
       fig_three,
       width = 10, height = 7)


# Fig 4  ------------------------------------------------------------------
## ------ Excess demand per 1,000 plots based on 10% ------ ##

# # Excess demand regions
# agg_region_s %>% 
#   ggplot() + 
#   geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
#   geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
#   geom_sf_text(aes(label = abs_excess_demand_hosp %>%  round(1)), 
#                size = 5,  color = "#333333", 
#                family = font_rc, fontface = 2)+
#   coord_sf(datum = NA)+
#   scale_fill_fermenter(
#     "Cases per\n1,000",
#     palette = 'PuBuGn', direction = 1
#   ) + 
#   own_theme 
# 
# region_abs_diff_hosp_demand <- last_plot()
# 
# 
# # Excess demand acute regions
# agg_region_s %>% 
#   ggplot() + 
#   geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
#   geom_sf(data = agg_region_b, size = .5, color = "#fafafa")+
#   geom_sf_text(aes(label = abs_excess_demand_hosp_acute %>%  round(1)), 
#                size = 5,  color = "#333333", 
#                family = font_rc, fontface = 2)+
#   coord_sf(datum = NA)+
#   scale_fill_fermenter(
#     "Cases per\n1,000",
#     palette = 'PuRd', direction = 1,
#     breaks = seq(1.75, 2.75, .25)
#   ) + 
#   own_theme 
# 
# region_abs_diff_hosp_acute_demand <- last_plot()


# Excess demand ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
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


# Excess demand acute ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = abs_excess_demand_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
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


# four
fig_four <- own_plot_grid(
  ccounty_abs_diff_hosp_demand, 
  ccounty_abs_diff_hosp_acute_demand,
  plot_title4
)

ggsave(filename = "figs_paper/four.pdf", 
       fig_four,
       width = 10, height = 7)

# Fig 5 -------------------------------------------------------------------

# Excess demand LSOA Wales
agg_lsoa_s_5 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(data = agg_ccounty_b_5, size = .25, color = "#fafafa")+
  geom_sf(data = wales_h, aes(size = beds), 
          shape = 1, stroke = .9, color = "#eec21f")+
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  scale_fill_fermenter(
    "Cases per\n1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  theme(legend.position = c(0, .6))

wales_pc_hosp <- last_plot()


# Excess demand acute LSOA Wales
agg_lsoa_s_5 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp_acute), color = NA)+
  geom_sf(data = agg_ccounty_b_5, size = .25, color = "#fafafa")+
  geom_sf(data = wales_h, aes(size = beds), 
          shape = 1, stroke = .9, color = "#df356b")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Cases per\n1,000",
    palette = 'RdPu', direction = 1
  ) + 
  scale_size_area("Beds", max_size = 10)+
  own_theme +
  theme(legend.position = c(0, .6))

wales_pc_hosp_acute <- last_plot()


# five
fig_five <- own_plot_grid(
  wales_pc_hosp, 
  wales_pc_hosp_acute,
  plot_title5
)

ggsave(filename = "figs_paper/five.pdf", 
       fig_five,
       width = 10, height = 7)

# Fig 6 -------------------------------------------------------------------

# Tipping point ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = tipping_point_capacity), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(aes(label = tipping_point_capacity %>%  round(1)), 
               size = 4,  color = "#333333", 
               family = font_rc, fontface = 3)+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Overall\nInfection",
    palette = 'Oranges', direction = 1
  ) + 
  own_theme

ccounty_tipping_point <- last_plot()


# Tipping point acute ccounties
agg_ccounty_s %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = tipping_point_capacity_acute), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  geom_sf_text(
    aes(
      label = tipping_point_capacity_acute %>% round(1) %>% 
        str_replace("0.", ".")
    ), 
    size = 4,  color = "#333333", 
    family = font_rc, fontface = 3
  )+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "Overall\nInfection",
    palette = 'PuRd', direction = 1,
    breaks = seq(.25, 1.5, .25)
  ) + 
  own_theme 

ccounty_tipping_point_acute <- last_plot()


# six
fig_six <- own_plot_grid(
  ccounty_tipping_point, 
  ccounty_tipping_point_acute,
  plot_title6
)

ggsave(filename = "figs_paper/six.pdf", 
       fig_six,
       width = 10, height = 7)



# Fig 7 -------------------------------------------------------------------

pal <- RColorBrewer::brewer.pal(11, "BrBG")[c(11,3)]

london_highlight_df <- readRDS("data/for graphs/london_highlight.rds")

london_highlight_df %>% 
  mutate(name = LSOA %>% str_extract("[^ ]+")) %>% 
  select(-3:-1) %>% 
  pivot_longer(cols = contains("Age_"), names_to = "age") %>% 
  mutate(
    age = age %>% 
      str_remove("Age_") %>% 
      str_replace("_plus", "+") %>% 
      fct_inorder()
  ) %>% 
  ggplot(aes(age, value * 100, color = name, fill = name, group = name))+
  geom_col(width = 1, alpha = .25, color = NA, position = position_nudge(.5))+
  geom_step(size = 1)+
  coord_flip()+
  scale_x_discrete(position = "top")+
  scale_y_reverse(position = "right")+
  scale_color_manual(NULL, values = pal)+
  scale_fill_manual(NULL, values = pal)+
  theme_minimal(base_family = font_rc, base_size = 15)+
  theme(legend.position = c(.25, .75))+
  labs(y = "Proportion of the population, %",
       x = "Age group")

london_highlight <- last_plot()


# Excess demand LSOA London
agg_lsoa_s_7 %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = pc_hosp), color = NA)+
  geom_sf(
    data = . %>% 
      filter(AreaCodes %in% c("E01033583", 	"E01002225")) %>% 
      st_centroid(), 
    aes(color = AreaCodes),
    shape = 1, size = 10, stroke = .9
  )+
  coord_sf(datum = NA)+
  scale_size_area("Beds", max_size = 10)+
  scale_color_manual(NULL, values = pal, guide = NULL)+
  scale_fill_fermenter(
    "Cases per\n1,000",
    palette = 'PuBuGn', direction = 1
  ) + 
  own_theme +
  theme(legend.position = c(.05, .15))+
  labs(title = plot_title7 %>% str_wrap(70))

london_pc_hosp <- last_plot()


fig_seven <- ggdraw()+
  draw_plot(london_pc_hosp, x =0, y = 0, width = .8, height = 1)+
  draw_plot(london_highlight, x = .67, y = 0, width = .33, height = .52)
  
ggsave(filename = "figs_paper/seven.pdf", 
       fig_seven,
       width = 10, height = 7, 
       device = cairo_pdf)



# Fig 8 -------------------------------------------------------------------

# Population density ccounties
agg_ccounty_s %>% 
  mutate(
    area = geometry %>% st_area(),
    popdens = (pop /area * 1e6) %>% as.numeric()
  ) %>% 
  ggplot() + 
  geom_sf(color = NA)+
  geom_sf(aes(fill = popdens), color = NA)+
  geom_sf(data = agg_ccounty_b, size = .25, color = "#fafafa")+
  geom_sf(data = agg_region_b, size = 1, color = "#fafafa")+
  geom_sf(data = cities, size = 10, shape = 1, stroke = 2, color = "#ffffff")+
  coord_sf(datum = NA)+
  scale_fill_fermenter(
    "People per\n1 sq. km",
    palette = 'Oranges', direction = 1
  ) + 
  own_theme +
  labs(caption = "Source: Leverhume Center for Demographic Science\n(using data from ONS, NHS and StatsWales)",
       title = "Population density in counties\nEngland & Wales")

ccounty_popdens <- last_plot()

ggsave(filename = "figs_paper/eight.pdf", 
       ccounty_popdens,
       width = 5, height = 6, 
       device = cairo_pdf)
