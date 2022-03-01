# Libraries
library(tidyverse)
library(janitor)
library(showtext)
library(patchwork)
library(ggtext)

library(cowplot)

showtext_auto(enable = TRUE)
showtext_opts(dpi = 300)

font_add_google("Roboto")
font_add_google("Lato")
font_add_google("Fjalla One")

f1 <- "Lato"
f2 <- "Roboto"
f3 <- "Fjalla One"

# Data Load
freedom <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv") %>% clean_names()


# Data Prep

europe_data <- freedom %>%
  rename(civil_liberties = cl,
         political_rights = pr,
         least_developed = is_ldc) %>%
  
  mutate(status = 
           case_when(status == "F" ~ "Free",
                     status == "NF" ~ "Not Free",
                     status == "PF" ~ "Partially Free")) %>%
  
  filter(region_name == "Europe") %>%
  mutate(country =
           case_when(country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK",
                     country == "Republic of Moldova" ~ "Moldova",
                     country == "Russian Federation" ~ "Russia",
                     country == "Czechia" ~ "Czech Republic",
                     TRUE ~ country)) %>%
  
  mutate(label = ifelse(country == "Russia", paste0(year,"\n", status), NA_character_))%>%

  filter(year %in% c(1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020))
  


country_list <- europe_data %>%
  select(country) %>%
  distinct()


mapdata <- map_data("world")  %>%
  semi_join(country_list, by = c("region" = "country")) %>%
  select(long, lat, region, group)


m <- mapdata %>%
  left_join(europe_data, by = c("region" = "country"))

palette <- c("#95afa6", "#859a89", "#658d85", "#437373", "#244f56", "#103d43", "#0b1820")


plot <- ggplot(m, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = factor(political_rights)), color = "white", size = 0.2) +
  
  scale_fill_manual(values = palette) +
  guides(fill = guide_legend(title = "Freedom level (1: Totally free)", title.position = "top", title.hjust = 0.5,
                             nrow = 1, label.position = "bottom")) +

  facet_wrap(~ year, ncol = 3, strip.position = "top") +
  theme_void()+
  
  labs(title = "",
       subtitle = "",
       caption = "Data: Freedom House | Graphic: Albert Campillo",
       fill = NULL) +

  theme(plot.margin = unit(c(2.8, 1, 1.2, 1), "cm"),
        strip.text.x = element_text(color = "#a4133c", size = 8, face = "bold", family = f2),
        legend.direction  = "horizontal",
        legend.position   = c(0.5, 1.09),
        legend.title = element_text(size = 7, face = "italic"),
        legend.text  = element_text(size = 6),
        legend.key.size = unit(0.4, units = "cm"),
        legend.key.width  = unit(0.4, units = "cm"),
        legend.key.height = unit(0.4, units = "cm"),
        plot.caption = element_text(size = 8,  family = f3, vjust = -0.1, hjust = 0.5),
        legend.justification = "center")

# Plot w/o image

pl <- ggdraw(plot) +
  draw_text("Political Freedom in", size = 24,  family = f1, vjust = 8, hjust = 0.5,
            x = 0.5, 1.18) +
  draw_text("Putin-ruled Russia", family = f1, face= "bold", size = 17,
            x = 0.5, y = 0.88)

ggsave("../MyDataVisualizations/TidyTuesday/2020_W07/2022_W07.png", pl, height=7, width=10, unit="in", bg="white")


# Plot w/ image

pl2 <- ggdraw(plot) +
  draw_text("Political Freedom in", size = 24,  family = f1, vjust = 8, hjust = 0.5,
            x = 0.5, 1.18) +
  draw_text("Putin-ruled Russia", family = f1, face= "bold", size = 17,
            x = 0.5, y = 0.88) +
  draw_image("https://pbs.twimg.com/media/D2HvT1hWsAAoCKX?format=png&name=small", width = 0.2, height = 0.2, x = 0.84, y = 0.8)

ggsave("../MyDataVisualizations/TidyTuesday/2020_W07/2022_W07_2.png", pl2, height=7, width=10, unit="in", bg="white")














