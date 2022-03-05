# TidyTuesday | Week 5 | Dog Breeds
# Ranking of Dog Breeds and their Sheeding Level

library(tidyverse)
library(ggbump)     # bump chart & sigmoid curves
library(ggtext)     # improved text rendering support ggplot2
library(ggimage)    # use images in ggplot2

library(showtext)   # using fonts in R graphs

font_add_google("Lato")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 = "Lato"


# Load Data
breed_traits      <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>% janitor::clean_names()
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv') %>% janitor::clean_names()
breed_rank_all    <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>% janitor::clean_names()


# Plot Data
rank_data <- 
  breed_rank_all %>%
  pivot_longer(x2013_rank:x2020_rank) %>%
  filter(value <= 10) %>%
  mutate(name = parse_number(name)) %>%
  mutate(y    = trimws(str_replace_all(breed, "\\*|\\(|\\)", ""))) %>%
  mutate(sheed_level = 
           case_when(breed == "Retrievers (Labrador)" ~ 4,
                     breed == "Retrievers (Golden)"   ~ 4,
                     breed == "Germand Shepherd Dogs" ~ 4,
                     breed == "Pointers (German Shorthaired)" ~ 3,
                     breed == "French Bulldogs" ~ 3,
                     breed == "Bulldogs" ~ 3,
                     breed == "Poodles" ~ 1,
                     breed == "Beagles" ~ 3,
                     breed == "Rottweilers" ~ 3,
                     breed == "Dachshunds" ~ 2,
                     breed == "Pembroke Welsh Corgis" ~ 4,
                     breed == "Yorkshire Terriers" ~ 1, 
                     breed == "Boxers" ~ 2)
         )

# Bump plot
p1 <- rank_data %>%
  ggplot(aes(name, value)) +
  geom_bump(aes(group= breed, color= factor(sheed_level)), size =6, smooth= 8, alpha= .8) +
  geom_vline(xintercept= seq(2013, 2020, 1), color= "white", size= .2) +
  scale_y_reverse() +
  scale_x_continuous(breaks= seq(2013, 2020, 1)) +
  scale_color_manual("Sheeding Level (1 to 4)", values= c("#619b8a", "#b8bedd", "#eaac8b", "#e56b6f")) +
  coord_cartesian(clip = "off") +
  theme_void(base_size= 10, base_family= f1) +
  theme(legend.position = "top",
        legend.margin   = margin(t=6,b=-2),
        legend.justification = "left",
        plot.margin     = unit(c(.5, 1, .5, .5), "cm"),
        plot.title      = element_text(face="bold"),
        axis.line.x.bottom = element_line(),
        axis.text.x     = element_text(size=8, margin=margin(t=3)),
        axis.ticks.x    = element_line(),
        axis.ticks.length = unit(.15, "cm"),
        plot.caption.position = "plot",
        plot.caption    = element_text( hjust=0, margin=margin(t=10)),
        legend.text     = element_text(size=8),
        legend.title    = element_text(size=8)
        ) +
  labs(title    = "10 most popular dog breeds and their sheeding level",
       subtitle = "Popularity of dog breeds by AKC registration statistics from 2013-2020",
       caption  = "TidyTuesday Week 5 | Data from American Kennel Club, courtesy of KKakey")

p1 +
  geom_image(data = rank_data %>% filter(name == min(name)), aes(image= image, x= name-.3), asp= 2) +
  geom_image(data = rank_data %>% filter(name == max(name)), aes(image= image, x= name+.3), asp= 2) +
  geom_text(data= rank_data %>% filter(name==2019, value<=5), 
            aes(label=breed, x= 2018.5), size=3, family=f1) + 
  geom_text(data= rank_data %>% filter(name==2020, between(value,6,7)), 
            aes(label=breed, x=2019.8), size=3, family=f1, hjust=1) +
  geom_text(data= rank_data %>% filter(name==2020, between(value,8,10)), 
            aes(label=breed, x=2019.8), size=3,family=f1, hjust=1) +
  geom_text(data= rank_data %>% filter(name==2014, value==6), 
            aes(label=breed, x=2014.25), size=3, hjust=1,family=f1) +
  geom_text(data= rank_data %>% filter(name==2014, value==8), 
            aes(label=breed, x=2014.25), size=3, hjust=1,family=f1)

# Save
ggsave("TidyTuesday/2022_W05/2022_W05.png", bg="white", height=5, width=7.4)    

