# TidyTuesday 2021 W21 | 
# Ask a Manager Salary Survey

library(tidyverse)
library(lubridate)
library(ggtext)
library(showtext)

font_add_google("Lato", "Roboto")
# showtext_opts(dpi = 125)
# showtext_auto(enable = TRUE)


# Load data

tuesdata   <- tidytuesdayR::tt_load('2021-05-18')
survey_tbl <- tuesdata$survey


survey_cleaned_tbl <- survey_tbl %>%
  select(how_old_are_you, industry, job_title, annual_salary, currency, 
         country, state, city, overall_years_of_professional_experience, 
         highest_level_of_education_completed, gender) %>%
  
  set_names(c("age", "industry", "job_title", "annual_salary", "currency",
            "country", "state", "city", "n_experience", "education_level", "gender")) %>%
  
  # clean country col
  mutate(gender = str_to_lower(gender),
         country = str_remove_all(country, "\\."),
         country = str_to_lower(country) %>% str_trim(side = "both"),
         country = 
           case_when(
             grepl("us|usa|united states of america|unites states|united state|u s|america|united sates|united stares", country) ~ "united states",
             grepl("uk|scotland|england", country) ~ "united kingdom",
             TRUE ~ country)
         ) %>%
  
  # clean college col
  mutate(education_level = 
           case_when(education_level == "High School" ~ "High School",
                     education_level == "Some college" ~ "Some College",
                     education_level == "College degree" ~ "College",
                     education_level == "Master's degree" ~ "Masters",
                     education_level == "Professional degree (MD, JD, etc.)" ~ "Professional degree",
                     TRUE ~ education_level)) %>%
  
  mutate(rank_education = 
           case_when(education_level == "High School" ~ 6,
                     education_level == "Some College" ~ 5,
                     education_level == "College" ~ 4,
                     education_level == "Masters" ~ 3,
                     education_level == "PhD" ~ 2,
                     education_level == "Professional degree" ~ 1)) %>%
  
  drop_na(education_level) %>%
  mutate(education_level = factor(education_level, 
                                  levels= c("Some College", "College", "High School", "Masters", "PhD", "Professional degree"))) %>%
  
  # clean n_experience col
  mutate(n_experience =
           case_when(
             grepl("less|2 - 4 years|5|8", n_experience) ~ "10 years or less",
             grepl("11 - 20 years|21 - 30 years", n_experience) ~ "11 to 30 years",
             grepl("31|more", n_experience) ~ "31 years or more",
             TRUE ~ n_experience)
         ) %>%
  
  # obs for analysis
  filter(country == "united states" | country == "united kingdom" | country == "canada") %>%
  filter(gender == "man" | gender == "woman") %>%
  filter(annual_salary >= 10000) %>%
  
  group_by(country, n_experience, education_level, rank_education, gender) %>%
  summarise(mean_salary = mean(annual_salary),
            n = n()) %>%
  ungroup() %>%
  
  
  pivot_wider(names_from = gender, values_from = c("mean_salary", "n")) %>%
  drop_na() %>%
  
  mutate(salary_gap = abs(mean_salary_man - mean_salary_woman),
         ypos = (mean_salary_man + mean_salary_woman)/2,
         ypos = scales::comma(ypos/1e3, accuracy = 1) %>% as.numeric()) %>%

  mutate(text = str_glue("<span style='font-size: 8pt'>{rank_education}.</span><span style='color:#343a40; font-size:10pt; family:Roboto'>**{education_level}**</span><br><span style='color:#343a40; font-size:9pt'> (</span><span style='color:#9d4edd; font-size:9pt'>{scales::comma(n_woman, accuracy = 1)}</span><span style='color:#343a40; font-size:9pt'> | </span><span style='color:#014F86; font-size:9pt'>{scales::comma(n_man, accuracy = 1)}</span><span style='color:#343a40; font-size:9pt'>)</span>")) %>%
  
  select(-n_man, -n_woman) %>%
  
  filter(country == "united states") %>%
  pivot_longer(mean_salary_man:salary_gap) %>%
  mutate(value = scales::comma(value/1e3, accuracy = 1) %>% as.numeric())


# Plot

survey_cleaned_tbl %>%
  ggplot(aes(value, y = factor(text, levels = rev(levels(factor(text)))))) +
  geom_line(data = survey_cleaned_tbl %>% filter(name != "salary_gap"),
            aes(group = text), color = "#6c757d", size = 5.5, alpha = 0.2) +
  geom_point(data = survey_cleaned_tbl %>% filter(name != "salary_gap"),
             aes(fill = factor(name), color = factor(name)), show.legend = F, size = 5.5, shape = 21) +
  scale_fill_manual(values = c("#89C2D9", "#E0AAFF")) +
  scale_color_manual(values = c("#014F86", "9d4edd")) +
  
  facet_grid(n_experience ~., scales = "free") +
  
  geom_text(data = survey_cleaned_tbl %>% filter(name == "salary_gap"),
            aes(x = ypos, y = text, label = paste0("$", value, "k")), size = 3.2, color = "#a4133c") +
  
  scale_x_continuous(limits = c(45, 200), breaks = seq(50, 200, 25) , labels = scales::dollar_format(suffix = "k")) +
  scale_y_discrete(expand = expansion(mult = c(0.06, 0.08))) +
  coord_cartesian(clip = "off") +
  theme_get() +
  
  theme(text = element_text(family = "Lato", size = 8),
        panel.spacing = unit(1.1, "lines"),
        panel.grid.major.y=element_line(linetype = "dotted", color = "#e9ecef"),
        panel.grid.major.x = element_line(linetype = "dotted", color = "gray85"),
        panel.background = element_rect(color = "#e9ecef", fill = "#f8f9fa", linetype = "dotted"),
        axis.title =element_text(color="grey15", size=8),
        axis.text.x = element_text(color="#343a40", size = 9, family = "Lato", face = "bold", vjust = -3),
        axis.text.y = element_markdown(hjust = 0, size = 10), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "gray85"),
        plot.title=element_markdown(size=12.5, hjust= 0, vjust = 5),
        plot.margin = unit(c(1, .7, 1, .6), "cm"),
        plot.caption=element_text(margin=margin(t=7), color="grey40", size = 9, vjust = -1),
        strip.text.y = element_text(size = 14, color = "#343a40", face = "bold.italic", family = "Lato"),
        strip.background = element_rect(color = "#f8f9fa", fill = "#e9ecef", size = 0.5)) +
  
  labs(x = "", y = "",
       title = "<span style='color:#343a40; font-size:12pt; family:Roboto'>Annual </span><span style='color:#a4133c; font-size:12pt; family:Roboto'>**salary gap**</span> between </span><span style='color:#343a40'>**men**</span> & <span style='color:#9d4edd'>**women**</span> in the US<br> based on education & experience",
       subtitle = "Salaries expressed in thousand USD. In brackets number of people surveyed",
       caption = "Source: Ask a Manager survey | Graphic: Albert Campillo")
  

ggsave("../MyDataVisualizations/TidyTuesday/2021_W21/2021_W21.png", height=9.5, width=7, unit="in", bg="white")

