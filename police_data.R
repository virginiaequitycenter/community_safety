# Script to tidy police-reported incidents of gun violence in Charlottesville and Albemarle 

# This data includes an anonymized collection of all gun violence incidents recorded by the Charlottesville 
# Police Department (CPD), the Albemarle County Police Department (ACPD), and the UVA Police Department (UPD). 

# This data is downloaded from the ACPD SQL database and shared with the Center for Community Partnerships. 
# It is not publicly available for download. 

# Libraries ----
library(tidyverse)
library(janitor)

# All incidents of gun violence from 2019 - 2025
data <- readxl::read_excel("data/raw/2025 - GV Incidents with Subject Information.xlsx") %>%
  clean_names()

# Filter to only include verified  gun violence incidents: 
# Shots Fired 1/2/3, 13A, 09A, Disorder w/Weapon, Gunshot Wound 1 Patient, Shooting/Stabbing, 
# Shooting/Stabbing 1, Disorder w/ Weapon 1, Active Shooter 1, Domestic w/ Weapon 1, 
# Stabbing/Gunshot ALS, Shooting/Stabbing 1 EMS, 09A,13A, Shooting/Stabbing 2

gv <- data %>%
  filter(str_detect(crime_code, "Shots|09A|13A|Active Shooter|Gunshot|Shoot"),
         !str_detect(verified, "^UN")) %>%
  mutate(reported_date = ymd(as.Date(reported_date)))

write_csv(gv, "data/police_gv.csv")


# Example viz ----

gv <- read_csv("data/police_gv.csv")
gva <- read_csv("data/gva_incidents.csv")


gv_plt <- gv %>%
  mutate(month_year = floor_date(reported_date, unit = "month")) %>%
  group_by(month_year) %>%
  count(name = "n_reports")

gva_plt <- gva %>%
  filter(between(incident_date, mdy("01-01-2019"), mdy("12-31-2025")),
         str_detect(city_or_county, "Charlottesville")) %>%
  mutate(month_year = floor_date(incident_date, unit = "month")) %>%
  group_by(month_year) %>%
  summarise(total_injured = sum(total_injured),
            total_killed = sum(total_killed)) %>%
  ungroup() %>%
  pivot_longer(matches("total"))

ggplot() +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name), width = 25) +
  stat_smooth(data = gv_plt, aes(x = month_year, y = n_reports), se = FALSE, span = .2, color = "#2f9aa0ff") +
  labs(x = NULL,
       y = "Monthly Counts",
       caption = "Data Source: Gun Violence Archive (Victim Statuses) & Local Police Reports (Incident Counts") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", date_minor_breaks="1 month") +
  scale_fill_manual(labels = c("Injured", "Killed"),
                    values = c("#440154FF", "#7AD151FF"),
                    guide = guide_legend(title = "Victim Status")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35),
        panel.grid.major.x = element_line(size = 1.5)) +
  annotate("label", x = mdy("11-1-2018"), y = 5.5, 
           label = paste("Gun-related 911 calls\n(ex. shots fired or assaults)"),
           size = 3) +
  geom_segment(aes(x = mdy("7-1-2018"), y = 6.7, xend = mdy("12-20-2018"), yend = 7.8),
               arrow = arrow(length = unit(0.20, "cm"))) +
  geom_vline(xintercept = mdy("3-12-2020"), linetype = "dotted", size = 0.75) +
  annotate("label", x = mdy("11-15-2020"), y = 9, label = "COVID lockdown", size = 3) +
  geom_segment(aes(x = mdy("7-1-2020"), y = 9.6, xend = mdy("4-1-2020"), yend = 10.5),
               arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("label", x = mdy("2-15-2022"), y = 19, label = paste(
    "Hearing or witnessing gun violence takes\na collective toll on our entire community"),
    size = 3) +
  geom_segment(aes(x = mdy("11-15-2020"), y = 19, xend = mdy("7-15-2020"), yend = 17.9),
               arrow = arrow(length = unit(0.2, "cm")))

# Plotly-ify ----

library(plotly)

p <- ggplot() +
  geom_col(data = gva_plt, aes(x = month_year, y = value, fill = name)) +
  geom_line(data = gv_plt, aes(x = month_year, y = n_reports))

ggplotly(p)

plot_ly() %>%
  add_bars(
    data = gva_plt,
    x = ~month_year,
    y = ~value,
    color = ~name,
    name = ~name
  ) %>%
  add_trace(
    data = gv_plt,
    x = ~month_year,
    y = ~n_reports
  )
