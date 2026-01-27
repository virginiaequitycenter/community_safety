# Script to pull and prep gun violence data from the Virginia Department of Health 
# via the Virginia Open Data Portal
# Homepage: https://data.virginia.gov/

# Libraries 
library(httr)
library(janitor)
library(jsonlite)
library(reactablefmtr)
library(tidyverse)
library(waffle)

# Download & Tidy ----

## Injuries ----

### County ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in Charlottesville and Albemarle by year. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-citycounty

# Access via CKAN API:
injury_county_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=f79c0a7d-6b41-4b9e-936b-dfc1cfc197ec&q=charlottesville"
injury_county_page <- GET(injury_county_url)
#status_code(injury_county_page)
injury_county_list <- fromJSON(injury_county_url)
injury_county <- injury_county_list$result$records

injury_county <- injury_county %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric) %>%
  rename(locality = patient_city_county)

write_csv(injury_county, "data/vdh_injury_county.csv")

### District ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in the Blue Ridge Health District by year. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-district

# Access via CKAN API:
injury_district_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=e7f3f1b8-c036-4061-a499-ec7aee58951b&q=blue"
injury_district_page <- GET(injury_district_url)
#status_code(injury_district_page)
injury_district_list <- fromJSON(injury_district_url)
injury_district <- injury_district_list$result$records

injury_district <- injury_district %>%
  clean_names() %>%
  select(-id, -patient_health_region, -rank) %>%
  mutate_at(c("firearm_injury_visits", "total_ed_visits", "rate_of_firearm_injuries_per_10k_ed_visits"), as.numeric) %>%
  rename(locality = patient_health_district)

write_csv(injury_district, "data/vdh_injury_district.csv")

### State ----
# This dataset includes the number and rate of emergency department (ED) visits 
# for firearm injury (FAI) in Virginia by year and by month. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-fai-by-month/

# Access via CKAN API:
injury_state_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=4e045ddd-fa67-4368-a2be-8a93233a209f"
injury_state_page <- GET(injury_state_url)
#status_code(injury_state_page)
injury_state_list <- fromJSON(injury_state_url)
injury_state_month <- injury_state_list$result$records

injury_state <- injury_state_month %>%
  clean_names() %>%
  select(-id) %>%
  group_by(year) %>%
  summarise(
    firearm_injury_visits = sum(firearm_injury_visits),
    total_ed_visits = sum(total_ed_visits)
  ) %>%
  mutate(rate_of_firearm_injuries_per_10k_ed_visits = 
           (firearm_injury_visits / total_ed_visits) * 10000,
         locality = "Virginia")

vdh_injuries <- rbind(injury_county, injury_district, injury_state)
write_csv(vdh_injuries, "data/vdh_injuries.csv")

## Deaths by Intent by District ----
# This dataset includes the number and rate of firearm-related deaths in the
# Blue Ridge Health District (Charlottesville, Albemarle, Fluvanna, Nelson, Greene, 
# and Louisa) by intent between 2018-2022. 
# Download CSV via URL: https://data.virginia.gov/dataset/vdh-pud-firearm-deaths-by-district-intent

# Access via CKAN API:
deaths_intent_url <- "https://data.virginia.gov/api/3/action/datastore_search?resource_id=b09626e9-3fbd-4ff5-bcba-763cf0659cc1&q=blue"
deaths_intent_page <- GET(deaths_intent_url)
#status_code(deaths_intent_page)
deaths_intent_list <- fromJSON(deaths_intent_url)
deaths_intent <- deaths_intent_list$result$records

deaths_intent <- deaths_intent %>%
  clean_names() %>%
  select(-id, -rank) %>%
  mutate(years = "2018-2022") %>%
  mutate_at(vars(contains("deaths")), as.numeric)

write_csv(deaths_intent, "data/vdh_deaths.csv")

# Example viz ----

## 1 Waffle: deaths by intent ----
vdh_intent <- read_csv("data/vdh_deaths.csv") %>%
  rename(intent = intent_of_injury) %>%
  mutate(intent = case_when(intent == "Undetermined/legal/war" ~ "Undetermined or \nPolice Intervention",
                            intent == "Unintentional" ~ "Accidental",
                            TRUE ~ intent))

vdh_parts <- vdh_intent %>%
  select(intent, firearm_deaths) %>%
  arrange(desc(firearm_deaths)) %>%
  mutate(intent_fct = fct_inorder(intent))

vdh_parts %>%
  ggplot(aes(label = intent_fct, values = firearm_deaths)) +
  geom_pictogram(n_rows = 6, size = 10, aes(colour = intent_fct), flip = FALSE, make_proportional = FALSE) +
  scale_color_manual(
    values = c("#440154FF", "#FDE725FF", "#7AD151FF", "#2f9aa0ff")) +
  scale_label_pictogram(
    values = c("male")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(face = "bold"),
        legend.title=element_blank()) +
  labs(caption = "Data source: Centers for Disease Control, Underlying Cause of Death 2018-2022")

## 2 Table: ED visits ----

orange_pal <- c("#fff2e6", "#ffb54d")

vdh_injuries <- read_csv("data/vdh_injuries.csv")

names(vdh_injuries) <- gsub("_", " ", names(vdh_injuries)) %>%
  str_to_title() %>%
  gsub("Ed", "ED", .)

vdh_injuries %>%
  filter(Locality == "Albemarle County and Charlottesville City") %>%
  select(-Locality) %>%
  arrange(Year) %>%
  reactable(
    defaultColDef = colDef(
      align = "center",
      headerStyle = list(background = "#f7f7f8"),
      minWidth = 50
    ),
    columns = list(
      "Total ED Visits" = colDef(format = colFormat(separators = TRUE)),
      "Rate Of Firearm Injuries Per 10k ED Visits" = colDef(style = color_scales(
        vdh_injuries, colors = orange_pal)),
      Locality = colDef(minWidth = 100)
    ),
    bordered = TRUE,
    highlight = TRUE,
    pagination = FALSE
  ) %>%
  add_source("Data Source: Virginia Department of Health Firearm-Related ED Visits",
             align = "right",
             font_size = 11)

## 3 Plot: hospitalization rates ----

vdh_injuries %>%
  filter(Locality != "Blue Ridge") %>%
  ggplot(aes(Year, `Rate Of Firearm Injuries Per 10k ED Visits`, color = Locality)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Firearm Injury Hospitalization Rates") +
  theme(legend.position = "bottom")
