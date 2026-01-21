# Script to download and clean Gun Violence Archive data for Virginia using 
# The Trace Datahub API and Redivis 

# Resources:
# - The Trace Datahub: https://datahub.thetrace.org/
# - Gun Violence Archive: https://www.gunviolencearchive.org/

# Note: Data access to the Redivis API is managed by the Trace with named user authentication. 

# Libraries ----
library(redivis)
library(tidyverse)

# Download ----
user <- redivis$user("thetrace")
# Will change with each download, copy from Rediviz workspace:
# https://redivis.com/datasets/245r-2cw07j71y
dataset <- user$dataset("x_va_gva:245r:v2_8")
table <- dataset$table("va_gva:z2zk")

# Load:
data <- table$to_tibble() # Will open authentication screen in browser

# Tidy ----
# Create dataset of incidents:

incidents <- data %>%
  select(-business_location_name, -updated_date) %>%
  mutate(incident_date = ymd(incident_date),
         across(matches("killed|injured|arrested|latitude|longitude"),
                ~ as.numeric(.x))) %>%
  rename(total_killed = killed, total_injured = injured)

write_csv(incidents, "data/gva_incidents.csv")
  
# Create dataset of participants:

ps <- data %>%
  select(incident_year, incident_date, participants, city_or_county) %>%
  separate_rows(participants, sep = "},") %>%
  mutate(participants= gsub('\\, ([SJ]r)', ' \\1', participants) %>% 
           gsub(", aka [^,]*'", "'", .) %>% 
           gsub("([a-z])\\,.*'", "\\1'", .) %>% 
           gsub('Q, Roane', 'Q. Roane', .) %>% 
           gsub('\\, Jr', ' Jr', .) %>% 
           gsub('Tommy Joe Strothers\\, Jr\\.\\,', 'Tommy Joe Strothers Jr', .) %>% 
           gsub("Jr\\.\\,'", "Jr.'", .)) %>% 
  separate(participants,
           into = c("id", "type", "name", "age", "age_group", "gender", "status", "relationship"),
           sep = ",") %>%
  mutate(relationship = gsub('\\}\\]', '', relationship)) %>%
  mutate(across(everything(), 
                ~ gsub("'", "", sub("^.*?: ", "", .)))) %>% 
  mutate(across(everything(), 
                ~ gsub('\\}\\]', '', .) %>%
                  na_if(' ') %>% 
                  na_if('')))

write_csv(ps, "data/gva_participants.csv")

# Note: each row represents a participant status. That is, if a participant had 
# multiple statuses (eg. Arrested, Unharmed) they will be listed twice. 

# Example viz ----

incidents <- read_csv("data/gva_incidents.csv")
participants <- read_csv("data/gva_participants.csv")

# Cville compared to VA over time:

## 1 - N incidents 
### 1.1 - N incidents in Cville
### 1.2 - Rate of incidents in Cville compared to VA 
### 1.3 - Incident characteristics? 
### 1.4 - Local incidents YTD
## 2 - N victims 
### 2.1 - N victims and their statuses in Cville
### 2.2 - Rate of victimization in Cville compared to VA ***
## 3 - Participant demographics (age, gender, role)
### 3.1 - Average age over time 

# 1 - Incidents over time 
# *1.1 Number of incidents in Charlottesville and Albemarle ----

# Grouped by quarter -- more visual noise, but shows seasonal changes
local_counts_qtr <- incidents %>%
  filter(incident_date >= "2018-01-01",
         str_detect(city_or_county, "Charlottesville")) %>%
  mutate(quarter = floor_date(as.Date(incident_date), "quarter")) %>%
  count(quarter, name = "quarter_count")

local_counts_qtr %>%
  filter(quarter < as.Date("2026-01-01")) %>%
  ggplot(aes(quarter, quarter_count)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "Number of incidents in Cville/Alb by Quarter")

# Grouped by year -- Less visual noise, but doesn't show seasonal trends 
local_counts_yr <- incidents %>%
  filter(str_detect(city_or_county, "Charlottesville"),
         incident_date >= "2018-01-01") %>%
  count(incident_year, name = "n_year")

local_counts_yr %>%
  filter(incident_year != 2026) %>%
  ggplot(aes(incident_year, n_year, group = 1)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "Number of incidents in Cville/Alb by Year")

# *1.2 Incident rates in Charlottesville and Albemarle compared to VA ----

library(tidycensus)
# Notes on populations over the years: 
# - 2017:2023 available via ACS-5 year estimates
# - 2020 available through the decennial census 
# - 2024 available through Population Estimates via get_estimate()
# - 2025 is not yet available, may need to estimate ourselves? ~8,879,000, 45,434, 118,700

acs_years <- c(2017:2019, 2021:2023)

va_acs <- map_dfr(acs_years,
                     ~ get_acs(
                        geography = "state",
                        variables = "B01003_001",
                        state = "VA",
                        survey = "acs5",
                        year = .x) %>%
                    mutate(yr = .x)) %>%
  select(region = NAME, pop = estimate, yr)

local_acs <- map_dfr(acs_years,
                  ~ get_acs(
                    geography = "county",
                    variables = "B01003_001",
                    state = "VA",
                    county = c(540, 003),
                    survey = "acs5",
                    year = .x) %>%
                  mutate(yr = .x)) %>%
  select(region = NAME, pop = estimate, yr) %>%
  group_by(yr) %>%
  summarise(pop = sum(pop)) %>%
  mutate(region = "Cville and Albemarle")
  
va_dec <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  state = "VA",
  year = 2020) %>%
  select(region = NAME, pop = value) %>%
  mutate(yr = 2020)

local_dec <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  state = "VA",
  county = c(540, 003),
  year = 2020) %>%
  select(region = NAME, pop = value) %>%
  mutate(yr = 2020) %>%
  group_by(yr) %>%
  summarise(pop = sum(pop)) %>%
  mutate(region = "Cville and Albemarle")

va_est_24 <- get_estimates(
  geography = "state",
  product = "population",
  state = "VA",
  year = 2024,
  vintage = 2024) %>%
  filter(variable == "POPESTIMATE") %>%
  select(region = NAME, pop = value, yr = year)

local_est_24 <- get_estimates(
  geography = "county",
  product = "population",
  state = "VA",
  county = c(540, 003),
  year = 2024, 
  vintage = 2024) %>%
  filter(variable == "POPESTIMATE") %>%
  select(region = NAME, pop = value, yr = year) %>%
  group_by(yr) %>%
  summarise(pop = sum(pop)) %>%
  mutate(region = "Cville and Albemarle")

pops <- bind_rows(va_acs, local_acs, va_dec, local_dec, va_est_24, local_est_24) %>%
  arrange(yr)

va_counts <- incidents %>%
  filter(incident_date >= "2017-01-01") %>%
  count(incident_year, name = "n_year") %>%
  mutate(region = "Virginia", 
         yr = as.numeric(incident_year)) %>%
  select(-incident_year)

local_counts <- incidents %>%
  filter(incident_date >= "2017-01-01",
         str_detect(city_or_county, "Charlottesville")) %>%
  count(incident_year, name = "n_year") %>%
  mutate(region = "Cville and Albemarle",
         yr = as.numeric(incident_year)) %>%
  select(-incident_year)

rates <- bind_rows(va_counts, local_counts) %>%
  left_join(pops) %>%
  mutate(
    pop = case_when(
      yr == 2025 & region == "Virginia" ~ 8879000,
      yr == 2025 & region == "Cville and Albemarle" ~ 164134,
      TRUE ~ pop),
    rate = (n_year / pop) * 100000)

rates %>%
  ggplot(aes(yr, rate, colour = region)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "bottom") +
  labs(title = "Incident rates per 100k pop")
  
# 1.3 Incident characteristics ----

local_char <- incidents %>%
  filter(incident_date >= "2017-01-01",
         str_detect(city_or_county, "Charlottesville"))

# Not the most informative, so maybe use alternative data instead (CDC, police, etc.)

# 1.4 YTD incidents in Cville ----

ytd <- incidents %>%
  filter(year(incident_date) == year(Sys.Date()),
         str_detect(city_or_county, "Charlottesville"))
         
last_yr <- incidents %>%
  filter(incident_date >= floor_date(Sys.Date() - years(1), "year") &
           incident_date <= (Sys.Date() - years(1)),
         str_detect(city_or_county, "Charlottesville"))

pct_change <- (nrow(ytd) - nrow(last_yr)) / nrow(last_yr) * 100

# 2 - Victims over time: 
# *2.1 Number of victims and their statuses in Cville ----

# Monthly
local_victims <- incidents %>%
  filter(str_detect(city_or_county, "Charlottesville"),
         incident_date >= "2017-01-01") %>%
  mutate(month_yr = floor_date(incident_date, "month")) %>%
  group_by(month_yr) %>%
  summarise(total_killed = sum(total_killed),
            total_injured = sum(total_injured)) %>%
  pivot_longer(matches("total"))

local_victims %>%
  ggplot(aes(month_yr, value, fill = name)) +
  geom_col() +
  labs(title = "Number of victims by month in Cville/Alb") +
  theme(legend.position = "bottom")

# Annually 
local_victims_yr <- incidents %>%
  filter(str_detect(city_or_county, "Charlottesville"),
         incident_date >= "2017-01-01") %>%
  group_by(incident_year) %>%
  summarise(total_killed = sum(total_killed),
            total_injured = sum(total_injured)) %>%
  pivot_longer(matches("total")) %>%
  mutate(region = "Cville and Albemarle")

local_victims_yr %>%
  ggplot(aes(incident_year, value, fill = name)) +
  geom_col() +
  labs(title = "Number of victims by year in Cville/Alb") +
  theme(legend.position = "bottom")

# *2.2 Victimization rates compared to VA ----

va_victims_yr <- incidents %>%
  filter(incident_date >= "2017-01-01") %>%
  group_by(incident_year) %>%
  summarise(total_killed = sum(total_killed),
            total_injured = sum(total_injured)) %>%
  pivot_longer(matches("total")) %>%
  mutate(region = "Virginia")

victims <- bind_rows(local_victims_yr, va_victims_yr)

victim_rates <- victims %>%
  left_join(pops, by = c("incident_year" = "yr", "region" = "region")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(total_victims = total_killed + total_injured)
  
rates <- victim_rates %>%
  mutate(
    pop = case_when(
      incident_year == 2025 & region == "Virginia" ~ 8879000,
      incident_year == 2025 & region == "Cville and Albemarle" ~ 164134,
      TRUE ~ pop),
    kill_rate = (total_killed / pop) * 100000,
    injured_rate = (total_injured / pop) * 100000,
    vic_rate = (total_victims / pop) * 100000)

rates %>%
  ggplot(aes(incident_year, vic_rate, colour = region)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Victimization rates per 100k pop") +
  theme(legend.position = "bottom")

# 3 - Participant demographics (age, gender, role):
# 3.1 Average age over time by role ----

local_ages <- participants %>%
  filter(str_detect(city_or_county, "Charlottesville"),
         incident_date >= "2017-01-01",
         !duplicated(id)) %>%
  group_by(type, yr = incident_year) %>%
  summarize(avg_age = mean(age, na.rm = T)) %>%
  mutate(region = "Cville and Albemarle")

va_ages <- participants %>%
  filter(incident_date >= "2017-01-01",
         !duplicated(id)) %>%
  group_by(type, yr = incident_year) %>%
  summarize(avg_age = mean(age, na.rm = T)) %>%
  mutate(region = "Virginia")

ages <- bind_rows(local_ages, va_ages)

ages %>%
  filter(!is.na(avg_age)) %>%
  ggplot(aes(yr, avg_age, color = type)) +
  geom_point() +
  geom_line(show.legend = F) +
  facet_wrap(~ region) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Averge age of participants") +
  theme(legend.position = "bottom")

# 3.2 Youth participant roles per pop ----

va_youth <- participants %>%
  filter(incident_date >= "2017-01-01",
         !duplicated(id),
         age_group == "Teen 12-17" | age_group == "Child 0-11") %>%
  mutate(region = "Virginia")

local_youth <- va_youth %>%
  filter(city_or_county == "Charlottesville") %>%
  mutate(region = "Cville and Albemarle")

youth <- rbind(va_youth, local_youth) %>%
  select(incident_year, type, age, age_group, region) %>%
  group_by(yr = incident_year, type, region) %>%
  count() %>%
  left_join(pops) %>%
  mutate(
    pop = case_when(
      yr == 2025 & region == "Virginia" ~ 8879000,
      yr == 2025 & region == "Cville and Albemarle" ~ 164134,
      TRUE ~ pop),
    rate = (n / pop) * 100000)

youth %>%
  ggplot(aes(yr, rate, color = type)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ region) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Rate of youth participation per 100k pop",
       subtitle = "rate = (N unique participants < 18 / total pop) * 100,000") +
  theme(legend.position = "bottom")

# 3.3 Percent youth participation ----

n_youth <- rbind(va_youth, local_youth) %>%
  select(incident_year, type, age, age_group, region) %>%
  count(yr = incident_year, region, name = "n_youth")

n_incidents <- bind_rows(va_counts, local_counts)

pct_youth <- left_join(n_youth, n_incidents) %>%
  mutate(pct_youth = (n_youth / n_year) * 100)

pct_youth %>%
  ggplot(aes(yr, pct_youth, color = region)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Percent of incidents involving youth") +
  theme(legend.position = "bottom")

# 3.4 Ages of participants in Cville and Albemarle ----

local_ps <- participants %>%
  filter(incident_date >= "2017-01-01",
         str_detect(city_or_county, "Charlottesville"),
         !duplicated(id)) 

# 97 ps dropped bc age is NA
local_ps %>%
  count(is.na(age))

age_dat <- local_ps %>%
  drop_na(age) %>%
  mutate(grp = case_when(
           age <= 9 ~ "0-9",
           age > 9 & age <= 14 ~ "10-14", 
           age > 14 & age <= 17 ~ "15-17",
           age > 17 & age <= 19 ~ "18-19",
           age > 19 & age <= 24 ~ "20-24",
           age > 24 & age <= 30 ~ "25-30",
           age > 30 & age <= 35 ~ "31-35",
           age > 35 & age <= 45 ~ "36-45",
           age > 45 & age <= 55 ~ "46-55",
           age > 55 & age <= 65 ~ "56-65",
           age > 65 & age <= 75 ~ "66-75",
           age > 75 ~ "76+")) %>%
  group_by(type, grp) %>%
  count() %>%
  mutate(pyr = ifelse(type == "victim", -n, n))

age_dat %>%
  ggplot(aes(pyr, grp, fill = type)) +
  geom_col() +
  scale_x_continuous(labels = abs) +
  coord_cartesian(xlim = c(-max(age_dat$n), max(age_dat$n))) +
  labs(title = "Ages of participants in Cville/Alb") +
  theme(legend.position = "bottom")
