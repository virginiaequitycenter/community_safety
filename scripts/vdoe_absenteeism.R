# Script to download and clean data from VDOE on school-level chronic absenteeism
# Reference: https://github.com/virginiaequitycenter/absenteeism

# Setup ----
library(here)
library(httr)
library(janitor)
library(readxl)
library(tidyverse)

# Data pulled from https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Create vector of urls:
urls <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/65179/638918113454530000", # 2024-2025
  "https://www.doe.virginia.gov/home/showpublisheddocument/57624/638629400729970000", # 2023-2024
  "https://www.doe.virginia.gov/home/showpublisheddocument/49151/638302838303870000", # 2022-2023
  "https://www.doe.virginia.gov/home/showpublisheddocument/20188/638043603951630000", # 2021-2022
  "https://www.doe.virginia.gov/home/showpublisheddocument/20186/638043603940530000", # 2020-2021
  "https://www.doe.virginia.gov/home/showpublisheddocument/20184/638043603927870000", # 2019-2020
  "https://www.doe.virginia.gov/home/showpublisheddocument/20182/638043603917100000", # 2018-2019
  "https://www.doe.virginia.gov/home/showpublisheddocument/20180/638043603903670000", # 2017-2018
  "https://www.doe.virginia.gov/home/showpublisheddocument/20178/638043603892400000") # 2016-2017

# Create vector of destination file names:
dest <- paste0("data/raw/raw_", c(2025:2017), ".xlsx")

if (!dir.exists(here("data/raw/"))) {
  dir.create(here("data/raw"))
}

# Download ----
# Use headers to masquerade as a browser by manually supplying your user-agent,
# otherwise you'll get a Error 403: Forbidden. You'll need to do this every time you
# update one of your browsers. 

# To get your user agent: 
# 1. Open url above (in Chrome): https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# 2. Right click anywhere on the page and select INSPECT
# 3. Navigate to NETWORK tab 
# 4. Resubmit the api request by selecting one of the school download links as an example 
# 5. Click on the request (it will start with image.aspx?...)
# 6. Scroll down to REQUEST HEADERS
# 7. Copy the text after USER-AGENT and paste it into field below

headers = c(
  'user-agent' = 'Mozilla/5.0 (iPhone; CPU iPhone OS 18_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.5 Mobile/15E148 Safari/604.1'
)

custom_dl_func = function(file, dest) {
  res <- GET(url = file, add_headers(.headers = headers))
  bin <- content(res, "raw")
  writeBin(bin, dest)
}

walk2(urls, dest, custom_dl_func)

# Read ----
files <- list.files("data/raw", pattern = ".xlsx", full.names = TRUE)
data <- map_dfr(files, ~read_excel(.x))

# Filter to Cville and Albemarle
data <- data %>%
  clean_names() %>%
  filter(div_num %in% c(2, 104),
         grade == "All Students") %>%
  mutate_at(c(8:9), ~as.numeric(.) %>% replace_na(0)) %>%
  mutate(school_year = gsub(" ", "", school_year)) %>%
  select(school_year, division = div_name, school = sch_name, subgroup,
         number_of_chronically_absent_students, percent_chronically_absent)

# Clean names and prep for analysis 
chronic_absenteeism <- data %>%
  mutate(school_level = gsub(".* ", "", school) %>% 
           gsub("Elem$", "Elementary", .) %>% factor(levels = c("Elementary", "Middle", "High")),
         school_level = case_when(
           (school == "Community Lab School" | 
              school == "Albemarle County Community Public Charter" |
              school == "Walker Upper Elementary") ~ "Middle", 
           school == "Murray School" ~ "High",
           TRUE ~ school_level),
         division = case_when(
           grepl("Charlottesville", division) ~ "CCS",
           TRUE ~ "ACPS"),
         school_short = word(school , 1  , -2),
         school_short = case_when(
           school_short == "Albemarle County Community Public" ~ "ACCP",
           school_short == "Agnor-Hurt" ~ "Agnor", 
           school_short == "Albemarle" ~ "AHS",
           school_short == "Benjamin F. Yancey" ~ "Yancey",
           school_short == "Buford" ~ "CMS",
           school_short == "Burnley-Moran" ~ "Sunrise",
           school_short == "Clark" ~ "Summit",
           school_short == "Jack Jouett" ~ "Journey",
           school_short == "Jackson P. Burley" ~ "Burley",
           school_short == "Johnson" ~ "Tall Oaks",
           school_short == "Joseph T. Henley" ~ "Henley",
           school_short == "Leslie H. Walton" ~ "Walton",
           school_short == "Mary Carr Greer" ~ "Greer",
           school_short == "Meriwether Lewis" ~ "Ivy",
           school_short == "Monticello" ~ "MHS",
           school_short == "Mortimer Y. Sutherland" ~ "Lakeside",
           school_short == "Paul H. Cale" ~ "Mountain View",
           school_short == "Venable" ~ "Trailblazer",
           school_short == "Virginia L. Murray" ~ "Murray",
           school_short == "Western Albemarle" ~ "WAHS",
           school_short == "Charlottesville" ~ "CHS",
           school_short == "Walker Upper" ~ "Walker",
           school_short == "Murray" ~ "Community Lab",
           TRUE ~ school_short)) %>%
  rename(n_students = number_of_chronically_absent_students,
         percent = percent_chronically_absent)

write_csv(chronic_absenteeism, "data/chronic_absenteeism.csv")

# Example viz ----

absenteeism <- read_csv("data/chronic_absenteeism.csv")

ec_colors <- c("Elementary" = "#232D4B", 
               "Middle" = "#007BAB",
               "High" = "#F8BE3D")

all <- absenteeism %>%
  filter(subgroup == "All Students") %>%
  group_by(division, school_year, school_level) %>%
  summarise(percent = mean(percent)) %>%
  mutate(
    division = case_when(
      division == "ACPS" ~ "Albemarle County", 
      division == "CCS" ~ "Charlottesville City"),
    school_year = str_sub(school_year, start = -4))

ggplot(all, aes(x = school_year, y = percent, colour = school_level, group = school_level)) +
  geom_point() +
  geom_path(size = 1) +
  facet_wrap(~division) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = ec_colors, breaks = c("Elementary", "Middle", "High")) +
  labs(y = "Rate of Chronic Absenteeism",
       x = "School Year",
       color = "School Level") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, vjust = .7))
