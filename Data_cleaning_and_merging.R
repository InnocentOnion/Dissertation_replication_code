#########################
#SHEET TO CLEAN AND MERGE DATA ON DEFAULTS AND OUTCOMES
#########################

#Importing sov default data
sov_default_data_raw<-read_xlsx("Asonuma_Trebesch_DEFAULT_DATABASE.xlsx", sheet = "DATASET Defaults & Restruct.", skip = 4)

#cleaning data
sd_data<-sov_default_data_raw %>%
  rename("Case" = "Case nr in Cruces/Trebesch database               (2014 update)")%>%
  mutate(Case = as.double(Case)) #adding  columns to aid in the merging


default_panel_simple <- sd_data %>%
  rename("country" = "Country case") %>%
  select(country, 
         Case,
         `WDI code`, 
         `Start of default or restructuring process: default or announcement`, 
         `End of restructuring: completion of exchange`,
         `Default date`,
         Announcement
         #,`Debt Restructured   (m US$)`, #These columns might not be needed anymore
         #`Preferred Haircut HSZ`,
         #`Face Value Reduction (in %)`
  ) %>%
  pivot_longer(cols = c(`Start of default or restructuring process: default or announcement`, `End of restructuring: completion of exchange`), 
               names_to = "event", values_to = "date")

#Adding all years between first  and last observation in data and creating default dummy
default_panel <- default_panel_simple %>%
  mutate(year = as.Date(as.character(date), format = "%Y")) %>% 
  complete(`WDI code`, year = seq.Date(min(year)-years(5), max(year), by = "year")) %>%#Fill dates from 5 years (1825d) before first default spell
  group_by(country) %>%
  fill(`WDI code`, .direction = "updown") %>% #filling WDI code 
  ungroup() %>%
  group_by(`WDI code`) %>% #creating a default dummy
  mutate(
    default_start = ifelse(!is.na(event) & grepl("Start", event), 1, 0),
    default_end = ifelse(!is.na(event) & grepl("End", event), 1, 0),
    default_episode = cumsum(default_start) - cumsum(default_end),
    defaulting = ifelse(default_episode > 0 | lag(default_episode > 0, default = 0), 1, 0)
  ) %>%
  select(-default_start, -default_end, -default_episode) %>%
  mutate(year = year(year))

default_panel_outcomes<-default_panel %>%
  right_join(maddison_gdp, by = c("WDI code" = "wbcode", "year" = "year")) %>% #merging the default dataset with GDPpC data from World Bank
  ungroup() %>%
  left_join(pip_215, by = c("WDI code" = "wbcode", "year" = "reporting_year"))%>% # year is transformed to numeric - change if necessary
  left_join(WDI_IM, by = c("WDI code", "year")) %>%
  left_join(WDI_LE, by = c("WDI code", "year")) %>%
  left_join(polity_dataP5, by = c("WDI code", "year")) %>%
  left_join(food_data, by = c("WDI code", "year")) %>%
  select(-`Country Name.x`, -`Country Name.y`, -country.y, -scode, -ccode, - Country) %>%
  rename("country" = "country.x") %>%
  mutate(defaulting = ifelse(is.na(defaulting), 0, defaulting),
         Life_expectancy = as.numeric(Life_expectancy),# some values for life expectancy are missing , therefore NAs are created
        Infant_mortality_rate = as.numeric(Infant_mortality_rate)
         ) %>%
  distinct(year, `WDI code`, .keep_all = TRUE) %>% # code removing duplicates
  mutate(country = countrycode(`WDI code`, origin = "wb", destination = "country.name")) %>% 
  left_join(war_data, by = c("WDI code" = "country", "year")) 

#although there is a warning, there are no duplicates for YUG

