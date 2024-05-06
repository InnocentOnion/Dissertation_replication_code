##############################
#SHEET TO IMPORT ALL SUPPLEMENTARY DATA
#############################
library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)
library(lubridate)
library(peacesciencer)
download_extdata()
library(vdemdata)
library(janitor)
library(wid)
library(haven)
library(zoo)

################
#Importing and cleaning WDI GDP data
WDI_GDPpc<-read_xlsx("GDP_per_Capita_Data_WDI.xlsx")
GDPpcCU<-WDI_GDPpc %>%
  filter(`Series Name` == "GDP per capita (constant 2015 US$)") %>%
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(cols = names(.)[-c(1:2)], 
               names_to = "year", values_to = "GDPpc") %>%
  mutate(year = as.Date(paste0(year, "-01-01"),format = "%Y"))

#Importing and cleaning Maddison GDP data
maddison_gdp<-read_xlsx("mpd2020.xlsx", sheet = 3) %>%
  filter(year >= 1970) %>%
  #mutate(wbcode = countrycode(countrycode, origin = "iso3c", destination = "wb" )) %>% 
  mutate(wbcode = ifelse(countrycode == "YUG", "YUG", countrycode(countrycode, origin = "iso3c", destination = "wb" ))) %>%
  select(wbcode, year, gdppc)


#Importing and cleaning Infant mortality and life expectancy
IM_LE_data<-read_xlsx("P_Data_Extract_From_World_Development_Indicators.xlsx")
WDI_IM<-IM_LE_data %>%
  filter(`Series Name` == "Mortality rate, infant (per 1,000 live births)") %>%
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(cols = names(.)[-c(1:2)], 
               names_to = "year", values_to = "Infant_mortality_rate") %>%
  mutate(year = as.numeric(gsub("(\\d{4}).*", "\\1", year))) %>%
  rename("WDI code" = "Country Code")

WDI_LE<-IM_LE_data %>%
  filter(`Series Name` == "Life expectancy at birth, total (years)") %>%
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(cols = names(.)[-c(1:2)], 
               names_to = "year", values_to = "Life_expectancy") %>%
  mutate(year = as.numeric(gsub("(\\d{4}).*", "\\1", year))) %>%
  rename("WDI code" = "Country Code")

#Importing and cleaning Polity V data
polity_dataP5<-read_xls("p5v2018.xls") %>%
  select(ccode, scode, country, year, polity2, democ, autoc, xconst, polcomp, parreg, parcomp, exrec, regtrans) %>% 
  group_by(country) %>% 
  mutate(reg_change= zoo::rollapply(regtrans, 15, function(x) {
    if(all(is.na(x))) {
      0
    } else {
      sum(x, na.rm = TRUE)
    }
  }, align = "right", fill = NA)) %>% 
  ungroup()
##polity P2 is chosen because it contains no "standardized authority scores" (-77, -88, -66)
##see documentation: https://www.systemicpeace.org/inscr/p5manualv2018.pdf

##Transforming country codes to WDI codes
polity_dataP5$`WDI code`<-countrycode(polity_dataP5$ccode, origin = "p5n", destination = "wb")
polity_dataP5$wbcode<-polity_dataP5$`WDI code`


##Importing an cleaning food data
food_data<-read.csv("global-food.csv") %>%
  select(Country, Year, Food.supply..kcal.per.capita.per.day.) %>%
  mutate("WDI code" = countrycode(Country, origin = "country.name", destination = "wb")) %>%
  rename("kcal_per_head_per_day" = "Food.supply..kcal.per.capita.per.day.",
         "year" = "Year")
food_data$`WDI code`[food_data$Country == "Yugoslavia"] <-"YUG"

#Importing and cleaning war data
##Creating inter country war data
inter_war_data<-create_dyadyears()%>%
  filter(year %in% c(1970:2023))%>%
  add_cow_wars(type = "inter") %>%
  group_by(year, ccode1) %>% #collapsing data in year-country pairs
  summarize(AtWar = max(cowinterongoing)) %>% #is country in at least one war in this year
  mutate(country = countrycode(ccode1, origin = "cown", destination = "wb"),
         country = ifelse(ccode1 == 260 | ccode1 == 265, "DEU", 
                        ifelse(ccode1 == 345, "YUG", 
                               ifelse(ccode1 == 315, "CZR", 
                                      ifelse(ccode1 == 678 | ccode1 == 680, "YEM", 
                                             ifelse(ccode1 == 817, "VNM", country ))))))

##creating intra country war database                                                                                          
intra_war_data<-create_stateyears()%>%
  filter(year %in% c(1970:2023))%>%
  add_cow_wars(type = "intra") %>%
  select(ccode, year, cowintraongoing) %>%
  mutate(country = countrycode(ccode, origin = "cown", destination = "wb"),
         country = ifelse(ccode == 260 | ccode == 265, "DEU", 
                          ifelse(ccode == 345, "YUG", 
                                 ifelse(ccode == 315, "CZR", 
                                        ifelse(ccode == 678 | ccode == 680, "YEM", 
                                               ifelse(ccode == 817, "VNM", country ))))))

##merging data on wars
war_data<-inter_war_data %>%
  rename("inter_state_war" = "AtWar") %>%
  left_join(intra_war_data, by = c("country", "year")) %>%  #Warning because Germany twice in dataset before 1991
  select(-ccode1, -ccode) %>% 
  mutate(war = inter_state_war + cowintraongoing, #creating a variable to capture whether country was engaged in any war
         war = ifelse(war == 2 | war == 1, 1, 0)) %>% 
  select(country, year, war) %>%
  distinct(year, country, .keep_all = TRUE)

#########################################
#IMPORTING MODERATOR DATA
#########################################

#Importing VDem data 
## civil society participation and corruption

vdem_data<-vdemdata::vdem %>%
  select(year, country_name, 
         country_text_id, 
         country_id, 
         v2x_corr,
         v2x_jucon,
         v2psparban,
         ) %>%
  filter(year > 1969) %>% 
  mutate(wbcode = countrycode(country_id, origin = "vdem", destination = "wb")) %>% 
  select(-country_name, -country_text_id, -country_id)



# Importing Barro human capital data 
##dataset has data on average years of schooling for the whole population (both sexes) between ages 15 and 64
hc_data<-read.csv("Barro_data_original.csv") %>%
  select(-country,-region_code,-sex, -pop, -agefrom, -ageto, -BLcode) %>%
  rename("wbcode" = "WBcode") %>% 
  filter(year>1960)%>% 
  mutate(year = as.numeric(year))

# importing data on physicians per capita, government expenditure and executive changes
cnts_data<-read_xlsx("2023 Edition CNTSDATA files with LINKS/2023 Edition CNTSDATA.xlsx")
colnames(cnts_data)<-cnts_data[1,]
cnts_data<-cnts_data[-1,]
cnts_data<-cnts_data %>%
  select(year, country, Wbcode, revexp5, revexp6, polit03, polit12, economics2, physician1)%>%
  rename("wbcode" = "Wbcode") %>% 
  group_by(wbcode) %>% 
  mutate(exec_change= zoo::rollapply(as.numeric(polit12), 10, function(x) {
    if(all(is.na(x))) {
      0
    } else {
      sum(x, na.rm = TRUE)
    }
  }, align = "right", fill = NA)) %>% 
  ungroup() %>% 
  filter(year>1964) %>% 
  mutate_at(vars(-wbcode, -country), as.numeric) %>% 
  mutate(revexp6 = revexp6*0.01,
         exp_share_gdp = ifelse(is.na(revexp6/economics2), NA, revexp6/economics2)) #%>% 
  #group_by(wbcode) %>% 
  #mutate(avg_exec_change = mean(polit12, na.rm = TRUE)) %>% 
  #ungroup()

# importing data on GINI (PIP with poverty line at 2.15)
pip_215<-read.csv("pip_215.csv") %>% 
  select(country_code, reporting_year, reporting_level, headcount) %>% 
  rename("wbcode" = "country_code") %>% 
  mutate(year = reporting_year,
         headcount = headcount*100) %>% 
  filter(reporting_level == "national")

#importing data on age-dependency ratio
agdr_data<-read.csv("age-dependency-ratio-of-working-age-population/Age dependency ratio (% of working-age population).csv") %>%
  rename("age_dependency_ratio" = "Value") %>% 
  rename("wbcode" = "Country.Code",
         "year" = "Year") %>% 
  select(wbcode, age_dependency_ratio, year) 
  
#importing age group as share of population data
ag_data<-read.csv("population-age-group-as-of-total-population/Population (age group as % of total population).csv") %>% 
  rename("wbcode" = "Country.Code",
         "year" = "Year") %>% 
  select(-Indicator.Code, -Disaggregation, -Country.Name) %>% 
  pivot_wider(names_from = Indicator.Name, values_from = Value) %>% 
  mutate(children_dr = (`Population ages 0-14 (% of total population)`/`Population ages 15-64 (% of total population)`)*100,
         elders_dr = (`Population ages 65 and above (% of total population)`/`Population ages 15-64 (% of total population)`)*100)
##"Population ages 0-14 (% of total population)"  likely most interesting
str(ag_data)



