#######################
# DESCRIPTIVE STATISTICS
#####################
# descriptive statistics on default spells
## number of defaults in original data
length(unique(sd_data$`Case nr`))

## number of defaults after dropping defaults with missing GDP
length(unique(default_panel_outcomes$Case))

## number of default spells
nrow(unique(defaults[c("country", "default_time")])) # there were 95 before dropping war episodes

## mean, median, min and max length of default spells
### Identify the start and end of default episodes
default_episodes <- default_panel_outcomes %>%
  mutate(prev_defaulting = lag(defaulting, default = 0),  # Previous value of 'defaulting'
         next_defaulting = lead(defaulting, default = 0)) %>%  # Next value of 'defaulting'
  filter(prev_defaulting == 0 & defaulting == 1 | defaulting == 1 & next_defaulting == 0) %>%
  mutate(default_episode_id = cumsum(prev_defaulting == 0 & defaulting == 1)) %>%
  group_by(country, default_episode_id) %>%
  summarise(default_time = min(year),
            end_year = max(year),
            length = max(year) - min(year) + 1) %>% 
  right_join(defaults, by = c("country", "default_time"))

## mean, median, min and max default episode duration
summary(default_episodes$length)

## country with  longest default episode
default_episodes %>% 
  arrange(desc(length))

## country with shortest default episode
default_episodes %>% 
  arrange((length))

# are defaults getting shorter?
cor(default_episodes$default_time, default_episodes$length)
plot(default_episodes$default_time, default_episodes$length)

## number of defaulting countries
length(unique(defaults$country))

## years in dataset
summary(defaults$default_time)

## last country to default
defaults$country[which.max(defaults$default_time)]
max(defaults$default_time)

## first country to default
defaults$country[which.min(defaults$default_time)]
min(defaults$default_time)

## countries to default most often
defaults %>% 
  group_by(country) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

# observations dropped after each step
##see above and 
nrow(unique(treatment_effects_GDP[c("country", "default_year")]))
nrow(unique(treatment_effect_IM[c("country", "default_year")]))
nrow(unique(treatment_effects_LE[c("country", "default_year")]))
nrow(unique(treatment_effects_pov[c("country", "default_year")]))
nrow(unique(treatment_effects_kcal[c("country", "default_year")]))

# observations after dropping 20% worst RMSE
nrow(unique(outcome_list_y5$gdp[c("country", "default_year")]))+nrow(unique(outcome_list_y5$pov[c("country", "default_year")]))+nrow(unique(outcome_list_y5$LE[c("country", "default_year")]))+nrow(unique(outcome_list_y5$IM[c("country", "default_year")]))+nrow(unique(outcome_list_y5$kcal[c("country", "default_year")]))

# descriptive statistics on outcome variables
# distribution of countries over outcome variables

## graphs on treatment effect (if they look presentable)

## distribution of outcome variables (summary to make sense of heterogeneity)
### dist GDP
summary(outcome_list_y5$gdp$outcome)
sd(outcome_list_y5$gdp$outcome)
var(outcome_list_y5$gdp$outcome)

### dist LE
summary(outcome_list_y5$LE$outcome) 
sd(outcome_list_y5$LE$outcome)
var(outcome_list_y5$LE$outcome)

### dist IM
summary(outcome_list_y5$IM$outcome)
sd(outcome_list_y5$IM$outcome)
var(outcome_list_y5$IM$outcome)

### dist kcal
summary(outcome_list_y5$kcal$outcome) 
sd(outcome_list_y5$kcal$outcome)
var(outcome_list_y5$kcal$outcome)

### dist pov
summary(outcome_list_y5$pov$outcome) 
sd(outcome_list_y5$pov$outcome)
var(outcome_list_y5$pov$outcome)

## histogram of outcome variable
hist(outcome_list_y5$kcal$outcome)
summary(outcome_list_y5$gdp$outcome)


# descriptive statistics on moderators
## distribution of moderator values
### dist polity
summary(outcome_list_y5$gdp$polity2)
hist(outcome_list_y5$gdp$yr_sch)

#### country with highest polity
outcome_list_y5$gdp[which.max(outcome_list_y5$gdp$polity2),]

#### country with lowest polity
outcome_list_y5$gdp[which.min(outcome_list_y5$gdp$polity2),]

### dist demography
summary(outcome_list_y5$gdp$age_dependency_ratio)

#### country with highest age dependency ratio
outcome_list_y5$gdp[which.max(outcome_list_y5$gdp$age_dependency_ratio),]

#### country with lowest age dependency ratio
outcome_list_y5$gdp[which.min(outcome_list_y5$gdp$age_dependency_ratio),]

### dist human capital
summary(outcome_list_y5$gdp$yr_sch)

#### country with highest HC
outcome_list_y5$gdp[which.max(outcome_list_y5$gdp$yr_sch),]

#### country with lowest HC
outcome_list_y5$gdp[which.min(outcome_list_y5$gdp$yr_sch),]

### countries that are NA
outcome_list_y5$gdp[which(is.na(outcome_list_y5$gdp$yr_sch)),]


# averages for outcome variables in year before default

## average GDP
GDP_avg_data<-treatment_effects_GDP %>% 
  filter(t == 0) %>% 
  mutate(year_lag =default_year-1 ) %>% 
  left_join(default_panel_outcomes %>% select(country, year, gdppc), by = c("year_lag" = "year", "country" = "country"))
mean_gdp_t1<-mean(GDP_avg_data$gdppc)

## average Infant mortality
Inf_avg_data<-treatment_effect_IM %>% 
  filter(t == 0) %>% 
  mutate(year_lag =default_year-1 ) %>% 
  left_join(default_panel_outcomes %>% select(country, year, Infant_mortality_rate), by = c("year_lag" = "year", "country" = "country"))
mean_IM_t1<-mean(Inf_avg_data$Infant_mortality_rate)

## average life expectancy
LE_avg_data<-treatment_effects_LE %>% 
  filter(t == 0) %>% 
  mutate(year_lag =default_year-1 ) %>% 
  left_join(default_panel_outcomes %>% select(country, year, Life_expectancy), by = c("year_lag" = "year", "country" = "country"))
mean_LE_t1<-mean(LE_avg_data$Life_expectancy)

## average poverty headcount
pov_avg_data<-treatment_effects_pov %>% 
  filter(t == 0) %>% 
  mutate(year_lag =default_year-1 ) %>% 
  left_join(default_panel_outcomes %>% select(country, year, poverty_headcount), by = c("year_lag" = "year", "country" = "country"))
mean_pov_t1<-mean(pov_avg_data$poverty_headcount)

## average calorie supply
kcal_avg_data<-treatment_effects_kcal %>% 
  filter(t == 0) %>% 
  mutate(year_lag =default_year-1 ) %>% 
  left_join(default_panel_outcomes %>% select(country, year, kcal_per_head_per_day), by = c("year_lag" = "year", "country" = "country"))
mean_kcal_t1<-mean(kcal_avg_data$kcal_per_head_per_day)

# default treatment effects in original units
## GDP (constant prices at 2011 USD)
(mean(outcome_list_y5$gdp$outcome)/100)*mean_gdp_t1

## LE (in months)
((mean(outcome_list_y5$LE$outcome)/100)*mean_LE_t1)*12 # 9 months lower life expectancy

## IM
(mean(outcome_list_y5$IM$outcome)/100)*mean_IM_t1

## pov
(mean(outcome_list_y5$pov$outcome)/100)*mean_pov_t1 # an additional 4.6% of the population living under poverty line

## kcal
(mean(outcome_list_y5$kcal$outcome)/100)*mean_kcal_t1





