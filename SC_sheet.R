############################
#SHEET TO CALCULATE SYNTHETIC CONTROLS
###########################


#generating treatment effects for outcome variables
treatment_effects_GDP<-TE_generator("gdppc")#uses GDP as predictor twice - does NOT influence results
treatment_effects_LE<-TE_generator("Life_expectancy")
treatment_effects_kcal<-TE_generator("kcal_per_head_per_day")
treatment_effects_IM<-TE_generator("Infant_mortality_rate")

#generating treatment effects for poverty headcount
treatment_effects_pov<-data.frame(t = numeric(), # creating the dataframe to be populated
                                   country = character(),
                                   default_year = numeric(),
                                   treatment_effect = numeric(), 
                                  rmse = numeric())

#Loop to calculate treatment effects
for (i in c(1:nrow(defaults))){
  
  treated_unit = defaults$country[i]
  default_time = defaults$default_time[i]
  
  country_data<-default_panel_outcomes
  
  if(!(treated_unit %in% country_data$country)){
    next
  }
  if(default_time<1982){next}#ADDED THIS
  
  #call fucntion to generate donor pool
  fdp<-donor_pool_generator(treated_unit, default_time, country_data = country_data)
  
  fdp <- fdp %>% # added this
    filter(year > 1980) %>%
    group_by(country) %>%
    filter(all(!is.na(headcount)))  %>% #&& all(round(headcount, 5) != 0))
    ungroup()
  
    
  if(length(unique(fdp$country))<3 | (treated_unit %in% fdp$country) == FALSE){
    next #skip this country if there are not enough units in donor pool OR
    #if there is no data for the treated unit (i.e. it is not in donor pool)
  }
  
  # scaling poverty headcount to 100 in year prior to default

  #fdp <- fdp %>% 
    #mutate(pov_head_scaled = (poverty_headcount /fdp$poverty_headcount[country == !!treated_unit & year == !!default_time-1]) * 100)

  
  time_window_start = default_time-7#CHANGED

  
  
  synth_results <- fdp %>%
    
    synthetic_control(
      outcome = headcount,
      unit = country,
      time = year,
      i_unit = treated_unit,
      i_time = default_time, 
      generate_placebos = FALSE 
    ) %>%
    generate_predictor(time_window = seq(time_window_start, default_time), 
                       GDP = mean(gdppc),
                       outcome = mean(headcount)
    ) %>% # this function includes both GDP and outcome as predictor
    generate_weights() %>% #default for optimization window is whole pre-intervention period, so no argument used
    #argument before: generate_weights(optimization_window = optimization_window_start, default_time)
    generate_control() %>%
    grab_synthetic_control()
  
  outcomes_transformed <- synth_results %>%
    mutate(t = (synth_results$time_unit-default_time),
           treatment_effect = (real_y-synth_y), 
           country = !!treated_unit,
           default_year = !!default_time ) %>%
    #select(-real_y, -synth_y, - time_unit) %>%
    filter(t >= -7 & t<=10)#CHANGED
  
  
  outcomes_transformed<-outcomes_transformed %>% 
    mutate(real_y_scaled = (real_y/outcomes_transformed$real_y[outcomes_transformed$t == -1])*100,
           synth_y_scaled =(synth_y/outcomes_transformed$real_y[outcomes_transformed$t == -1])*100)
  # scaling synth_y with real y
    
  # calculating RMSE
  outcomes_transformed<-outcomes_transformed %>%
    mutate(treatment_effect = (real_y_scaled-synth_y_scaled),
           rmse = rmse(outcomes_transformed$real_y_scaled[outcomes_transformed$t <0], outcomes_transformed$synth_y_scaled[outcomes_transformed$t <0])) %>% 
    select(-real_y, -synth_y, - time_unit, -real_y_scaled, -synth_y_scaled)
  
  treatment_effects_pov <- treatment_effects_pov %>%
    rbind(outcomes_transformed)}


