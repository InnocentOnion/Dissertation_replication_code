###########################
#SHEET WITH ALL CUSTOM FUNCTIONS
##########################

#loading essential libraries
library(dplyr)
library(tidysynth)
library(Metrics)
library(pracma)
#####################
#FUNCTION TO CALCULATE MODE
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#FUNCTION TO CALCULATE AND VISUALISE MODELS
model_visualise <- function(outcome_list, 
                            var1 = NULL, var2 = NULL, var3 = NULL, var4 = NULL, var5 = NULL,
                            var6 = NULL, var7 = NULL, var8 = NULL, var9 = NULL, var10 = NULL,
                            var11 = NULL, var12 = NULL, var13 = NULL, var14 = NULL, var15 = NULL,
                            var16 = NULL, var17 = NULL, var18 = NULL, var19 = NULL, var20 = NULL,
                            var21 = NULL, var22 = NULL, var23 = NULL, var24 = NULL, var25 = NULL, type = "latex") {
  model_list <- list()
  
  # Generate models and store them in the list
  for (name in names(outcome_list)) {
    d <- outcome_list[[name]]
    vars <- c(var1, var2, var3, var4, var5, var6, var7, var8, var9, var10,
              var11, var12, var13, var14, var15, var16, var17, var18, var19, var20,
              var21, var22, var23, var24, var25)
    vars <- vars[!is.null(vars)]
    formula <- as.formula(paste("outcome ~", paste(vars, collapse = "+")))
    model <- lm(formula, data = d, na.action = "na.omit")
    
    model_name <- paste("model", name, sep = "_")
    
    # Assign the model to the list
    model_list[[model_name]] <- model
  }
  
  stargazer::stargazer(model_list, type = type)
}

#########################
#FUNCTION TO CREATE A LIST OF OUTCOME DATASETS

outcome_list_generator<-function(TE_list, moderators_list, criterion, slice=0.8, AUC = FALSE, vars, y=10){
  outcome_list<-list()
  
  if (AUC){
    for (name in names(TE_list)) {
      d <- TE_list[[name]]  # Get the dataframe from TE_list
      
      # Perform two calculations on the dataframe
      modified_data <- outcome_generator_auc(d, slice = slice, y=y)
      modified_data <- moderators_add(modified_data, moderators_list)
      
      #outcome_name <- paste0("outcome_", name)
      #modified_data<-modified_data %>% 
       # rename(!!outcome_name := outcome)
      
      # create dummy vars
      for (var in vars) {
        # Calculate median of the variable
        median_val <- median(modified_data[[var]], na.rm = TRUE)
        
        # Create dummy variable column
        dummy_col <- paste0(var, "_dummy")
        modified_data[dummy_col] <- ifelse(modified_data[[var]] > median_val, 1, 0)
      }
      
      # Store the modified dataframe in outcome_list under the same name
      outcome_list[[name]] <- modified_data
      
    }}
  else{
    for (name in names(TE_list)) {
      d <- TE_list[[name]]  # Get the dataframe from TE_list
      
      # Perform two calculations on the dataframe
      modified_data <- outcome_generator_var(d, criterion = criterion, slice = slice)
      modified_data <- moderators_add(modified_data, moderators_list)
      
      #outcome_name <- paste0("outcome_", name)
      #modified_data<-modified_data %>% 
      # rename(!!outcome_name := outcome)
      
      # create dummy vars
      for (var in vars) {
        # Calculate median of the variable
        median_val <- median(modified_data[[var]], na.rm = TRUE)
        
        # Create dummy variable column
        dummy_col <- paste0(var, "_dummy")
        modified_data[dummy_col] <- ifelse(modified_data[[var]] > median_val, 1, 0)
      }
      
      # Store the modified dataframe in outcome_list under the same name
      outcome_list[[name]] <- modified_data
    }
  }
  return(outcome_list)
}

#FUNCTION TO ADD MODERATORS TO OUTCOME VARIABLE
moderators_add<- function(outcome_df, predictors){
  
  output_data<-outcome_df
  
  for (p in predictors){
    
    closest_year <- outcome_df %>% # finding the year in the predictor data that is closest to the year PRIOR todefault year
      mutate(closest_year = sapply(default_year-1, function(x) {
        filtered_years <- p$year[p$year < x]  # Filter years less than year PRIOR to default_year
        if (length(filtered_years) == 0) {
          closest_year <- NA  # If no years are found, set closest_year to NA
        } else {
          closest_year <- max(filtered_years)  # Find the maximum of filtered years
        }
        return(closest_year)
      })) %>%
      ungroup() %>% 
      select(default_year, wbcode, closest_year)
    
    merged_data <- {
      closest_year %>% 
        left_join( p, by = c("wbcode", "closest_year" = "year"))
    }
   
     #extract the name of the object p accessed 
    for (i in seq_along(moderators_list)) {
      if (identical(p, moderators_list[[i]])) {
        predictor_name <- names(moderators_list)[i]
        col_name <- paste0("closest_year_", predictor_name)
        
      }
    }
    
    output_data<-output_data %>% 
      left_join(merged_data, by = c("wbcode", "default_year")) %>%
      rename(!!col_name := closest_year)
  }
  
  return(output_data)
}




#####################
#FUNCTION TO GENERATE OUTCOME VARIABLE

#GENERAL OUTCOME GENERATOR FUNCTION
outcome_generator_var<-function(treatment_df, criterion, slice = 0.8){
  
  # generate dataframe of unique default episodes and their rmse
  outcome_df<-unique(treatment_df[, c("country", "default_year", "rmse")]) %>% 
    slice_min(rmse, prop = slice) # drop slice% highest RMSE

  
  
  help_df<-treatment_df %>% # help df to apply criterion to treatment effects of default episodes
    filter(t>0) %>% 
    group_by(country, default_year)%>%
    summarise(outcome= criterion(treatment_effect))
  
  outcome_df<-left_join(outcome_df, help_df, by =c("country", "default_year")) 
  # merging help df and initial unique df
  
  outcome_df$wbcode<-countrycode(outcome_df$country, origin = "country.name", destination = "wb")
  
  return(outcome_df)
}


#AUC OUTCOME GENERATOR FUNCTION
outcome_generator_auc<-function(treatment_df, slice = 0.8, y =10){
  
  # generate dataframe of unique default episodes and their rmse
  outcome_df<-unique(treatment_df[, c("country", "default_year", "rmse")])%>% 
    slice_min(rmse, prop = slice) # drop slice% highest RMSE

  
  
  help_df<-treatment_df %>% # help df to apply criterion to treatment effects of default episodes
    filter(t>0 & t<=y) %>% 
    group_by(country, default_year)%>%
    summarise(outcome= trapz(treatment_effect, t))
  
  outcome_df<-left_join(outcome_df, help_df, by =join_by(country, default_year)) 
  # merging help df and initial unique df
  
  outcome_df$wbcode<-countrycode(outcome_df$country, origin = "country.name", destination = "wb")
  
  return(outcome_df)
}





#####################
#SET UP AND FUNCTIONS TO GENERATE SC

#Creating list of defaults
#EXCLUDING WARS
defaults <- data.frame(country = character(), default_time = numeric(), stringsAsFactors = FALSE)
for (current_country in na.omit(unique(default_panel_outcomes$country))) {
  
  # Subset data for the current country
  country_data <- default_panel_outcomes %>%
    filter(country == current_country)
  
  # Iterate through each year for the current country
  for (i in 2:(nrow(country_data))) {
    # Check for the switch from 0 to 1 in the "defaulting" variable
    if (country_data$defaulting[i - 1] == 0 && country_data$defaulting[i] == 1) {
      if (!any(country_data$war[i:(i + 5)] == 1, na.rm = TRUE)) { # exclude defaults that coincide with wars or wars during next 5 years
        # Add the country and year to the "defaults" dataframe
        defaults <- rbind(defaults, c(current_country, country_data$year[i]))
      }
    }
  }
}
colnames(defaults) <- c("country", "default_time") # changing col names
defaults$default_time<-as.numeric(defaults$default_time) # changing year to be numeric


###########################
#Creating a function to calculate TE by variable

TE_generator<-function(outcome_var){ 
  #outcome_var = the desired outcome 
  treatment_effects<-data.frame(t = numeric(), # creating the dataframe to be populated
                                country = character(),
                                default_year = numeric(),
                                treatment_effect = numeric())
  
  #Loop to calculate treatment effects
  for (i in c(1:nrow(defaults))){
    
    treated_unit = defaults$country[i]
    default_time = defaults$default_time[i]
    
    if(outcome_var != "gdppc"){
      country_data <- default_panel_outcomes %>%
        group_by(country) %>%
        filter(all(!is.na(!!sym(outcome_var)))) %>%
        ungroup()} else(country_data<-default_panel_outcomes)
    
    if(!(treated_unit %in% country_data$country)){
      next
    }
    
    #call fucntion to generate donor pool
    fdp<-donor_pool_generator(treated_unit, default_time, country_data = country_data)
    
    if(length(unique(fdp$country))<3 | (treated_unit %in% fdp$country) == FALSE){
      next #skip this country if there are not enough units in donor pool OR
      #if there is no data for the treated unit (i.e. it is not in donor pool)
    }
    
    #scaling the outcome variable to 100 in year prior to default
    fdp <- fdp %>% 
      mutate(outcome_var_scaled = (!!sym(outcome_var) / fdp[[outcome_var]][country == !!treated_unit & year == !!default_time-1]) * 100)
    
    
    time_window_start = default_time-10
    #optimization_window_start = default_time-10 # do not need this now
    
    
    synth_results <- fdp %>%
      
      synthetic_control(
        outcome = outcome_var_scaled,
        unit = country,
        time = year,
        i_unit = treated_unit,
        i_time = default_time, 
        generate_placebos = FALSE 
      ) %>%
      generate_predictor(time_window = seq(time_window_start, default_time), 
                         GDP = mean(gdppc) 
                         ,outcome = mean(outcome_var_scaled)
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
      filter(t >= -10 & t<=10)
    
    # calculating RMSE
    outcomes_transformed<-outcomes_transformed %>%
      mutate(rmse = rmse(outcomes_transformed$real_y[outcomes_transformed$t < 0], outcomes_transformed$synth_y[outcomes_transformed$t < 0])) %>% 
      select(-real_y, -synth_y, - time_unit)
    
    treatment_effects <- treatment_effects %>%
      rbind(outcomes_transformed)
    
  }
  return(treatment_effects)
}

#####################
#DONOR POOL RESTRICTION
###How to restrict donor pool
#Upper Bound: gdppc_treated_unit + 0.25 * (Q3 - Q1)
#Lower Bound: gdppc_treated_unit - 0.25 * (Q3 - Q1)

#function to restrict donor pool
donor_pool_generator<-function(treated_unit, default_time, country_data){

  #find gdppc for treated unit 4 years before default
  treated_gdppc<- country_data %>%
    filter(country == treated_unit, 
           year == default_time - 4) %>% 
    pull(gdppc)
  
  #find polity2 for treated unit 4 years before default
  treated_polity<-country_data %>%
    filter(country == treated_unit, 
           year == default_time - 4) %>% 
    pull(polity2)
  
  #filter default_panel_outcomes based on GDP values
  #filter that have missing values for gdppc or polity
  step1_donor_pool <- country_data %>%
    filter(year %in% (default_time - 10):default_time) %>%
    group_by(country) %>%
    filter(all(!is.na(gdppc)) & all(!is.na(polity2))) %>%
    ungroup()
  
  # Step 2: Exclude countries that have defaulted in the 10 years leading up to default
  step2_donor_pool <- step1_donor_pool %>%
    group_by(country) %>%
    filter(all(defaulting != 1) | country == !!treated_unit)
  
  # Step 3: Filter based on treated_gdppc and treated_polity
  donor_pool <- step2_donor_pool %>%
    filter(year == (default_time - 4),
           gdppc < treated_gdppc + 0.5 * IQR(default_panel_outcomes$gdppc, na.rm = TRUE) & 
             gdppc > treated_gdppc - 0.5 * IQR(default_panel_outcomes$gdppc, na.rm = TRUE),
           polity2 < treated_polity + 0.5 * IQR(default_panel_outcomes$polity2, na.rm = TRUE) & 
             polity2 > treated_polity - 0.5 * IQR(default_panel_outcomes$polity2, na.rm = TRUE))
  
  final_donor_pool<-default_panel_outcomes %>% 
    filter(country %in% donor_pool$country)
 return(final_donor_pool)
}

#Year observations for Russia, Serbia, Bosnia in early 1990s are lost because 
#they have no polity score before -> were soviet union/yugoslavia

#Q1 and Q3 of the whole dist
#Q1-Q3 is 50% of the data
###################
