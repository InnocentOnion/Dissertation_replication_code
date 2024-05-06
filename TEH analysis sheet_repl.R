##################
#Treatment effect heterogeneity analysis sheet
##################
library(stargazer)
########
#adding moderators to outcome variables
# creating moderators list
moderators_list<-list(hc = hc_data, 
                      ag = ag_data, 
                      agdr= agdr_data,
                      vdem = vdem_data,
                      cnts = cnts_data,
                      gov_exp = gov_exp_data,
                      polity = polity_dataP5,
                      maddison = maddison_gdp,
                      stc = stc_data
                      )

# creating a list of outcome variables
TE_list<- list(gdp = treatment_effects_GDP,
               IM = treatment_effects_IM,
               pov = treatment_effects_pov,
               LE = treatment_effects_LE,
               kcal = treatment_effects_kcal)

# populating a list with the full data sets for analysis
vars = c("age_dependency_ratio", 
         "polity2", 
         "yr_sch")



# testing different versions of the outcome variable
outcome_list_median<-outcome_list_generator(TE_list, moderators_list, criterion = median, slice = 0.8, vars = vars)
outcome_list_y5<-outcome_list_generator(TE_list, moderators_list, criterion = function(x) ifelse(length(x)<5, x[length(x)], x[5]), slice = 0.8, vars = vars)
outcome_list_y10<-outcome_list_generator(TE_list, moderators_list, criterion = function(x) x[length(x)], slice = 0.8, vars = vars)
outcome_list_auc5<-outcome_list_generator(TE_list, moderators_list, criterion = median, slice = 0.8, vars = vars, AUC =TRUE, y =5)
outcome_list_auc10<-outcome_list_generator(TE_list, moderators_list, criterion = median, slice = 0.8, vars = vars, AUC =TRUE, y =10)

model_visualise(outcome_list_y5, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy"
                )

model_visualise(outcome_list_y10, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy")

model_visualise(outcome_list_auc5, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy")


# investigating correlations for different outcome measures
## all measures are correlated well
## except AUC for 10 years with y5 outcome and median outcome for GDP
cor(data.frame(outcome_list_auc5$gdp$outcome,
               outcome_list_median$gdp$outcome, 
               outcome_list_y5$gdp$outcome, 
               outcome_list_y10$gdp$outcome,
               outcome_list_auc10$gdp$outcome))

cor(data.frame(outcome_list_auc5$LE$outcome,
               outcome_list_median$LE$outcome, 
               outcome_list_y5$LE$outcome, 
               outcome_list_y10$LE$outcome,
               outcome_list_auc10$LE$outcome))

cor(data.frame(outcome_list_auc5$IM$outcome,
               outcome_list_median$IM$outcome, 
               outcome_list_y5$IM$outcome, 
               outcome_list_y10$IM$outcome,
               outcome_list_auc10$IM$outcome))


## investigating correlations for moderators
polit_vars<-outcome_list_median$gdp %>% 
  select(polity2, v2x_corr, avg_exec_change, v2csprtcpt)
cor(polit_vars)


# investigating the channels through which variables impact outcomes

## expanding the range of vars
vars_exp = c("age_dependency_ratio", 
         "Population ages 0-14 (% of total population)",
         "Population ages 65 and above (% of total population)",
         "elders_dr",
         "children_dr",
         "Population ages 15-64 (% of total population)",
         "gdppc",
         "polity2",
         "yr_sch",
         "yr_sch_ter",
         "v2psparban",
         "v2x_jucon"
         
)


## generating a list of outcome dataframes with expanded number of variables
outcome_list_y5_exp<-outcome_list_generator(TE_list, 
                                            moderators_list, 
                                            criterion = function(x) ifelse(length(x)<5, x[length(x)], x[5]), 
                                            slice = 0.8, 
                                            vars = vars_exp)

## specification with gdp
### gdppc changes the results, but not the order of magnitude
### human capital is not significant in explaining LE outcome anymore, but gdppc is
model_visualise(outcome_list_y5_exp, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy", 
                "gdppc_dummy")

## additional specification on age dependency ratio
model_visualise(outcome_list_y5_exp,
                "children_dr_dummy",
                "polity2_dummy",
                "yr_sch_dummy")

model_visualise(outcome_list_y5_exp, 
                "elders_dr_dummy",
                "polity2_dummy",
                "yr_sch_dummy")


## investigating the impact of institutional features
institutions_model_visualise<-function(data){
  model1<- lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy+exec_change_dummy, data = data)
  model2<- lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy+reg_change_dummy, data = data)
  model3<- lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy+v2xcl_prpty_dummy, data = data)
  model4<- lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy+v2psparban_dummy+v2x_jucon_dummy, data = data)
  #model5<- lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy+v2x_corr, data = data)
  
  stargazer(model1, 
            model2,
            model3,
            model4, 
            #model5,
            type = "latex")
}
institutions_model_visualise(outcome_list_y5_exp$LE)


## human capital measure
### yr_sch_ter significant even when accounting for GDP
model_visualise(outcome_list_y5_exp, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "gdppc_dummy",
                "yr_sch_ter_dummy")



               



