########################
#SHEET TO GENERATE TABLES AND APPENDIX FIGURES
########################
# Load required library
library(knitr)
################
#Tables
################
# Create a dataframe with the summary statistics
summary_data <- data.frame(
  Variable = c("polity2", "age_dependenc_ratio", "yr_sch"),
  Min = c(-9.000, 42.80, 0.667),
  `1st Qu.` = c(-7.000, 67.17, 2.665),
  Median = c(-5.000, 87.98, 5.008),
  Mean = c(-1.455, 81.89, 5.103),
  `3rd Qu.` = c(6.500, 93.88, 7.131),
  Max = c(10.000, 106.05, 11.533),
  `NA's` = c("", "", 6)
)

# Print the table
kable(summary_data, align = "c", caption = "Summary Statistics")


# Create a dataframe with the summary statistics
summary_data <- data.frame(
  Variable = c("gdp", "LE", "IM", "kcal", "pov"),
  Min = c(-51.089, -11.4252, -17.616, -19.401, -53.911),
  `1st Qu.` = c(-29.510, -3.5811, -3.762, -5.868, -5.412),
  Median = c(-17.868, -0.2634, 5.898, -1.573, 8.186),
  Mean = c(-15.579, -1.2352, 4.094, -1.328, 18.025),
  `3rd Qu.` = c(-3.039, 1.4258, 10.246, 3.927, 33.912),
  Max = c(20.719, 7.0757, 30.493, 10.924, 155.304)
)

# Print the table
kable(summary_data, align = "c", caption = "Summary Statistics")

#######################
#Appendix
#######################
# Alternative measures of the treatment effect

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

model_visualise(outcome_list_median, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy")

# Including GDP per capita

model_visualise(outcome_list_y5_exp, "age_dependency_ratio_dummy", 
                "polity2_dummy",
                "yr_sch_dummy", 
                "gdppc_dummy")

# Specification without human capital 

model_visualise(outcome_list_y5_exp, "age_dependency_ratio_dummy", 
                "polity2_dummy")

# regression results with robust standard errors

## robust standard errors generator
library(sandwich)

robust_se_fun<-function(data){
  model<-lm(outcome~polity2_dummy+yr_sch_dummy+age_dependency_ratio_dummy, data = data)
  robust_se <- sqrt(diag(vcovHC(model, type = "HC0")))
  coefficients <- coef(model)
  results <- cbind(coefficients, robust_se, t_values = coefficients / robust_se, p_values = 2 * pt(-abs(coefficients / robust_se), df = df.residual(model)))
  return(results)
}
robust_se_fun(outcome_list_y5$gdp)
robust_se_fun(outcome_list_y5$IM)
robust_se_fun(outcome_list_y5$LE)
robust_se_fun(outcome_list_y5$kcal)
robust_se_fun(outcome_list_y5$pov)

results <- data.frame(
  Outcome_Variable = c("GDP", "GDP", "GDP", "GDP",
                       "IM", "IM", "IM", "IM",
                       "LE", "LE", "LE", "LE",
                       "kcal", "kcal", "kcal", "kcal",
                       "pov", "pov", "pov", "pov"),
  Predictor = c("(Intercept)", "polity2_dummy", "yr_sch_dummy", "age_dependency_ratio_dummy",
                "(Intercept)", "polity2_dummy", "yr_sch_dummy", "age_dependency_ratio_dummy",
                "(Intercept)", "polity2_dummy", "yr_sch_dummy", "age_dependency_ratio_dummy",
                "(Intercept)", "polity2_dummy", "yr_sch_dummy", "age_dependency_ratio_dummy",
                "(Intercept)", "polity2_dummy", "yr_sch_dummy", "age_dependency_ratio_dummy"),
  Coefficients = c(-0.442, -15.699, -2.231, -14.140,
                   4.000, -5.630, -1.078, 5.327,
                   -4.101, 2.513, 2.558, 1.360,
                   -1.806, -0.563, -0.197, 0.472,
                   -1.184, -9.966, 32.906, 20.712),
  Robust_SE = c(3.767, 4.200, 3.951, 3.496,
                2.403, 2.602, 2.585, 2.516,
                1.146, 0.911, 0.935, 0.917,
                2.769, 2.160, 2.345, 2.354,
                16.793, 20.931, 25.344, 18.734),
  t_Values = c(-0.117, -3.738, -0.565, -4.044,
               1.664, -2.164, -0.417, 2.117,
               -3.580, 2.760, 2.736, 1.482,
               -0.652, -0.261, -0.084, 0.201,
               -0.070, -0.476, 1.298, 1.106),
  p_Values = c(0.907, 0.001, 0.575, 0.000,
               0.104, 0.036, 0.679, 0.040,
               0.001, 0.008, 0.009, 0.145,
               0.518, 0.795, 0.933, 0.842,
               0.944, 0.638, 0.204, 0.278)
)

# Display the results using kable from the knitr package
knitr::kable(results, align = "cccccc", caption = "Regression Results")



