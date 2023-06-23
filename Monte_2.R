#working directory ----

setwd(getwd())

#used packages ----
install.packages("DiagrammeR")

library(readr)
library(decisionSupport)
library(vctrs)
library(tidyverse)
library(DiagrammeR)

#Data Import ####
input_estimates_project <- read_delim("C:/Users/user/Desktop/Karriere/Agrar/Sweeter Swerv'/Im Studium/DECISION ANALYSIS - Forecasting and Modelling/AgriPV v.s. AgroForst/agripv-vs-agroforestry/input_estimates_project.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(input_estimates_project)
df_estimates <- as.data.frame(input_estimates_project)

input_estimates_project$lower <- as.numeric(input_estimates_project$lower)

#Time period 'n' for 25 years.
n <- 25

#Build function ####

agrivp_vs_agroforestry_function_two <- function(){
  
# ex-ante risks: impact the implementation of interventions ####
  
  #pv_int_event_no_involvement_by_population <-
  #chance_event(av_int_no_involvement_by_population, 1, 0, n = 1)
  
  # af_int_event_no_involvement_by_institution <-
  # chance_event(av_int_no_involvement_by_institution, 1, 0, n = 1)
  
  
#set variables with variation over a period of time with the vv_function is optional
  
#profits_both_systems ####
  
  yield_crop                      <- vv(var_mean=crop_Yield, var_CV, n)
  monetary_crop                   <- vv(var_mean=crop_monetary, var_CV, n)
  profit_photovoltaic             <- vv(var_mean=profit_pv, var_CV, n)
  profit_agroforestry             <- vv(var_mean=profit_af, var_CV, n)

  #costs_photovoltaic

  Investment_cost_photovoltaic    <- vv(var_mean=investment_cost_pv, var_CV, n)
  anual_cost_photovoltaic         <- vv(var_mean=anual_cost_pv, var_CV, n)
  yield_loss_photovoltaic         <- vv(var_mean=yield_loss_pv, var_CV, n)

  #costs_agroforestry

  investment_cost_agroforestry    <- vv(var_mean=investment_cost_af, var_CV, n)
  anual_cost_agroforestry         <- vv(var_mean=anual_cost_af, var_CV, n)
  yield_loss_agroforestry         <- vv(var_mean=yield_loss_af, var_CV, n)


# decision conventional farm vs photovoltaic or agroforestry #### 
  
  for (decision_af in c(FALSE, TRUE)) {
    
    if (decision_af){
      
      intervention_af_gain <- TRUE
      intervention_af_overall_costs <- TRUE
      intervention_af_risks <- TRUE 
    } else 
    {
      intervention_af_gain <- FALSE
      intervention_af_overall_costs <- FALSE
      intervention_af_risks <- FALSE  
    }
    
    # gain_af
    
    if (intervention_af_gain) {
      gain_af_intervention <- 
        subsidies_af+
        profit_af                # profit_af = wood_yield*wood_price 
    } else 
      gain_af_intervention <- 0
    
    # costs_af
    
    if (intervention_af_overall_costs) {
      costs_overall_af_intervention <-
        investment_cost_af +
        annual_cost_af 
    } else 
      costs_overall_af_intervention <- 0
  }
  
  # risks_af_(optional)
  
  
  for   (decision_pv in c(FALSE, TRUE)) {

    if (decision_pv){

      intervention_pv_gain <- TRUE
      intervention_pv_overall_costs <- TRUE
      intervention_pv_risks <- TRUE
    } else
    {
      intervention_pv_gain <- FALSE
      intervention_pv_overall_costs <- FALSE
      intervention_pv_risks <- FALSE
    }

  }


  # calculation gain, costs, risks

  # gain_pv
  #  if (intervention_pv_gain) {
  #      gain_intervention_pv <-
  #      solar_radiaton_nrw *
  #      EEG_price
  #     } else
  #       gain_intervention_pv <- 0


  # gain_pv
  if (intervention_pv_gain) {
    gain_intervention_pv <-
      profit_pv

  } else
    gain_intervention_pv <- 0

  # costs_pv
  if (intervention_pv_overall_costs) {
    costs_intervention_pv_overall_costs <-
      investment_cost_pv +
      annual_cost_pv
  } else {
    costs_intervention_pv_overall_costs <- 0
  }

  
    
  
  
  NPV_af <- discount(gain_af_intervention, discount, calculate_NPV = TRUE)
  NPV_af_int <- discount(costs_overall_af_intervention, discount, calculate_NPV = TRUE)

  NPV_pv <- discount(gain_intervention_pv, discount, calculate_NPV = TRUE)
  NPV_pv_int <- discount( costs_intervention_pv_overall_costs, discount, calculate_NPV = TRUE)

  Cashflow_decision_af = gain_af_intervention -  costs_overall_af_intervention
  Cashflow_decision_pv = gain_intervention_pv - costs_intervention_pv_overall_costs
  
return(list(af_NPV = NPV_af,
            int_af_NPV = NPV_pv_int,
            pv_NPV = NPV_pv,
            int_pv_NPV = NPV_pv_int,
            Cashflow_decision_af,
            Cashflow_decision_pv
             ))

 

} # <- Function bracket

# Monte ####


mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("input_estimates_project.csv"),
  model_function = agrivp_vs_agroforestry_function_two(),
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)


example_mc_simulation <- mcSimulation(estimate = as.estimate(df_estimates),
                                      model_function = agrivp_vs_agroforestry_function_two,
                                      numberOfModelRuns = 100,
                                      functionSyntax = "plainNames")
# plotting ####
result <- agrivp_vs_agroforestry_function_two()
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = c("Gain_photovoltaic", "Gain_agroforestry"),
                   method = 'smooth_simple_overlay',
                   base_size = 7)




vv(mean(input_estimates_project$upper[1]+ input_estimates_project$lower[1]), 
    +  9, 
    +  25, 
    +  input_estimates_project$lower[1], 
    +  input_estimates_project$upper[1] )
fuck_yall_r <- vv(mean(input_estimates_project$upper[1]+ input_estimates_project$lower[1]), 
                  +  9, 
                  +  25, 
                  +  input_estimates_project$lower[1], 
                  +  input_estimates_project$upper[1] )
