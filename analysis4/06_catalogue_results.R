#This script takes a folder of R models and generates a table showing the model predictors,
#organized by predictor category, the R2 and the AIC. This is where we rank the models

# Note the NF models under consideration are in a folder called NF_mods_with_warm_dry
# while the wilderness models under consideration are in a folder called mods

source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(sf)
library(sp)

#pred vars
var_categories <- c("topography", "fire_history", "historical_climate", "post_fire_weather","recovery_time")

topoVars <- tibble(category = var_categories[1], var = c("hli","tpi","adjustedNorthness","adj_fold_northness","elevation"))
fireHistory <- tibble(category = var_categories[2], var = c("burnSev","mediumReburns","lowReburns","postFireSAP"))
histClimateVars <-  tibble(category = var_categories[3], var = c("AEThist","CWDhist","TmaxHist","PPThist"))
postFireWeatherVars <-  tibble(category = var_categories[4], var = c("postFirePrecip","ppt_Yr1_3_mean_annual_anom", "pptYr1_3_annual_mean_t_adjusted","minPostFirePrecip", "min_annual_ppt_Yr1_3_anom","min_annual_ppt_t_adjusted_Yr1_3","postFireMaxT"))

timeVar <- tibble(category = var_categories[5], var = "recoveryTime")
predictor_vars <- rbind(topoVars,fireHistory,histClimateVars,postFireWeatherVars,timeVar)

# Make table of model formulas, AIC, and R2
get_formula <- function(path_to_mod){
  load(path_to_mod)
  return(mod$formula)
}

get_vars_in_formula <- function(path_to_mod){
  load(path_to_mod)
  form <- mod$formula
  term_labels <- paste(labels(terms(form)),collapse = "")
  included_vars <- c()
  for(v in predictor_vars$var){
    if(grepl(pattern = v,x = term_labels)){
      included_vars <- c(included_vars,v)
    }
  }
  return(paste(sort(included_vars),collapse = ","))
}


get_vars_in_formula <- function(path_to_mod){
  out <- tibble(i = 1)
  load(path_to_mod)
  form <- mod$formula
  term_labels <- paste(labels(terms(form)),collapse = "")
  print(term_labels)
  pred_cats <- unique(predictor_vars$category)[-5]
  for(c in pred_cats){
    predVars <- predictor_vars %>% filter(category == c) %>% pull(var)
    included_vars <- c()
    for(v in predVars){
      if(grepl(pattern = v,x = term_labels)){
        included_vars <- c(included_vars,v)
        #print(paste(c,v,included_vars))
      }
    }
    included_vars <- paste(sort(included_vars),collapse = ",")
    tmp <- tibble(!!c := included_vars)
    out <- cbind(out,tmp)
  }
  return(out)
}

get_vars_in_formula <- function(path_to_mod){
  out <- tibble(i = 1)
  load(path_to_mod)
  form <- mod$formula
  term_labels <- get_term_labels(path_to_mod)
  pred_cats <- unique(predictor_vars$category)[-5]
  for(c in pred_cats){
    predVars <- predictor_vars %>% filter(category == c) %>% pull(var)
    included_vars <- c()
    for(v in predVars){
      for(t in term_labels){
        interaction <- FALSE
        if(grepl(pattern = "te",x = t)){
         interaction <- TRUE 
        }else{
          interaction <- FALSE
        }
        if(grepl(pattern = v,x = t) & interaction == FALSE){
          included_vars <- c(included_vars,v)
        }
        if(grepl(pattern = v,x = t) & interaction == TRUE){
          iv <- paste0("i_",v)
          included_vars <- c(included_vars,iv)
        }
      }
    }
    included_vars <- paste(sort(included_vars),collapse = ",")
    tmp <- tibble(!!c := included_vars)
    out <- cbind(out,tmp)
  }
  return(out)
}


get_AIC <- function(path_to_mod){
  load(path_to_mod)
  summry <- summary(mod)
  return(c(round(as.numeric(AIC(mod)),2),round(as.numeric(summry$r.sq),2)))
}

get_domain <- function(path_to_mod){
  domains <- c("nf","wild")
  for(d in domains){
    if(grepl(pattern = d,x = path_to_mod)){
      out <- tibble(domain = d,mod_name = basename(path_to_mod))
    }
  }
  return(out)
}

get_line_of_mod_table <- function(path_to_mod){
  domain_tib <- get_domain(path_to_mod)
  aic_tib <- tibble(aic = get_AIC(path_to_mod)[1], rsq = get_AIC(path_to_mod)[2])
  var_mod <- get_vars_in_formula(path_to_mod)
  out <- cbind(domain_tib,aic_tib,var_mod) 
  return(out %>% dplyr::select(-i))
}


get_term_labels <- function(path_to_mod){
  load(path_to_mod)
  term_labels <- labels(terms(mod$formula))
  return(term_labels)
}








#get table of mods in the NF domain

paths <- paste0("analysis4/NF_mods_with_warm_dry/",list.files("analysis4/NF_mods_with_warm_dry/"))

table_of_mods <- tibble()
for(f in paths){
  table_of_mods <- rbind(table_of_mods,get_line_of_mod_table(f))
} 

table_of_mods <- table_of_mods %>% arrange(domain,aic)

top_mod_aic <- min(table_of_mods$aic)

table_of_mods <- table_of_mods %>% mutate(d_aic = round(aic - top_mod_aic,2),
                                          model_rank = row_number()) %>% dplyr::select(
                                            domain,mod_name,aic,model_rank,d_aic,rsq,
                                            topography,fire_history,historical_climate,post_fire_weather)


write_csv(table_of_mods,"analysis4/table_of_mods_052523.csv")




#get table of mods for the wild domain
paths <- paste0("analysis4/mods/",list.files("analysis4/mods",include.dirs = FALSE))

table_of_mods <- tibble()
for(f in paths){
  table_of_mods <- rbind(table_of_mods,get_line_of_mod_table(f))
} 

table_of_mods <- table_of_mods %>% filter(domain == "wild") %>% 
  arrange(domain,aic)

top_mod_aic <- min(table_of_mods$aic)

table_of_mods <- table_of_mods %>% mutate(d_aic = round(aic - top_mod_aic,2),
                                          model_rank = row_number()) %>% dplyr::select(
                                            domain,mod_name,aic,model_rank,d_aic,rsq,
                                            topography,fire_history,historical_climate,post_fire_weather)

write_csv(table_of_mods,"analysis4/table_of_mods_wild_052423.csv")

  