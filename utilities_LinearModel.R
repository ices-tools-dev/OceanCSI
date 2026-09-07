
install.packages("emmeans")
install.packages("Kendall")
library(emmeans)
library(Kendall)

linear_model_average<-function(df,level,indicator){
  
  if (indicator=="CHL"){
    logparam <- "logChlorophyll"
  }else if (indicator=="DIN"){
    logparam <- "logDIN"
  }else if (indicator=="DIP"){
    logparam <- "logPhosphate"
  }
  
  
  if (level ==0){
    formula_model <- paste0(logparam, " ~ Year + Month")
  }else if (level==1){
    formula_model <- paste0(logparam, " ~ Year")
  }
  
  if (level %in% c(0,1)){
    
    df <- base::split(df, by = "CellCode")
    
    df <- lapply(df, function(x) lm(as.formula(formula_model), data = x))
    
    df_emmeans <- lapply(df, function(model) emmeans(model, "Year", weights = "flat"))
    
    df_emmeans <- lapply(df_emmeans,as.data.frame)
    
    df_emmeans <- rbindlist(df_emmeans, idcol = "CellCode")
    
    df_emmeans[, Year := as.numeric(as.character(Year))]
    
    df_emmeans[, Value := exp(emmean)]# Calculate the indicator as exp(emmean) since we used log(indicator) to calculate emmeans
    
    df_emmeans[, type := "emmeans"]
    
    setnames(df_emmeans, old = "emmean", new = "avg")
    
    return(df_emmeans)
    
  }else{
    df <- base::split(df, by = "CellCode")
    
    df <- purrr::map(df, dplyr::group_by, Year)
    
    df <- purrr::map(df, dplyr::summarise, mean=mean(get(logparam),na.rm=T), SE=sd(get(logparam),na.rm=T)/sqrt(dplyr::n()),.groups="drop")
    
    df_means <- lapply(df,as.data.table)
    
    df_means <- rbindlist(df_means, idcol = c("CellCode"))
    
    df_means[, Year := as.numeric(as.character(Year))]
    
    df_means[, Value := exp(mean)]# Calculate the indicator as exp(emmean) since we used log(indicator) to calculate the mean
    
    df_means[, type := "arithmetic"]
    
    setnames(df_means, old = "mean", new = "avg")
    
    return(df_means)
  }
}

linear_model_trends<-function(df,indicator,nrYears){
  
  #apply now the linear model to both datasets
  lm_list<- split(df, by = "CellCode")
  
  lm_list <- lapply(lm_list, function(dt) lm(logValue ~ Year, data = dt))
  
  #Extract coefficients from each linear model in the list
  coef_list <- lapply(lm_list, function(model) as.data.frame(t(as.matrix(coef(model)))))
  
  # Combine all data frames in the list into a single data frame
  model <- rbindlist(coef_list, idcol = "CellCode")
  tidy_list <- lapply(lm_list, broom::tidy)
  
  # Combine all data frames in the list into a single data frame
  model_extended <- rbindlist(tidy_list, idcol = "CellCode")
  
  # Filter out rows where term is '(Intercept)'
  model_extended <- subset(model_extended, term != "(Intercept)")
  
  # Add a new column 'Parameter'
  model_extended <- transform(model_extended, Parameter = indicator)
  
  # Left join the two parts of the model by "CellCode"
  trend <- merge(model, model_extended, by = "CellCode", all = TRUE)
  
  # Rename Year column to slope
  setnames(trend, "Year", "slope")
  
  #Calculate robustness of the linear model
  trend[, robustness := ifelse(slope < 0 & p.value <= 0.05 | slope > 0 & p.value <= 0.05, "robust", "no robust")]
  
  #now we are going to check the model using MANN KENDALL function
  lm_list <- split(df, by = "CellCode")
  
  #make an empty df where we will put our data
  dummy_trend <- data.frame(tau = numeric(0), sl = numeric(0), S = numeric(0), D = numeric(0), varS = numeric(0),CellCode=character())
  
  # Iterate over both models, the pre and the post
  for (name in names(lm_list)) {
    # Filter out NA and NaN values
    lm_list[[name]] <- lm_list[[name]][!is.na(lm_list[[name]]$logValue) & !is.nan(lm_list[[name]]$logValue), ]
    # Apply MannKendall to each group
    dataset<- lm_list[[name]]$logValue
    
    if(length(dataset) < 3){
      result<-list(tau = NA_integer_,
                   sl = NA_integer_, 
                   S = NA_integer_,
                   D = NA_integer_, 
                   varS = NA_integer_,
                   CellCode=unique(lm_list[[name]]$CellCode))
    }else{
      result <- MannKendall(dataset)
      result$CellCode <- unique(lm_list[[name]]$CellCode) #Add CellCode 
    }
    
    # Append the result to the dummy_trend
    dummy_trend <- rbind(dummy_trend, result)
  }
  
  
  setDT(dummy_trend)
  
  kendall<-dummy_trend[, trend := ifelse(S < 0 & sl <= 0.05, "decreasing",
                                         ifelse(S > 0 & sl <= 0.05, "increasing",
                                                ifelse(sl > 0.05, "no trend", NA)))]
  
  general_trends<- merge(kendall, trend, by = "CellCode", all.x = TRUE)
  
  general_trends[, accuracy := ifelse(robustness == "no robust" & trend != "no trend", 1, 0), by = .(robustness, trend)]
  
  #if the model is not robust, no trend should be given
  general_trends[, trend := ifelse(robustness == "no robust", "no trend", trend)]
  
  general_trends<- merge(general_trends,nrYears, by="CellCode", all.x = T)
}
