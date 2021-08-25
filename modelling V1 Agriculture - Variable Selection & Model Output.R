# ************************************************************************************************
# Climate Change Model for Agriculture
# ************************************************************************************************

# install packages

library(AxiomWholesale)
library(MQGmacro)
library(MQGmigrations)
library(MQGG)
library(corrplot)
library(glmnet)
library(plsr)
library(dplyr)
library(plyr)
library(stringr)
library(magrittr)
library(RSQLite)
library(broom)
library(pls)
library(plsVarSel)

source("C:/Users/jye8/OneDrive - Macquarie Group/Personal Folders/Documents/r-scripts/Segmentation/calibration-functions.r")

# collect historical z-scores

Z_obj_Ag <- readRDS("C:/Users/jye8/OneDrive - Macquarie Group/Personal Folders/Documents/2021 CREDIT MODELLING (RMG Credit)/Climate Change/Z_obj_Ag_2021V2.Rds")

Z_Scores_Ag <- Z_obj_Ag[["rho_calc"]][["Z_value"]]; Z_Scores_Ag$flag <- 1
Z_Scores_Ag <- melt(Z_Scores_Ag, id.vars=c("flag"), measure.vars=setdiff(names(Z_Scores_Ag), c("flag")))

setDT(Z_Scores_Ag)
setnames(Z_Scores_Ag, c("variable", "value"), c("Year","Z"))
Z_Scores_Ag$Year <- as.character(Z_Scores_Ag$Year)
Z_Scores_Ag$Year <- as.integer(Z_Scores_Ag$Year)

# import raw macro data

macro_data <- readRDS("C:/Users/jye8/OneDrive - Macquarie Group/Personal Folders/Documents/2021 CREDIT MODELLING (RMG Credit)/Climate Change/ds.Rds"); setDT(macro_data)

macro_data$Year <- year(macro_data$Date)
macro_data[,Date:= NULL]

macro_data <- as.data.frame(macro_data)

# detrend

a_ply(names(macro_data %>% select(-Year)), 1, function(v){
  macro_data[,v] <<- detrend_and_norm(macro_data[,v])
})

setDT(macro_data)
ag_macro_data <- merge(Z_Scores_Ag, macro_data, by="Year", all.x = TRUE)
ag_macro_data <- ag_macro_data[Year >= 1999]

ag_macro_data <- as.data.frame(ag_macro_data)

# SELECT VARIABLES

## Imputes Zeros where NA's are
ag_macro_data <- ag_macro_data[, colSums(is.na(ag_macro_data)) == 0]

# Run all Single Linear Regressions
calib_univar <- adply(names(ag_macro_data)[c(-1,-2)], # Takes names of all 357 variables (except Year and Flag)
                      1, # Splits by rows, one variable per row
                      function(x){ # Regress Z on each of the variables - now each variable has:
                        modl <- lm(ag_macro_data[,3] ~ ag_macro_data[,x]) # An SLR
                        data.frame(Variable = x,
                                   Correlation = cor(ag_macro_data[,3], ag_macro_data[,x]), # A correlation
                                   "R-squared" = summary(modl)$r.squared, # An R-squared/goodness of fit
                                   "p-value" = summary(modl)$coefficients[2,4], # and a p-value
                                   check.names = F, stringsAsFactors = F)
                      })

## Filter p-values > 0.1 (there are 56 variables for Ag)
ag_macro_data <- ag_macro_data %>% select_("Year", "Z", 
                                           .dots = calib_univar %>% filter(`p-value` <= 0.1) %>% .$Variable)
# Selects 56 variables and calls them by .dots

# We use a cross-validation lasso fit to find the optimal shrinkage parameter, at which point the set of regressors with coefficients that haven't been shrunken to zero can be considered the best subset.
# The optimal range of shrinkage values is designated by the dashed lines in the chart below from a single CV run,
# where between (1) and (11) non-zero coeffcient variables remain in the model at different shrinkage values.
# In this case, we select the variables for the shrinkage value that produces the smallest mean-squared error in the cross validation.

# Single Linear Regression Plot

fit <- glmnetUtils::glmnet(Z ~ ., ag_macro_data %>% select(-Year), dfmax = 10, standardize = F)
plot(fit)
# How to interpret the graph: 
# Left hand side = highly constricted, every variable has been pulled to zero.
# Right hand side = low constriction, you just get a OLS solution.

# Single CV Plot (Optimal Range)

set.seed(3323)
cv.fit <- glmnetUtils::cv.glmnet(Z ~ ., ag_macro_data  %>% select(-Year), dfmax = 10, standardize = F)
plot(cv.fit) 

#We run the cross validation 1000 times, each time selecting the variables where RMSE is minimised.
#We tabulate the frequency of the factors appearing in the best subset for each CV.
#We are now left with the following subset of variables, where the frequency greater than 500:

find_factors_cv_glmnet2 <- function(calib_data, dfmax = 10, n = 1000){
  vars <- NULL  
  
  for(i in 1:n){
    cv.fit <- glmnetUtils::cv.glmnet(Z ~ ., calib_data  %>% dplyr::select(-Year), dfmax = dfmax, standardize = F, grouped=FALSE)
    vars <- c(vars, names(which(coef(cv.fit, s = "lambda.min")[, 1] != 0))[-1])
  }
  
  table(vars) %>% 
    data.frame() %>%
    dplyr::select(Variable = vars, Frequency = Freq) %>%
    mutate(Variable = as.character(Variable))
} # Updated function which specifices package

var_freqs <- find_factors_cv_glmnet2(calib_data = ag_macro_data)

#var_freqs %>%
#  left_join(calib_meta %>% select(instance, Description) %>% unique(), by = c("Variable" = "instance")) %>%
#  arrange(desc(Frequency)) %>%
#  filter(Frequency >= 500) %>%
#  knitr::kable(digits = 3)

glmnet_vars <- var_freqs$Variable[var_freqs$Frequency >= 500] # split here

# The strongest variables can be chosen based on their importance metric:
form <- as.formula(paste("Z ~ ", paste(glmnet_vars, collapse = "+"), sep = ""))
ag_model_lm <- lm(form, ag_macro_data)
varImp(ag_model_lm) %>% knitr::kable(digits = 3)

# ******************* correlation *******************-----

setDT(ag_macro_data)
res <- cor(ag_macro_data[Year >= 1999])

test <- data.table(res, keep.rownames = TRUE)
test <- test[,.(rn, Z)]

test <- (test[,.(rn, Z, WLD_PSH_avg_yrly_rel_chg, USA_PC_avg_yrly_rel_chg, WLD_OECDTPEN_avg_yrly_rel_chg, WLD_WCARB_avg_yrly_rel_chg_lag_1, WLD_WPGASHH_avg_yrly_rel_chg_lag_1, WLD_WDOIL_avg_yrly_rel_chg)])
test <- test[rn %in% c("WLD_PSH_avg_yrly_rel_chg", "USA_PC_avg_yrly_rel_chg", "WLD_OECDTPEN_avg_yrly_rel_chg", "WLD_WCARB_avg_yrly_rel_chg_lag_1", "WLD_WPGASHH_avg_yrly_rel_chg_lag_1","WLD_WDOIL_avg_yrly_rel_chg")]
View(test)

setDT(ag_macro_data)

# *********************** modelling ********************************----

WLD_GDPHED_EXC

# Find correlations of shortlisted variables against Z-Score

cor(ag_macro_data$Z, ag_macro_data$WLD_WPPOWER_avg_yrly_rel_chg)
ag_macro_data$Z

# filter for agriculture related variables

macro_data_ag <- macro_data[ , grepl( "USA_PC_|WLD_WCARB_|WLD_WPC_|WLD_PSH_|WLD_WPFD_|WLD_WPFER_|WLD_WPFOOD_" , names( macro_data ) ) ]
macro_data_ag[is.na(macro_data_ag)] <- 0

# narrow list to shortlisted agriculture variables

macro_data_ag2 <- macro_data_ag[ , grepl( "_rel_chg" , names( macro_data_ag ) ) ]

# Current Shortlisted Variables

shortlisted <- select(macro_data_ag2,
                      USA_PC_avg_yrly_rel_chg,
                      WLD_WCARB_avg_yrly_rel_chg_lag_1,
                      WLD_WPC_avg_yrly_rel_chg,
                      WLD_PSH_avg_yrly_rel_chg,
                      WLD_WPFD_avg_yrly_rel_chg,
                      WLD_WPFER_avg_yrly_rel_chg,
                      WLD_WPFOOD_avg_yrly_rel_chg) # CHANGE THIS

# form correlation table

correlmatrix_ag <- cor(shortlisted)

zcorrel_ag <- cor(shortlisted[-1], shortlisted$USA_PC_avg_yrly_rel_chg) # CHANGE USA to Z-Score once RDS received

correltable_ag <- data.frame(row=rownames(correlmatrix_ag)[row(correlmatrix_ag)[upper.tri(correlmatrix_ag)]],
                             col=colnames(correlmatrix_ag)[col(correlmatrix_ag)[upper.tri(correlmatrix_ag)]],
                             corr=correlmatrix_ag[upper.tri(correlmatrix_ag)])

# view correlations in descending order

correltable_ag[order(correltable_ag$corr, decreasing = TRUE),]

# Final Variable Selection----

setDT(ag_macro_data)

calib_data <- ag_macro_data[,.(Z, Year, 
                               WLD_WPFER_avg_yrly_rel_chg,
                               WLD_WPGAS_avg_yrly_rel_chg,
                               WLD_WSTOCK_avg_yrly_rel_chg,
                               WLD_WBGAS_avg_yrly_rel_chg
)]

# *** model variables ***
my_vars <- setdiff(names(calib_data), c("Z", "Year"))
# **** model change ****
my_form <- as.formula(paste("Z ~ ", paste(c(my_vars) , collapse = "+"), sep = ""))

### PARTIAL LEAST SQUARES
model <- plsr(my_form, data = calib_data, validation = "LOO", jackknife = T, ncomp = 2)
#coefficients(model, intercept = TRUE, ncopm = 2, intersect = T)
glance.mvr(model)
tidy.mvr(model)
# plot fit
plot_macro_Z_fit(model, calib_data)
#  labs(title = "Agriculture Z-series macro calibration (candidate 1)",
#       subtitle = "")

### MULTIPLE LINEAR REGRESSION
model2 = lm(my_form,data=calib_data)  
summary(model2)
# plot fit
plot_macro_Z_fit(model2,calib_data)
  labs(title = "Agriculture Z-series macro calibration (candidate 2)",
         subtitle = "")
  
# linear regression
model <- lm(calib_data, data = calib_data)

summary(model)

# plot data
ggplot(data = ag_macro_data, aes(x=Year, y=USA_GDP_EXC_avg_yrly_rel_chg)) +
  geom_line()

# *****************************
# ******** functions **********-----
# *****************************
WLD_WPC_avg_yrly_rel_chgtidy.mvr <- function(x, ...){
  jt <- jack.test(x, ncomp = x$ncomp)
  ret <- data.frame(term = rownames(jt$coefficients),
                    estimate = jt$coefficients %>% array(),
                    std.error = jt$sd %>% array(),
                    statistic = jt$tvalues %>% array(),
                    p.value = jt$pvalues %>% array(), row.names = NULL)
  ret %>% tbl_df %>% fix_data_frame()
}

glance.mvr <- function(x, ...){
  R2 <- drop(pls::R2(x, estimate = "train", intercept = FALSE)$val)[[x$ncomp]]
  CV <- drop(pls::R2(x, estimate = "CV", intercept = FALSE)$val)[[x$ncomp]]
  ret <- data.frame(R.squared = R2, Pred.R.squared = CV) #, r.squared.adjCV = adjCV)
  ret %>% tbl_df %>% fix_data_frame()
}

make_model_package <- function(name, segment, region,
                               Z_obj, regression_object, regression_data,
                               raw_regression_data, grade_block = MQGCreditMisc::MQ_ratings[1:16],
                               ncomp = 1,
                               weight, version){

  data_params <- if(!is.null(regression_data)) plyr::adply(names(raw_regression_data)[-1],1,function(s){
    raw_regression_data %>% select_(s) %>% unlist() %>% series_params(.) %>% data.frame(name=s,stringsAsFactors=F)
  }, .id = NULL) else NA

  #Z_obj$hist$historical_TM2 <- MQGmigrations::avg_trans_mat.MQG

  pkg <- list(name = name,
              segment = segment,
              region = region,
              Z_obj = Z_obj,
              regression_object = regression_object,
              regression_data = regression_data,
              raw_regression_data = raw_regression_data %>% na.omit(),
              data_params = data_params,
              grade_block = grade_block,
              weight = weight,
              ncomp = ncomp,
              version = version)
}

series_params <- function(series){
  series_detrended <- detrend_series(series)

  sigma <- sd(series_detrended, na.rm = T)
  mu <- tail(series, 1) - tail(series_detrended, 1)

  data.frame("mean" = mu, "sd" = sigma)
}

make_segment_object <- function(objs){

  if(!is.list(objs))
    objs <- list(objs)

  segment_obj = list(objs = objs,
                     segment = objs[[1]]$segment,
                     region = objs[[1]]$region,
                     version = "2.0")

  segment_obj
}

write_segment_obj <- function(segment_obj, path = "./segments/", filename = NULL){
  if(is.null(filename))
    filename <- paste(segment_obj$segment, segment_obj$region, "Rds", sep = ".")

  dir.create(path, showWarnings = F)
  saveRDS(segment_obj, file = paste(path, filename, sep="/"))
}


series_params <- function(series){
  series_detrended <- detrend_series(series)

  sigma <- sd(series_detrended, na.rm = T)
  mu <- tail(series, 1) - tail(series_detrended, 1)

  data.frame("mean" = mu, "sd" = sigma)

}


detrend_series <- function(series){
  idx <- 1:length(series)
  idx <- idx[!is.na(series)&!is.infinite(series)]

  if(length(idx)<3)
    return(series)


  l <- lm(series[idx] ~  idx)

  series[idx] <- series[idx] - l$fitted.values
  series
}

plot_macro_Z_fit <- function(model, Z_data, response = "Z"){
  rsq <- if(class(model) == "mvr") drop(pls::R2(model, estimate = "train", intercept = FALSE)$val)[[model$ncomp]] else summary(model)$r.squared
  formula <- paste(response, paste(attr(model$terms, "term.labels"), collapse = " + "), sep = " ~ ")
  Z_data <- rename_(Z_data, "Z" = response)
  Z_data$Fit <- if(class(model) == "mvr") predict(model, Z_data, ncomp = model$ncomp) else predict(model, Z_data)

  dplyr::select(Z_data, Year, Z, Fit) %>% reshape2::melt(id.vars = "Year") %>%
    ggplot() + geom_line(aes(x = Year, y = value, colour = variable, linetype = variable), size=0.75) +
    geom_point(aes(x = Year, y = value, colour = variable), size = 2.5) + ylim(-3, 3) +
    scale_x_continuous(labels = Z_data$Year, breaks = Z_data$Year) +
    annotate("text", x = Z_data$Year[2], y = 2.75, label = paste("R^2==", round(rsq,2), sep = ""), parse = TRUE) +
    theme(panel.grid.major.x = element_line(colour = "light grey", size = 0.5), axis.text.x	= element_text(angle = 90)) +
    scale_colour_brewer(palette = "Set2") +
    labs(caption = paste0("Formula: Z ~ ",
                          attr(model$terms, "term.labels") %>% paste0(collapse = " + \n"))) + theme_grey()
}

