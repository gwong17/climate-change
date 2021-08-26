# ************************************************************************************************
# Climate Change Modelling for General Corporate
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
#source("packager.r")

# collect historial z-scores
# corporate_all_segment <- readRDS("C:/Users/mnograd/Documents/r-scripts/Segmentation/calibrations/Corporate.AMERICAS.MQ1-MQ16.all.Rds")
Z_obj_GC    <- readRDS("C:/Users/jye8/OneDrive - Macquarie Group/Personal Folders/Documents/2021 CREDIT MODELLING (RMG Credit)/Climate Change/Z_obj_GC_2021V1.Rds")

Z_Scores_GC <- Z_obj_GC[["rho_calc"]][["Z_value"]]; Z_Scores_GC$flag <- 1
Z_Scores_GC <- melt(Z_Scores_GC, id.vars=c("flag"), measure.vars=setdiff(names(Z_Scores_GC), c("flag")))

setDT(Z_Scores_GC)
setnames(Z_Scores_GC, c("variable", "value"), c("Year","Z"))
Z_Scores_GC$Year <- as.character(Z_Scores_GC$Year)
Z_Scores_GC$Year <- as.integer(Z_Scores_GC$Year)

# import raw macro data
macro_data <- readRDS("C:/Users/jye8/OneDrive - Macquarie Group/Personal Folders/Documents/2021 CREDIT MODELLING (RMG Credit)/Climate Change/ds.Rds"); setDT(macro_data)

macro_data$Year <- year(macro_data$Date)
macro_data[,Date:= NULL]

macro_data <- as.data.frame(macro_data)

# detrending
a_ply(names(macro_data %>% select(-Year)), 1, function(v){
  macro_data[,v] <<- detrend_and_norm(macro_data[,v])
})

setDT(macro_data)
gc_macro_data <- merge(Z_Scores_GC, macro_data, by="Year", all.x = TRUE)
gc_macro_data <- gc_macro_data[Year >= 1999]

gc_macro_data <- as.data.frame(gc_macro_data)

# ******** Variable Selection ********

gc_macro_data <- gc_macro_data[, colSums(is.na(gc_macro_data)) == 0]

calib_univar <- adply(names(gc_macro_data)[c(-1,-2)], # Takes names of all 357 variables (except Year and Flag)
                      1, # Splits by rows, one variable per row
                      function(x){ # Regress Z on each of the variables - now each variable has:
                        modl <- lm(gc_macro_data[,3] ~ gc_macro_data[,x]) # An SLR
                        data.frame(Variable = x,
                                   Correlation = cor(gc_macro_data[,3], gc_macro_data[,x]), # A correlation
                                   "R-squared" = summary(modl)$r.squared, # An R-squared/goodness of fit
                                   "p-value" = summary(modl)$coefficients[2,4], # and a p-value
                                   check.names = F, stringsAsFactors = F)
                      })

# filter p-vals > 0.1
gc_macro_data <- gc_macro_data %>% select_("Year", "Z", 
                                           .dots = calib_univar %>% filter(`p-value` <= 0.1) %>% .$Variable)

#We use a cross-validation lasso fit to find the optimal shrinkage parameter, at which point the set of regressors with coefficients that haven't been shrunken to zero can be considered the best subset.
#The optimal range of shrinkage values is designated by the dashed lines in the chart below from a single CV run,
#where between 6  and 11 non-zero coeffcient variables remain in the model at different shrinkage values.
#In this case, we select the variables for the shrinkage value that produces the smallest mean-squared error in the cross validation.

fit <- glmnetUtils::glmnet(Z ~ ., gc_macro_data %>% select(-Year), dfmax = 10, standardize = F)
plot(fit)

set.seed(3323)
cv.fit <- glmnetUtils::cv.glmnet(Z ~ ., gc_macro_data  %>% select(-Year), dfmax = 10, standardize = F)
plot(cv.fit)

#We run the cross validation 1000 times, each time selecting the variables where RMSE is minimised.
#We tabulate the frequency of the factors appearing in the best subset for each CV.
#We are now left with the following subset of variables, where the frequency greater than 500:

find_factors_cv_glmnet2 <- function(calib_data, dfmax = 10, n = 1000){
  vars <- NULL  
  
  for(i in 1:n){
    cv.fit <- glmnetUtils::cv.glmnet(Z ~ ., calib_data  %>% select(-Year), dfmax = dfmax, standardize = F, grouped=FALSE)
    vars <- c(vars, names(which(coef(cv.fit, s = "lambda.min")[, 1] != 0))[-1])
  }
  
  table(vars) %>% 
    data.frame() %>%
    select(Variable = vars, Frequency = Freq) %>%
    mutate(Variable = as.character(Variable))
}

var_freqs <- find_factors_cv_glmnet2(calib_data = gc_macro_data)

#var_freqs %>%
#  left_join(calib_meta %>% select(instance, Description) %>% unique(), by = c("Variable" = "instance")) %>%
#  arrange(desc(Frequency)) %>%
#  filter(Frequency >= 500) %>%
#  knitr::kable(digits = 3)

glmnet_vars <- var_freqs$Variable[var_freqs$Frequency >= 500] # split here

# The strongest variables can be chosen based on their importance metric:
form <- as.formula(paste("Z ~ ", paste(glmnet_vars, collapse = "+"), sep = ""))
gc_model_lm <- lm(form, gc_macro_data)
varImp(coal_model_lm) %>% knitr::kable(digits = 3)


# ******************* correlation *******************----

setDT(gc_macro_data)
res <- cor(gc_macro_data[Year >= 1999])

test <- data.table(res, keep.rownames = TRUE)
test <- test[,.(rn, Z)]

test <- (test[,.(rn, Z, WLD_PSH_avg_yrly_rel_chg, USA_PC_avg_yrly_rel_chg, WLD_OECDTPEN_avg_yrly_rel_chg, WLD_WCARB_avg_yrly_rel_chg_lag_1, WLD_WPGASHH_avg_yrly_rel_chg_lag_1, WLD_WDOIL_avg_yrly_rel_chg)])
test <- test[rn %in% c("WLD_PSH_avg_yrly_rel_chg", "USA_PC_avg_yrly_rel_chg", "WLD_OECDTPEN_avg_yrly_rel_chg", "WLD_WCARB_avg_yrly_rel_chg_lag_1", "WLD_WPGASHH_avg_yrly_rel_chg_lag_1","WLD_WDOIL_avg_yrly_rel_chg")]
View(test)

setDT(gc_macro_data)

# *********************** modelling ********************************

WLD_GDPHED_EXC

# Find correlations of shortlisted variables against Z-Score

cor(gc_macro_data$Z, gc_macro_data$WLD_WPPOWER_avg_yrly_rel_chg)

# filter for general corporate related variables

macro_data_GC <- macro_data[ , grepl( "USA_PC_|WLD_WCARB_|WLD_WPC_|WLD_PSH_" , names( macro_data ) ) ]
macro_data_GC[is.na(macro_data_GC)] <- 0

# narrow list to shortlisted general corporate variables

macro_data_GC2 <- macro_data_GC[ , grepl( "_rel_chg" , names( macro_data_GC ) ) ]

# Current Shortlisted Variables

shortlisted <- select(macro_data_GC2,
                      USA_PC_avg_yrly_rel_chg,
                      WLD_WCARB_avg_yrly_rel_chg_lag_1,
                      WLD_WPC_avg_yrly_rel_chg,
                      WLD_PSH_avg_yrly_rel_chg) # CHANGE THIS

# form correlation table

correlmatrix_GC <- cor(shortlisted)

zcorrel_GC <- cor(shortlisted[-1], shortlisted$USA_PC_avg_yrly_rel_chg) # CHANGE USA to Z-Score once RDS received

correltable_GC <- data.frame(row=rownames(correlmatrix_GC)[row(correlmatrix_GC)[upper.tri(correlmatrix_GC)]],
                             col=colnames(correlmatrix_GC)[col(correlmatrix_GC)[upper.tri(correlmatrix_GC)]],
                             corr=correlmatrix_GC[upper.tri(correlmatrix_GC)])

# view correlations in descending order

correltable_GC[order(correltable_GC$corr, decreasing = TRUE),]

# Final Variable Selection----

calib_data <- gc_macro_data[,.(Z, 
                               Year, 
                               WLD_PSH_avg_yrly_rel_chg, 
                               USA_PC_avg_yrly_rel_chg, 
                               WLD_WPC_avg_yrly_rel_chg, 
                               WLD_WCARB_avg_yrly_abs_chg, 
                               WLD_WDOIL_avg_yrly_rel_chg, 
                               WLD_WDGAS_avg_yrly_rel_chg)]

# *** model variables ***
my_vars <- setdiff(names(calib_data), c("Z", "Year"))

# **** model change ****
my_form <- as.formula(paste("Z ~ ", paste(c(my_vars) , collapse = "+"), sep = ""))

model <- plsr(my_form, data = calib_data, validation = "LOO", jackknife = T, ncomp = 2)
#coefficients(model, intercept = TRUE, ncopm = 2, intersect = T)
glance.mvr(model)
tidy.mvr(model)

# plot fit
plot_macro_Z_fit(model, calib_data)
  labs(title = "Americas Oil & Gas Z-series macro calibration (candidate 1)",
       subtitle = "")

# linear regression
model <- lm(calib_data, data = calib_data)
summary(model)

# plot data
ggplot(data = gc_macro_data, aes(x=Year, y=USA_GDP_EXC_avg_yrly_rel_chg)) +
  geom_line()

# *****************************
# ******** functions **********
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

