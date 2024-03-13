# MTE dissertation
# Jesus Lopez Perez
# Modeling

# Setup ---------------------------

rm(list = ls())

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/"
path2 <- paste("D:/Personal/MTE/Tesis/Propuestas_Temas/",
               "credit-markets/", sep = "")


source(paste(path, "scripts/",
             "utility_functions.R", sep = ""))

library(sandwich)
library(lmtest)
library(tidymodels)
library(AER)
library(plm, warn.conflicts = FALSE)
library(stargazer)
library(estimatr)

G7 <- TRUE
if(G7 == TRUE){
  banks <- c("040002", "040012", "040014", "040021",
             "040072", "040044", "040036")
  which_db <- "G7"
}else{
  banks = NULL
  which_db <- "ALL"
}

subset_term_0 <- FALSE

# subset data
if(subset_term_0 == TRUE){
  which_term <- "term_u_1"
}else{
  which_term <- "term_geq_1"
}

# Select Heteroskedasticity-consistent covariance options
# white1 - general heteroskedasticity, no serial cor (rec R.E.)
# white2 - white1, restricted to common variance within groups (rec. R.E.)
# arellano - both Heteroskedasticity and serial correlation (Rec. for F.E.)
method_vcov <- "arellano"

# HC0 - Heteroskedasticity consistent
# HC1, HC2, HC3 - for small samples
# HC4 - small samples with influential observations
# HAC - Heteroskedasticity and autocorrelation consistent
vcov_type <- "HAC" #

# formula for double-clustering (Millo, 2014)
# https://mpra.ub.uni-muenchen.de/54954/1/MPRA_paper_54954.pdf, p. 21
Vw <- function(x) vcovHC(x, method = "white1")
Vcx <- function(x) vcovHC(x, cluster = "group", method = "arellano")
Vct <- function(x) vcovHC(x, cluster = "time", method = "arellano")
Vcxt <- function(x) Vcx(x) + Vct(x) - Vw(x)

# Load data ---------------------------
panel_data_r <- read_csv(paste(path, "/proc/panel_firms_",
                               which_db,"_",which_term,"_2T_16_4T_21.csv", sep = ""),
                         locale = readr::locale(encoding = "latin1"))

panel_data_r <- panel_data_r %>% 
  mutate(institution_code = str_pad(institution_code,
                width = 6, pad = "0", side = "left")) %>% 
  mutate(period = period %>% lubridate::ymd()) 



# setup for iv variables ---------------------------
iv_vars <- c("wa_atm",
             "wa_est_pos", "wa_branches",
             "wa_transactions_atm", "wa_total_staff")
iv_vars_public <- c("Number of ATM", "Number of POS terminals",
                 "Number of branches", "Number of ATM transactions",
                 "Total Staff")

iv_mat <- panel_data_r[,iv_vars]

# code for combining IV
N <- length(iv_vars)
n_comb <- 0
for(i in 1 : N)
  n_comb <- n_comb + ncol(combn(N, i))

mat_models <- matrix(FALSE, n_comb, N)

colnames(mat_models) <- iv_vars

h <- 0
for(i in 1 : N){
  combn_i <- combn(N, i)
  
  for(j in 1 : ncol(combn_i)){
    J <- j + h
    mat_models[J, combn_i[,j]] <- TRUE
  }
  h <- h + ncol(combn_i)
}

iv_models_list <- list()
for(i in 1:n_comb){
  iv_models_list[[i]] <- paste(iv_vars[mat_models[i,]], collapse = " + ")
  #iv_models_list[[i]] <- paste("log(wa_interest_rate_d) ~ ",temp, sep ="")
}


# create dummies function
clase_mat <- function(X, clase_var){
  # X <- panel_data_r; clase_var <- "institution_code"
  X <- data.frame(X)
  X_c <- X[,clase_var]
  
  X_d <- matrix(0, length(X_c), length(unique(X_c)))
  colnames(X_d) <- unique(X_c)
  for(class_i in unique(X_c))
    X_d[which(X_c == class_i), class_i] <- 1
  
  return(X_d)
}

# create dummies for institution
dum_inst <- clase_mat(panel_data_r,"institution_code")
inst <- cat_eeffs[cat_eeffs[,"institution_code"] %in% colnames(dum_inst),"institution"]
colnames(dum_inst) <- inst


# create dummies for sector_code
dum_sect <- clase_mat(panel_data_r,"sector_code")
colnames(dum_sect) <- paste("S",colnames(dum_sect), sep = "")
sect_names <- colnames(dum_sect)


# modeling ---------------------------

# IV demand estimation 

# exogenous variables: wa_term_d, scian_code, funds_use_code
#     currency_code, firm_size_code
# endogeous variable: wa_interest_rate_d
# IV: num_atm, num_est_pos, num_branches,
#   num_transactions_atm, total_staff
# Dependent variable: sum_disposition
# panel indices: ID and period (Year-Month_Day)


# panel setup 
pdata <- pdata.frame(panel_data_r,
                     index = c("sector_code") )
print(pdim(pdata))

summary(log(pdata$wa_interest_rate_d))
summary(log(pdata$sum_disposition))

head(as.matrix(pdata$wa_interest_rate_d))

temp <- sapply(1:ncol(pdata), function(x) is.na(pdata[,x]))
n_nas <- colSums(temp)
names(n_nas) <- colnames(pdata); n_nas

# write.csv(m1, paste(path, "/proc/int_rate_as_matrix.csv", sep = ""))

# plot fixed effects deviated from mean
# plot(fixef(base_fixed, type = "dmean")[order(fixef(base_fixed, type = "dmean"))])


# first stage estimations, pick 1:31 models
# log(wa_interest_rate) ~ log(IV) + controls
function_fs <- function(data, model_i, transf = "log"){
  # data <- panel_data_r; model_i <- 1; transf = "log"
  
  # prepare data
  which_logs <- c("sum_disposition","wa_interest_rate_d",
                  "wa_term_d",iv_vars)
  if(transf == "log"){
    data[,which_logs] <- log1p(data[,which_logs])
  }
  
  # full model
  form_first_stage_full <- 
    paste("wa_interest_rate_d",
          " ~ wa_term_d + ",
          paste(colnames(dum_inst)[-2],collapse = " + "),
          " + ", 
          paste(sect_names, collapse = " + "),
          " - 1 ", " + ", 
          iv_models_list[[model_i]],
          
          sep = "")

  fs_plm_full <- lm(form_first_stage_full, 
                     #model = "within",
                     data = data,
                     #index = c("sector_code")
                    weights = N )
  
  cov_fs_plm <- Vcxt(fs_plm_full)
  robust_se_fs_plm <- sqrt(diag(cov_fs_plm))
  
  
  # cov_fs_plm <- vcovHC(fs_plm_full, 
  #                      method = method_vcov,
  #                      type = "HC0")
  # robust_se_fs_plm <- sqrt(diag(cov_fs_plm))
  
  # full_model_data <- fs_plm_full$model
  # full_model_data$wa_interest_rate_d <- 
  #   exp(full_model_data$`log(wa_interest_rate_d)`)
  
  # F test by hand, equal to Wald Test
  # WARNING! No robust errors
  # reduced model
  # form_first_stage_red <- paste("log(wa_interest_rate_d)",
  #                               " ~ wa_term_d + state_code + scian_code + ", 
  #                               "funds_use_code + currency_code + ",
  #                               "firm_size_code + institution_code", sep = "")
  # fs_plm_red <- plm(form_first_stage_red, 
  #                   model = "within",
  #                   effect = "individual",
  #                   data = full_model_data)
  # ssr_ur <- sum(summary(fs_plm_full)$residuals^2)
  # ssr_r <- sum(summary(fs_plm_red)$residuals^2)
  # N <-  df.residual(fs_plm_full)
  # F_ <- (ssr_r - ssr_ur) / (ssr_ur) * (N - 2) / 1
  # waldtest(fs_plm_full, fs_plm_red)
  # Wald test with robust errors
  # car::linearHypothesis  is equivalent to 
  # waldtest(fs_plm_full, fs_plm_red,
  #     vcov=function(x) vcovHC(x, method="arellano", type="HC1"))
  
  fs_f_test <-
    linearHypothesis(fs_plm_full, 
                     paste(iv_models_list[[model_i]],"=0", sep = ""),
                     type = c("Chisq"), 
                     vcov. = cov_fs_plm
    )
  
  fs_f_test_results <- c(fs_f_test[3][2,], 
                         fs_f_test[4][2,])
  names(fs_f_test_results) <- c("F", "P-value")
  temp <- list(fs_plm_full, robust_se_fs_plm,
               fs_f_test_results)
  names(temp) <- c("fs", "fs_se","fs_f_test")
  return(temp)
}


# estimate IV models
# log(sum_disposition) ~ log(wa_interest_rate) + log(term) 
#                          + controls + IV
function_iv_models <- function(data, model_i, transf = "log"){
  # data <- panel_data_r; model_i <- 1; transf = "log"
  
  # prepare data
  which_logs <- c("sum_disposition","wa_interest_rate_d",
                  "wa_term_d",iv_vars)
  if(transf == "log"){
    data[,which_logs] <- log1p(data[,which_logs])
  }
  
  # benchmark model, no IV
  if(model_i == 0){
    # formula
    base_model <- paste("sum_disposition ~ wa_interest_rate_d +",
            " wa_term_d + ", 
            paste(colnames(dum_inst)[-2],collapse = " + "),
            " + ", 
            paste(sect_names, collapse = " + "),
            " - 1 ", 
            sep = "")
    
    # within estimator
    base_fixed <- ivreg(base_model,
                            data = data, 
                            weights = N)
        
    # Adjust standard errors
    cov_base_fixed <- vcovHC(base_fixed, type = "HC1") # provisional due to 
    # Error in 1 - diaghat : non-numeric argument to binary operator
    # cov_base_fixed <- Vcxt(base_fixed)
    robust_se_cov_base_fixed <- sqrt(diag(cov_base_fixed))
    plm_est <- base_fixed
    rob_se <- robust_se_cov_base_fixed
  }else{
    form_iv <- 
      paste("sum_disposition ~ wa_interest_rate_d +",
            " wa_term_d + ", 
            paste(colnames(dum_inst)[-2],collapse = " + "),
            " + ", 
            paste(sect_names, collapse = " + "),
            " - 1 ",
            "| wa_term_d + ",
            paste(colnames(dum_inst)[-2],collapse = " + "),
            " + ",
            paste(sect_names, collapse = " + "),
            " - 1 + ",
            iv_models_list[[model_i]], 
            sep = "")
    
    estim <- ivreg(form_iv,
                   data = data, weights = N)

    # Adjust standard errors
    cov_fixed_iv <- Vcxt(estim)
    robust_se_cov_base_fixed <- sqrt(diag(cov_fixed_iv))
    plm_est <- estim
    rob_se <- robust_se_cov_base_fixed
  }
  
  temp = list(plm_est, rob_se)
  names(temp) <- c("model", "robust_se")
  return(temp)
}



# punbalancedness(panel_data_r, 
#                 index = c("sector_code"))


# Run estimations
# first set of IV estimations --------------------
# a. first stages
fs_models_list_1_5 <- 
  map(1:5,function(x) function_fs(pdata,x,transf = "log"))

# b. estimating IV models
base_fixed <- function_iv_models(pdata, 0, transf = "log")
models_list_1_5 <- 
  map(1:5,function(x) function_iv_models(pdata,x) )


# Save console
save.image(paste(path2, "outputs/R/",
                 "Estimations_",which_panel,"_",which_db,".RData", sep = ""))





#### end not run -------------------------------
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% fit(log(sum_disposition) ~ log(wa_interest_rate_d) + 
                     wa_term_d + scian_code + 
                     state_code + funds_use_code + 
                     currency_code + firm_size_code, 
                   data = panel_data_r)

lm_form_fit %>% extract_fit_engine()

lm_form_fit %>% extract_fit_engine() %>% vcov()

tidy(lm_form_fit)



iv_mat <- panel_data_r[,iv_vars]
r_sq <- vector("numeric",length = nrow(mat_models))
# estimate all models P ~ IV
for(i in 1 : nrow(mat_models)){ print(i)
  # debug: i <- 7
  
  iv_mat_i <- 
    as.matrix(iv_mat[,mat_models[i,],drop = FALSE])
  
  which_cols <- which(mat_models[i,] == TRUE)
  
  print(paste("n = ", nrow(panel_data_r), "; p = ", ncol(panel_data_r), 
              "; q = ", ncol(iv_mat_i), sep = "") )
  
  lm_i <- 
    lm(log(panel_data_r$wa_interest_rate_d) ~ scale(iv_mat_i[,,drop=FALSE ]))
  r_sq[i] <- summary(lm_i)$r.squared
  sig_vars[i,which_cols] <- summary(lm_i)$coefficients[,"Pr(>|t|)"][-1] <= alpha
  
}

max_sig <- max(colSums(sig_vars, na.rm = T))

rel_variables <- 
  colnames(sig_vars)[colSums(sig_vars, na.rm = T) == max_sig]

# optim model
best_model <- 
  formula(log(panel_data_r$wa_interest_rate_d) ~ scale(iv_mat[,rel_variables]))

summary(lm(best_model))

# lasso   
library(glmnet)

list_var_names <- list()

# para regresion LASSO
lambda_seq <- c(.00001, .001, .1, .5, 1, 2, 10, 50, 500)

# seleccionado la lamda optima

iv_mat$iv_mat_is_NA <- rowSums(apply(iv_mat, 2, is.na))


cv_output <- cv.glmnet(scale(iv_mat[iv_mat$iv_mat_is_NA ==0,1:5]), 
                       log(panel_data_r$wa_interest_rate_d[iv_mat$iv_mat_is_NA ==0]), 
                       alpha = 1, lambda = lambda_seq)

# lambda optima
best_lam <- cv_output$lambda.min

# regresion lasso
lasso_best <- glmnet(scale(iv_mat[iv_mat$iv_mat_is_NA ==0,1:5]), 
                     log(panel_data_r$wa_interest_rate_d[iv_mat$iv_mat_is_NA ==0]),
                     alpha = 1, lambda = best_lam)

# variables signficativas
names(which(abs(coef(lasso_best)[-1, 1]) > 0))

knitr::kable(data.frame(cbind(names(coef(lasso_best)),
                              matrix(coef(lasso_best)))), digits = 3)
