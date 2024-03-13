*MTE disertation
* Jesus López-Pérez

* Default prediction
clear

* Declare and setup working directory
global Path "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets"

capture log close
*log using "$Path/scripts/stata-files/modelling.log" , text replace


*----------------- (1) Creating master data set -------------------------------*
* Importing data
import delimited "$Path/Data/proc/panel_firms_G7large.csv"

*br



* recode variables
gen inst_code = string(institution_code,"%06.0f")
* gen scian_code = string(scian_code,"%02.0f")

* we take logarithms
gen log_mean_disposition = log(mean_disposition)
gen log_wa_interest_rate_d = log(wa_interest_rate_d)
gen log_i_other_sector = log(i_other_sector)
gen log_wa_term_d = log(wa_term_d)

*replace mean_disposition_r = log(mean_disposition_r)

* replace wa_term_d = asinh(wa_term_d)

* IV variables pre-treatment
* destring num_atm, replace force
*replace num_atm = log1p(num_atm)
*destring num_est_pos, replace force
*replace num_est_pos = log1p(num_est_pos)
*destring num_branches, replace force
*replace num_branches = log1p(num_branches)
*destring num_transactions_atm, replace force
*replace num_transactions_atm = log1p(num_transactions_atm)
*destring total_staff, replace force
*replace total_staff = log1p(total_staff)


encode inst_code,gen(inst_code1)
drop inst_code
rename inst_code1 inst_code

encode scian_code,gen(scian_code1)
drop scian_code
rename scian_code1 scian_code

encode period,gen(period1)
drop period
rename period1 period

encode industry_year,gen(industry_year1)
drop industry_year
rename industry_year1 industry_year

encode firm_size,gen(firm_size1)
drop firm_size
rename firm_size1 firm_size

*egen industry_year = group(industry_year)

order scian_code period inst_code
* drop scian_code institution_code
sort period scian_code inst_code

egen inst_sect = group(inst_code scian_code)

*summarize mean_disposition
*summarize mean_disposition[fweight = n]
*summarize mean_disposition[fweight = n_sect]

*summarize mean_disposition
*summarize mean_disposition[aweight = n]
*summarize mean_disposition[aweight = n_sect]

label var mean_disposition "$L$"
*label var mean_disposition_r "$L_r$"

*label var log1p_wa_interest_rate_d_r "$\ln(Interest Rate+1)$"
*label var asinh_wa_int_rate_d_r "$\text{arcsinh(Interest Rate}\text{)}$"

* for log-log
* calculate sample mean for log_mean_disposition
quietly sum log_mean_disposition, meanonly
scalar mymean_disp = r(mean)
*calculate sample mean for wa_interest_rate_d
quietly sum wa_interest_rate_d, meanonly
scalar mymean_i = r(mean)


quietly sum log_mean_disposition, meanonly
scalar mymean_disp = r(mean)
*calculate sample mean for wa_term_d
quietly sum wa_term_d, meanonly
scalar mymean_term = r(mean)



*  setup panel data 
quietly xtset scian_code

* Run regressions and save estimaes
* Base models

* NO IV
quietly xtivreg2 log_mean_disposition wa_interest_rate_d wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 [aweight = n_sect], cluster(scian_code) fe robust
estimates store no_iv

* elasticity between disposition and interest rate
estimates restore no_iv
nlcom (_b[wa_interest_rate_d] * (mymean_i) ), post 
scalar e_int = _b[_nl_1]
scalar e_int_SE = _se[_nl_1]
qui estadd scalar elas_int = e_int: no_iv
qui estadd scalar elas_int_SE = e_int_SE: no_iv
* elasticity between disposition and term
est restore no_iv
nlcom (_b[wa_term_d] * (mymean_term) ), post 
scalar e_term = _b[_nl_1]
scalar e_term_SE = _se[_nl_1]
qui estadd scalar elas_term = e_term: no_iv
qui estadd scalar elas_term_SE = e_term_SE: no_iv

		
* NO IV + industry_year
quietly xtivreg2 log_mean_disposition wa_interest_rate_d wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 x11_2016-x52_2021 [aweight = n_sect], cluster(scian_code) fe robust
estimates store no_iv_industry_year

* elasticity between disposition and interest rate
estimates restore no_iv_industry_year
nlcom (_b[wa_interest_rate_d] * (mymean_i) ), post 
scalar e_int = _b[_nl_1]
scalar e_int_SE = _se[_nl_1]
qui estadd scalar elas_int = e_int: no_iv_industry_year
qui estadd scalar elas_int_SE = e_int_SE: no_iv_industry_year
* elasticity between disposition and term
est restore no_iv
nlcom (_b[wa_term_d] * (mymean_term) ), post 
scalar e_term = _b[_nl_1]
scalar e_term_SE = _se[_nl_1]
qui estadd scalar elas_term = e_term: no_iv_industry_year
qui estadd scalar elas_term_SE = e_term_SE: no_iv_industry_year



* IV + industry year
quietly xtivreg2 log_mean_disposition wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 x11_2016-x52_2021 (wa_interest_rate_d = i_other_sector) [aweight = n_sect], cluster(scian_code) fe robust first
estimates store iv_model

* elasticity between disposition and interest rate
estimates restore iv_model
nlcom (_b[wa_interest_rate_d] * (mymean_i) ), post 
scalar e_int = _b[_nl_1]
scalar e_int_SE = _se[_nl_1]
qui estadd scalar elas_int = e_int: iv_model
qui estadd scalar elas_int_SE = e_int_SE: iv_model
* elasticity between disposition and term
est restore iv_model
nlcom (_b[wa_term_d] * (mymean_term) ), post 
scalar e_term = _b[_nl_1]
scalar e_term_SE = _se[_nl_1]
qui estadd scalar elas_term = e_term: iv_model
qui estadd scalar elas_term_SE = e_term_SE: iv_model


estimates restore iv_model
estadd scalar weak_iv_test1 =  `e(rkf)': iv_model


* IV + industry_year + log
quietly xtivreg2 log_mean_disposition log_wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 x11_2016-x52_2021 (log_wa_interest_rate_d = log_i_other_sector) [aweight = n_sect], cluster(scian_code) fe robust first

estimates store iv_industry_year_log

estimates restore iv_industry_year_log
estadd local weak_iv_test2 =  `e(rkf)': iv_industry_year_log

******************************************	
* write table to file	
******************************************

esttab no_iv no_iv_industry_year iv_model iv_industry_year_log using"$Path/scripts/stata_files/results/large.tex", replace indicate("Industry effects = a2016" "Year effects = a2020" "Industry-Year effects = a2019") nonotes substitute(\_ _) nogaps nogaps compress b(a3) se(a3) star(* 0.10 ** 0.05 *** 0.01) eqlabel(none) nodep label keep(wa_interest_rate_d log_wa_interest_rate_d wa_term_d log_wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate" log_wa_interest_rate_d "log(Interest rate)" wa_term_d "log(Term)" log_wa_term_d "Term"  inflation "Inflation rate" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte") mtitle("Log Loan Amount") stats(N r2 elas_int elas_int_SE elas_term elas_term_SE weak_iv_test1 weak_iv_test2, labels(Observations R-squared "elas_i" "elas_i_SE" "elas_t" "elas_t_SE" "F first stage" "F first stage"))