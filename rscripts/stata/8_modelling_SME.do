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
import delimited "$Path/Data/proc/panel_firms_G7SME.csv"

*br



* recode variables
gen inst_code = string(institution_code,"%06.0f")
* gen scian_code = string(scian_code,"%02.0f")

* we take logarithms
gen log_mean_disposition = log(mean_disposition)
*replace mean_disposition_r = log(mean_disposition_r)
replace wa_term_d = asinh(wa_term_d)

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


*  setup panel data 
quietly xtset scian_code

* Run regressions and save estimaes
* Base models
quietly xtivreg2 log_mean_disposition wa_interest_rate_d wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 [aweight = n_sect], cluster(scian_code)  fe robust
quietly estimates save "$Path/scripts/stata_files/results/log_lin", replace
est sto m_00


*quietly xtivreg2 mean_disposition wa_interest_rate_d log_wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020  [aweight = n_sect], cluster(scian_code) fe robust 
*quietly estimates save "$Path/scripts/stata_files/results/lin_lin", replace
*est sto m_lin_lin

* Results Table 1
* estimates table m_00 m_lin_lin 

eststo clear
scalar drop _all


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


* for lin-lin
* calculate sample mean for mean_disposition
*quietly sum mean_disposition, meanonly
*scalar mymean_disp = r(mean)
*calculate sample mean for wa_interest_rate_d
*quietly sum wa_interest_rate_d, meanonly
*scalar mymean_i = r(mean)



*  following  bellemare and wichman (2020)
*  https://github.com/cjwichman/ihs/blob/master/script/makeTable3.do
*  see also https://blog.modelworks.ch/elasticities-in-estimated-linear-models-2/
*****************************************************
** Column 1 -- log mean_disposition, wa_interest_rate_d
*****************************************************

* elasticity between disposition and interest rate
est use "results/log_lin"
nlcom (_b[wa_interest_rate_d] * (mymean_i) ), post 
scalar e_mean_disp_mean_i = _b[_nl_1]
scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
	est use "results/log_lin"		
	qui estadd scalar elas = e_mean_disp_mean_i
	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
	qui eststo

* elasticity between disposition and term
est use "results/log_lin"
nlcom (_b[wa_term_d] * (mymean_term) ), post 
scalar e_mean_disp_mean_term = _b[_nl_1]
scalar e_mean_disp_mean_term_SE = _se[_nl_1]

* store variables
	est use "results/log_lin"		
	qui estadd scalar elas_t = e_mean_disp_mean_term
	qui estadd scalar elas_t_SE = e_mean_disp_mean_term_SE
	qui eststo
		
	
*****************************************************
** Column 2 -- log mean_disposition_r, lin wa_interest_rate_d_r
*****************************************************

* elasticity between disposition and interest rate
*est use "results/lin_lin"
*nlcom (_b[wa_interest_rate_d] * (mymean_i / mymean_disp))  , post 
*scalar e_mean_disp_mean_i = _b[_nl_1]
*scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
*	est use "results/lin_lin"		
*	qui estadd scalar elas = e_mean_disp_mean_i
*	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
*	qui eststo


******************************************	
* write table to file	
******************************************


esttab using "results/reg1_SME.tex", indicate("Sector effects = a2016" "Year effects = a2018") replace nonotes substitute(\_ _) nogaps compress b(a3) se(a3) ar2(a3) star(* 0.10 ** 0.05 *** 0.01) eqlabel(none) nodep label keep(wa_interest_rate_d wa_term_d inflation citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate" wa_term_d "Term" inflation "Inflation rate" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte") mtitle("Log Loan Amount") scalars("a \hline" "elas \hspace{5mm} Price elasticity" "elas_SE \hspace{5mm} Std. Err." "elas_t \hspace{5mm} Term elasticity" "elas_t_SE \hspace{5mm} Std. Err.") sfmt(a3)





