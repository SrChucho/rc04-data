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
import delimited "$Path/Data/proc/panel_firms_G7_real_adhoc.csv"

*br



* recode variables
gen inst_code = string(institution_code,"%06.0f")
* gen scian_code = string(scian_code,"%02.0f")

* we take logarithms
replace mean_disposition = log(mean_disposition)
replace mean_disposition_r = log(mean_disposition_r)
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
label var mean_disposition_r "$L_r$"

label var log1p_wa_interest_rate_d_r "$\ln(Interest Rate+1)$"
label var asinh_wa_int_rate_d_r "$\text{arcsinh(Interest Rate}\text{)}$"


*  setup panel data 
quietly xtset scian_code

* Run regressions and save estimaes
* Base models
quietly xtivreg2 mean_disposition wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 [aweight = n_sect], cluster(scian_code) fe robust 
quietly estimates save "$Path/scripts/stata_files/results/log_lin", replace
est sto m_00

quietly xtivreg2 mean_disposition_r wa_interest_rate_d_r wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 [aweight = n_sect], cluster(scian_code) fe robust
quietly estimates save "$Path/scripts/stata_files/results/log_r_lin_r", replace
est sto m_11_no_trans

quietly xtivreg2 mean_disposition_r log1p_wa_interest_rate_d_r wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 [aweight = n_sect], cluster(scian_code) fe robust
quietly estimates save "$Path/scripts/stata_files/results/log_r_log1p_r", replace
est sto m_11_log1p

quietly xtivreg2 mean_disposition_r asinh_wa_int_rate_d_r wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 [aweight = n_sect], cluster(scian_code) fe robust
quietly estimates save "$Path/scripts/stata_files/results/log_r_asinh_r", replace
est sto m_11_asinh

* Results Table 1
*esttab m_00 m_11_no_trans m_11_log1p m_11_asinh, stats(N r2 r2_a F) 

eststo clear
scalar drop _all



* calculate sample mean for log(mean_disp)
quietly sum mean_disposition, meanonly
scalar mean_disp = r(mean)
* calculate sample mean for log(mean_disp)
quietly sum mean_disposition_r, meanonly
scalar mean_disp_r = r(mean)

*calculate sample mean for wa_interest_rate_d_r
quietly sum wa_interest_rate_d, meanonly
scalar mean_i = r(mean)
*calculate sample mean for wa_interest_rate_d_r
quietly sum wa_interest_rate_d_r, meanonly
scalar mean_i_r = r(mean)
*calculate sample mean for log1p_wa_interest_rate_d_r
quietly sum log1p_wa_interest_rate_d_r, meanonly
scalar mean_log1p_i_r = r(mean)
*calculate sample mean for asinh_wa_int_rate_d_r
quietly sum asinh_wa_int_rate_d_r, meanonly
scalar mean_asinh_i_r = r(mean)

*  following  bellemare and wichman (2020)
*  https://github.com/cjwichman/ihs/blob/master/script/makeTable3.do
*  see also https://blog.modelworks.ch/elasticities-in-estimated-linear-models-2/
*****************************************************
** Column 1 -- log mean_disposition, lin wa_interest_rate_d
*****************************************************

* elasticity between disposition and interest rate
est use "results/log_lin"
nlcom (_b[wa_interest_rate_d] * mean_i ), post //[math]\epsilon= be^{a+bX}\frac{X}{Y} = bY\frac{X}{Y} =bX[/math]
scalar e_mean_disp_mean_i = _b[_nl_1]
scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
	est use "results/log_lin"		
	qui estadd scalar elas = e_mean_disp_mean_i
	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
	qui eststo
*****************************************************
** Column 2 -- log mean_disposition_r, lin wa_interest_rate_d_r
*****************************************************

* elasticity between disposition and interest rate
est use "results/log_r_lin_r"
nlcom (_b[wa_interest_rate_d_r] * mean_i  ), post //[math]\epsilon= be^{a+bX}\frac{X}{Y} = bY\frac{X}{Y} =bX[/math]
scalar e_mean_disp_mean_i = _b[_nl_1]
scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
	est use "results/log_r_lin_r"		
	qui estadd scalar elas = e_mean_disp_mean_i
	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
	qui eststo


*****************************************************
** Column 3 -- log mean_disposition_r, log (wa_interest_rate_d_r + 1)
******************************************************

* elasticity between disposition and interest rate
est use "results/log_r_log1p_r"
nlcom (_b[log1p_wa_interest_rate_d_r] ), post // [math]\epsilon= \frac{bY}{X}\frac{X}{Y} =b[/math]
scalar e_mean_disp_mean_i = _b[_nl_1]
scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
	est use "results/log_r_log1p_r"		
	qui estadd scalar elas = e_mean_disp_mean_i
	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
	qui eststo


*****************************************************
** Column 4 -- log mean_disposition_r, asinh (wa_interest_rate_d_r)
******************************************************

* elasticity between disposition and interest rate
est use "results/log_r_asinh_r"
nlcom (_b[asinh_wa_int_rate_d_r] * (mean_i/ sqrt(mean_i^2+1)) / mean_disp_r ), post
scalar e_mean_disp_mean_i = _b[_nl_1]
scalar e_mean_disp_mean_i_SE = _se[_nl_1]

* store variables
	est use "results/log_r_asinh_r"		
	qui estadd scalar elas = e_mean_disp_mean_i
	qui estadd scalar elas_SE = e_mean_disp_mean_i_SE
	qui eststo
	
******************************************	
* write table to file	
******************************************


esttab using "results/reg1.tex", indicate("Sector effects = a2016" "Bank effects = a2017" "Year effects = a2018") replace nonotes substitute(\_ _) nogaps compress b(a3) se(a3) ar2(a3) star(* 0.10 ** 0.05 *** 0.01) eqlabel(none) nodep label keep(wa_interest_rate_d wa_interest_rate_d_r log1p_wa_interest_rate_d_r asinh_wa_int_rate_d_r wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate" wa_interest_rate_d_r "Real interest rate" log1p_wa_interest_rate_d_r "log(Real interest rate + 1)" asinh_wa_int_rate_d_r "asinh(Real interest rate )" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte") mtitle("Nominal Credit" "Real Credit" "Real Credit" "Real Credit") scalars("aic AIC" "bic BIC"  "a \hline" "elas  $\xi(\text{L},\text{Interes Rate})$" "elas_SE \hspace{5mm} Std. Err.") sfmt(a3)


*esttab m_00 m_11_log1p m_11_asinh using "$Path/weekly report/stata_results/fe_estimations.tex", se obslast scalar(F) r2 keep(wa_interest_rate_d wa_interest_rate_d_r wa_term_d citibanamex santander hsbc inbursa scotiabank banorte)

* IV models
*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 (wa_interest_rate_d = num_atm)  [aweight = n_sect], cluster(scian_code)fe robust ffirst

* xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = wa_atm)  [aweight = n_sect], cluster(scian_code) fe robust ffirst liml  // to see first stage results

*est sto mIV1
* di e(F)

*quietly xtivreg2 mean_disposition_r  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 (wa_interest_rate_d = num_est_pos)  [aweight = n_sect], cluster(scian_code) fe robust
*est sto mIV2

*quietly xtivreg2 mean_disposition_r  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 (wa_interest_rate_d = num_branches)  [aweight = n_sect], cluster(scian_code) fe robust
*est sto mIV3

*quietly xtivreg2 mean_disposition_r  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 (wa_interest_rate_d = num_transactions_atm)  [aweight = n_sect], cluster(scian_code) fe robust
*est sto mIV4

*quietly xtivreg2 mean_disposition_r  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte a2016 a2017 a2018 a2019 a2020 a2021 (wa_interest_rate_d = total_staff)  [aweight = n_sect], cluster(scian_code) fe robust
*est sto mIV5
 
* Results Table 2
*esttab m0_w_1 mIV1 mIV2 mIV3 mIV4 mIV5, se obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) mtitle("Base Model (Weights)" "IV:ATM" "IV:TPV" "IV:BRANCHES" "IV:ATM_TRANS" "IV:STAFF") 


*estimates store base_fixed
*esttab m0_w_1 mIV1 mIV2 using "$Path/weekly report/stata_results/iv_estimationsA.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("Base Model (Weights)" "IV:ATM" "IV:TPV") replace 


*estimates store base_fixed
*esttab mIV3 mIV4 mIV5 using "$Path/weekly report/stata_results/iv_estimationsB.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("IV:BRANCHES" "IV:ATM_TRANS" "IV:STAFF") replace 



* IV models IV1
*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_atm num_est_pos)  [aweight = n_sect], cluster(scian_code)fe robust
est sto mIV12

*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_atm num_branches)  [aweight = n_sect], cluster(scian_code)fe robust
est sto mIV13

*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_atm num_transactions_atm)  [aweight = n_sect], cluster(scian_code)fe robust
est sto mIV14

*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_atm total_staff)  [aweight = n_sect], cluster(scian_code)fe robust
est sto mIV15

*esttab mIV1 mIV12 mIV13 mIV14 mIV15, se obslast scalar(F) r2  keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) mtitle("IV1" "IV:atm tpv" "IV:atm branches" "IV:atm num_trans" "IV:atm staff") 

*esttab mIV1 mIV12 mIV13 mIV14 mIV15 using "$Path/weekly report/stata_results/iv_estimationsIV1.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("IV1" "IV:atm tpv" "IV:atm branches" "IV:atm num_trans" "IV:atm staff") 

* IV models IV2
*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_est_pos num_branches)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV23

*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_est_pos num_transactions_atm)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV24

*quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_est_pos total_staff)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV25

*esttab mIV2 mIV23 mIV24 mIV25, se obslast scalar(F) r2  keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) mtitle("Base Model (Weights)" "IV:tpv branches" "IV:tpv num_trans" "IV:tpv staff") 


*esttab mIV2 mIV23 mIV24 mIV25 using "$Path/weekly report/stata_results/iv_estimationsIV2.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("Base Model (Weights)" "IV:tpv branches" "IV:tpv num_trans" "IV:tpv staff") 

* IV models IV3
quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_branches num_transactions_atm)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV34

quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_branches total_staff)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV35

esttab  mIV3 mIV34 mIV35, se obslast scalar(F) r2  keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) mtitle("Base Model (Weights)" "IV:branches num_trans" "IV:branches staff") 

esttab mIV3 mIV34 mIV35 using "$Path/weekly report/stata_results/iv_estimationsIV3.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("Base Model (Weights)" "IV:branches num_trans" "IV:branches staff") 

* IV models IV4
quietly xtivreg2 mean_disposition  wa_term_d citibanamex santander hsbc inbursa scotiabank banorte (wa_interest_rate_d = num_transactions_atm total_staff)  [aweight = n_sect], cluster(scian_code) fe robust
est sto mIV45
 
esttab mIV4  mIV45, se obslast scalar(F) r2  keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) mtitle("Base Model (Weights)"  "IV:num_transactions_atm staff") 


esttab mIV4 mIV45 using "$Path/weekly report/stata_results/iv_estimationsIV4.tex", se  obslast scalar(r2_a) keep(wa_interest_rate_d wa_term_d citibanamex santander hsbc inbursa scotiabank banorte) coeflabel(wa_interest_rate_d "Interest rate (log)" wa_term_d "Term (months)" citibanamex "Citibanamex" santander "Santander" hsbc "HSBC" inbursa "Inbursa" scotiabank "Scotiabank" banorte "Banorte")  label nonumber title("IV Models of Credit Disposition (log)") mtitle("Base Model (Weights)"  "IV:num_transactions_atm staff") 





