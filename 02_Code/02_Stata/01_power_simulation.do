/*******************************************************************************
@Name: power_simulation.do

@Author: Marco Medina

@Date: 30/08/2022

@In: 
	 
@Out: 
*******************************************************************************/

********************
version 17.0
clear all
cd "$directory"
********************

********************************
* Number of incoming casefiles *
********************************

* Import jobs_data.csv
import delimited "01_Data/03_Working/jobs_data.csv", clear

* Clean fecha_sirede
keep fecha_sirede
gen aux_fecha_sirede = date(fecha_sirede, "DMY")
drop fecha_sirede
rename aux_fecha_sirede fecha_sirede
format fecha_sirede %td
keep if fecha_sirede < td(27aug2022)

* Count the number of incoming casefiles per day
egen num_exp = count(fecha_sirede), by(fecha_sirede)
duplicates drop
twoway bar num_exp fecha_sirede
qui summarize num_exp
di as text "media diaria = " as result r(mean)

* Count the number of incoming casefiles by week, and day of the week
gen sem_sirede = week(fecha_sirede)
bysort sem_sirede: egen num_exp_sem = sum(num_exp)
twoway bar num_exp_sem sem_sirede
qui summarize num_exp_sem
di as text "media semanal = " as result r(mean)

gen dia_sirede = dow(fecha_sirede)
bysort dia_sirede: egen num_exp_dia = mean(num_exp)
twoway bar num_exp_dia dia_sirede



*********************
* Power Calculation *
*********************

timer clear 1
timer on 1

* Potential sample size: 3730 + 5 * 404 (actual + weeks_left * weekly_avg) = 5750
local n = 5750

* Number of replications
local m = 1000

* Baseline prob dependent variable: 30% of cases "concilian"
local p_baseline = 0.3

* Probability of treatment
local p_treat = 2/3

* Probability of take up: 0.82 * 0.26 * 0.19 = 0.04 
* Cases we are able to contact * Cases with an appointment * Cases where both parts assist the appointment)
*local p_takeup = 0.04
local takeup_probs = "0.02(0.02)0.2"
local length_takeup_probs = 10

* Significance level
local signif = 0.051

* Grid for beta
local betas "0.05(0.05)0.3"
local length_betas = 6
local beta_sd = 0.01

********************************************************************************
********************************************************************************


local np =`length_takeup_probs'*`length_betas'*`m'

di "--------"
di "Número de procesos:"
di `np'
di "--------"
		
local pb = 0

* Define matrices in which power is stored 
matrix pwr = J(`length_takeup_probs',`length_betas',0)

local j = 0
forvalues beta = `betas' {
	local j = `j' + 1

	local i = 0
	forvalues takeup_prob = `takeup_probs' {
		local i = `i' + 1

		forvalues t = 1/`m' {
			local pb = `pb' + 1

			clear 
			qui set obs `n'

			* Simulate a normal dist from where the effect is going to be drawn
			qui gen yy = rnormal(`beta', `beta_sd')

			* Exogenous randomization
			qui gen z = (uniform()<=`p_treat')

			* Take up. For actual observations is fixed. For future observations can vary.
			*qui gen t = .
			*qui replace t = (uniform() <= `p_takeup') if z == 1 & [_n] <= 3730 
			*qui replace t = (uniform() <= `takeup_prob') if z == 1 & [_n] > 3730 
			
			* Take up. For all observations is not fixed
			qui gen t = (uniform() <= `takeup_prob') if z == 1

			* Simulate dep var
			qui gen y = .

			* Baseline dep var probability
			qui replace y = (uniform() <= 0.3) if z == 0 

			* Baseline dep var probability for cases without take up
			qui replace y = (uniform() <= 0.3) if z == 1 & t == 0 

			* ATT
			qui replace y = (uniform() <= 0.3 + yy) if z == 1 & t == 1


			* Reg
			qui reg y z, r
			qui test _b[z]=0
			matrix pwr[`i',`j'] = pwr[`i',`j'] + (`r(p)'<=`signif')


			*Progress bar

			if `pb' == 1 {
				di "Progress"
				di "--------"
			}
			if `pb' == floor(`np'/10) {
				di "10%"
			}
			if `pb' == floor(`np'*2/10) {
				di "20%"
			}
			if `pb' == floor(`np'*3/10) {
				di "30%"
			}
			if `pb' == floor(`np'*4/10) {
				di "40%"
			}
			if `pb' == floor(`np'*5/10) {
				di "50%"
			}
			if `pb' == floor(`np'*6/10) {
				di "60%"
			}
			if `pb' == floor(`np'*7/10) {
				di "70%"
			}
			if `pb' == floor(`np'*8/10) {
				di "80%"
			}
			if `pb' == floor(`np'*9/10) {
				di "90%"
			}
			if `pb' == floor(`np'-10) {
				di "100%"
				di "--------"
				di "        "
			}

		}

		matrix pwr[`i',`j']=pwr[`i',`j']/`m'
	}
}


clear
qui svmat pwr

export delimited using "01_Data/03_Working/jobs_power_simulation.csv", replace novarnames

timer off 1
timer list



***************************
* Power Calculation Graph *
***************************

* Import data from power calculation
import delimited "01_Data/03_Working/jobs_power_simulation.csv", clear

* Gen takeup variable
gen takeup = (_n)*0.02

twoway (line v1 takeup, lwidth(medthick)) ///
	   (line v2 takeup, lwidth(medthick)) ///
	   (line v3 takeup, lwidth(medthick)) ///
	   (line v4 takeup, lwidth(medthick)) ///
	   (line v5 takeup, lwidth(medthick)) ///
	   (line v6 takeup, lwidth(medthick)) ///
	   ,graphregion(color(white)) ///
	   xtitle("Take up") ///
	   ytitle("Statistical power") ///
	   legend(order(1 "5%" 2 "10%" 3 "15%" 4 "20%" 5 "25%" 6 "30%") rows(2) subtitle("Effect Size")) ///
	   xlabel(0.02 "2%" 0.04 "4%" 0.06 "6%" 0.08 "8%" 0.1 "10%" 0.12 "12%" 0.14 "14%" 0.16 "16%" 0.18 "18%" 0.2 "20%")
	   
graph export "04_Figures/jobs_power_simulation.pdf", replace

