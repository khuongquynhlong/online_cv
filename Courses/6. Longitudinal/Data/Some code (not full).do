* Do file: Analyse CH2 baseline to endline data*

clear all
set more off


*** install packages

	capture which psmatch2
	if _rc == 111 {
	ssc install psmatch2
	}
	capture which st0026_2.pkg //pscore pkg
	if _rc == 111 {
	net install st0026_2.pkg
	}
	capture which pbalchk
	if _rc == 111 {
	net install pbalchk.pkg
	}



********** Create data for analysing, PSM và IPW  ************
use "$rawdata/CH2_baseline_1FU_2FU_3FU_endline_long.dta", clear


*----- Create dummy_treatment variable
gen trt = .
replace trt = 0 if site == 2
replace trt = 1 if site == 1
lab def trt 0 "Control" 1"Intervention"
lab val trt trt
tab trt site

*------ calculate propensity score for matching
* Create dummy variables
tab nhomtuoi, gen(nhomtuoi)
tab edu, gen(eduyn)
tab job, gen(job)
tab smoke, gen(smoke)

set seed 12345
pscore trt a1 a3 nhomtuoi2 nhomtuoi3 eduyn2 eduyn3 eduyn4 eduyn5 eduyn6 job2 job3 job4 job5 ///
			job6 marriage smoke2 smoke3 nonalc if round == 0, pscore(mypscore) blockid(myblock) logit detail 

* Visualize Pscore
psgraph, treated(trt) pscore(mypscore)

gen logitpscore = ln(mypscore/(1-mypscore)) // calculate logit score of Pscore to calculated caliper

* save data matching for futher analysis
sum logitpscore
di r(sd)
local caliper = r(sd) * 0.2
di `caliper'

psmatch2 trt if round == 0, pscore(mypscore) caliper(`caliper') noreplace neighbor(1)
ren _weight psmatch
replace psmatch = 0 if psmatch ==. & round ==0

* coppy psmatch to all round
	
forvalue i = 1/2701 {
	su psmatch if round == 0 & group == `i', meanonly 
	local x = r(mean) 
	replace psmatch = `x' if group == `i'
}



*------ calculate propensity score for IPW
logit trt a1 i.nhomtuoi i.edu i.job marriage i.smoke nonalc if round == 0

set seed 12345
predict ipw if round == 0
replace ipw = 1/ipw if trt==1
replace ipw = 1/(1-ipw) if trt==0
sum ipw

forvalue i = 1/2701 { 
		su ipw if round == 0 & group == `i', meanonly 
		local x = r(mean)
		replace ipw = `x' if group == `i'
}
	
save "$rawdata/alldata.dta", replace


*------------------------ Some code for analysis (not full)

use "$rawdata/alldata.dta", clear

lab def yesno 0 "No" 1"Yes"

*----- coppy BMI and BMI cat from baseline to all round
foreach zzz of varlist bmi bmicat { 
		 forvalue i = 1/2701 {
			su `zzz' if round == 0 & group == `i', meanonly 
			local x = r(mean) 
			replace `zzz' = `x' if group == `i'
		}
}

*** Create interaction term of round and treatment, it is also know as DID coefficient

************************* Participant characteristics **************************
*** Table 1
*----- General characteristics of the study sample at T0
table1 if round==0, vars(a1 cat\ tuoi_n conts\ nhomtuoi cat\ ///
								a3 cat\ marriage cat\ education cat\ job cat) ///
								format(%9.2f) onecol ///
								saving("D:/Dropbox/Long/Projects/CH2/bs_site_demographic.xls", sheet (Overall, replace))
								
table1 if round==0, by(site) vars(a1 cat\ tuoi_n conts\ nhomtuoi cat\ ///
								a3 cat\ marriage cat\ education cat\ job cat) ///
								format(%9.2f) onecol ///
								saving("D:/Dropbox/Long/Projects/CH2/bs_site_demographic.xls", sheet (Bysite, replace))
*
table1 if round==0 & psmatch ==1, by(site) vars(a1 cat\ tuoi_n conts\ nhomtuoi cat\ ///
								a3 cat\ marriage cat\ education cat\ job cat \ ///
								smoke cat \ nonalc cat\ bmicat cat ) ///
								format(%9.2f) onecol 
								
*----- Medical history, smoking and alcohol consumption -----


*----- Self-management tool  (Indicator 3.5)
* 3.5	Percentage of hypertensive adults who are using self-management tool to ///
* support treatment adherence 

gen indi35 = f6
replace indi35 = 0 if indi35==2

foreach var of varlist f6x1 f6x2 f6x3 f6x4 f6x5 f6x6 {
	replace `var' = 0 if `var' ==2
}
egen f6_score = rsum(f6x1 f6x2 f6x3 f6x4 f6x5 f6x6), missing

replace indi35 = 1 if f6_score > 0 & f6_score ~=.
replace indi35 = 0 if f6_score == 0

lab val indi35 yesno


table1 if site ==1, by(round) vars (indi35 cat) ///
					one format(%9.2f) ///
					saving("D:/Dropbox/Long/Projects/CH2/Indi3.5.xls",sheet (Intervention, replace))

table1 if site ==2, by(round) vars (indi35 cat) ///
					one format(%9.2f) ///
					saving("D:/Dropbox/Long/Projects/CH2/Indi3.5.xls",sheet (Control, replace))

* matched data

table1 if site ==1 & psmatch ==1, by(round) vars (indi35 cat) ///
					one format(%9.2f) ///
					saving("D:/Dropbox/Long/Projects/CH2/Indi3.5.xls",sheet (Inter_m, replace))

table1 if site ==2 & psmatch ==1, by(round) vars (indi35 cat ) ///
					one format(%9.2f) ///
					saving("D:/Dropbox/Long/Projects/CH2/Indi3.5.xls",sheet (Control_m, replace))

*** grap including baseline
preserve
		collapse (mean) indi35 (sum) n = cons, by(round site)
		
		gen se = sqrt(indi35*(1-indi35)/n)
		gen ul = indi35 + 1.96*se
		gen ll = indi35 - 1.96*se
		replace indi35 = indi35*100
		replace ul = ul * 100
		replace ll = ll * 100
		
		format indi35 %4.1f
		twoway ///
		(connected indi35 round if site == 1, ///
								jitter(1) mlabel(indi35) mlabposition(1) ///
								msymbol(smcircle) lcolor(blue) mcolor(blue)) ///
		(connected indi35 round if site == 2, ///
								jitter(1) mlabel(indi35) mlabposition(1) ///
								msymbol(smcircle) lcolor(red) mcolor(red)) ///
		(rcap ul ll round if site==1, lcolor(blue)) ///
		(rcap ul ll round if site==2, lcolor(red)), ///
		yline(140, lpattern(shortdash) lcolor(black)) ///
		title({bf: % Using self-management tool to support treatment adherence}, size(medium)) ///
		ytitle(%, size(medsmall)) ///
		xtitle("", size(medsmall)) ///
		graphregion(fcolor(white)) /// 
		ylabel(0 "0" 20 "20" 40"40" 60 "60" 80 "80" 100 "100", labsize(small) angle(0)) ///
		xlabel(0 "Baseline" 1 "Follow-up 1" 2 "Follow-up 2" 3 "Follow-up 3" 4 "Endline", labsize(small)) ///
		legend(notextfirst order(1 "Intervention" 2 "Control"))
		
		graph export "D:/Dropbox/Long/Projects/CH2/Figure2.png", width(1850) replace	
restore

*** mixed effect model
* test scenario
melogit indi35 i.trt##i.round if psmatch ==1 || macanhan :, cov(unstructured) or
eststo modelA

melogit indi35 i.trt i.round if psmatch ==1  || macanhan : round, cov(unstructured) or
eststo modelB

lrtest modelA modelB // use random slope
eststo clear

*** Multivariable model using matched sample (for comparision)
melogit indi35 i.trt##i.round a1 i.nhomtuoi  i.a3 i.marriage i.education2 ///
						ib3.job i.smoke i.nonalc ib1.bmicat if psmatch ==1 || macanhan : round, or

*** Multivariable model using Propensity matching
eststo: melogit indi35 i.trt##i.round if psmatch ==1  || macanhan : round, cov(unstructured) or
		
esttab using "D:/Dropbox/Long/Projects/CH2/indicator3.5.rtf", b(2) ci(2) replace /// 
	wide label eform nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N)
	eststo clear
	

