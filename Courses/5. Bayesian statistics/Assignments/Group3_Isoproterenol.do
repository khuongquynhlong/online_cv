clear
set more off
use "Isoproterenol.dta", clear

* n = 22 obs
count
* check missing - missing 1 at doses of 10, 20, 150 and 300 mg
mdesc
br if fbf10==. // missing tren cung 1 nguoi

* Explore variance-covariance matrix
corr fbf0 fbf10 fbf20 fbf60 fbf150 fbf300 fbf400, covariance

* Explore correlation matrix
corr fbf0 fbf10 fbf20 fbf60 fbf150 fbf300 fbf400

* Mo ta su thay doi luu luong mau cang tay
parplot fbf0-fbf400, transform(raw)
parplot fbf0-fbf400, transform(raw) by(race)

* reshape outcomes
reshape long fbf, i(id) j(dose) 
br

* HYPOTHESIS 1

*********** Xac dinh prior // mac dinh

*********** Xay dung mo hinh Bayes
* Xay dung mo hinh rong

* them phan random intercept
bayes: mixed fbf || id:

* mo hinh co random slope
bayes,dots melabel: mixed fbf dose || id: dose, cov(unstructured) 

************ chan doan MCMC
bayesgraph trace {fbf: dose}
bayesgraph ac {fbf: dose}
bayesgraph hist {fbf: dose}, norm
bayesgraph diagnostics {fbf: dose}

* Effect sample size
bayesstats ess

* Tang chuoi MCMC cho mo hinh
bayes, mcmcsize(100000) burnin(5000) thinning(5): ///
 mixed fbf dose || id: dose, cov(unstructured) 
* back lai chan doan model

************ dien giai ket qua
bayesstats summary

************ kiem dinh gia thuyet
* Compval
bayestest interval ({fbf: dose}, lower(0))

* Rope
bayestest interval {fbf: dose}, lower(0) upper(0.1)

* Bayes factor


*** hypothesis 2
* tao bien tuong tac giua race va dose
gen race_dose = race*dose

* add vao model
bayes: mixed fbf dose race || id: dose, cov(unstructured) 
