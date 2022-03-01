 use "/Users/phuongthao/Documents/Biostatistics learning/adhere-bayesian-statistics-master/Data/BDHS.dta", clear

 
/*logistic regression*/
bayes: logistic antemed bord mage i.urban i.meduc i.islam i.wealth

/*diagnostics*/
bayesgraph diagnostics _all

/* Tat ca deu khong du hoi tu-> tang MCMC size*/
bayes, mcmcsize(15000) burnin(5000) thinning(5): logistic antemed bord mage i.urban i.meduc i.islam i.wealth

/*effect size*/
bayesstats ess

/* dien giai ket qua*/
bayesstats summary

/*Kiem dinh gia thuyet*/

	bayestest interval ({antemed: bord}, lower(0)) /*compVal*/
	bayestest interval {antemed: bord},  lower(-0.3) upper(0) /*ROPE*/
	
	bayestest interval ({antemed: mage}, lower(0)) /*compVal*/
	bayestest interval {antemed: mage},  lower(0) upper(0.3) /*ROPE*/
	
	bayestest interval ({antemed: urban}, lower(0)) /*compVal*/
	bayestest interval {antemed: urban},  lower(0) upper(0.1) /*ROPE*/
	
	bayestest interval ({antemed: meduc}, lower(0)) /*compVal*/
	bayestest interval {antemed: meduc},  lower(0) upper(1) /*ROPE*/
	
	bayestest interval ({antemed: islam}, lower(0)) /*compVal*/
	bayestest interval {antemed: islam},  lower(-0.2) upper(0) /*ROPE*/
	
	bayestest interval ({antemed: wealth}, lower(0)) /*compVal*/
	bayestest interval {antemed: wealth},  lower(0) upper(0.3) /*ROPE*/
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
