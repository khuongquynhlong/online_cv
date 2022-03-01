*mat do xuong va tuoi
bayes:  reg fnbmd age
bayesgraph diagnostics { fnbmd : age }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd age
bayesgraph diagnostics { fnbmd : age }
bayesstats ess
bayestest interval ({ fnbmd : age }, upper(0))
*mat do xuong va can nang
bayes:  reg fnbmd weight 
bayesgraph diagnostics { fnbmd : weight }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd weight
bayesgraph diagnostics { fnbmd : weight }
bayesstats ess
bayestest interval ({ fnbmd : weight }, lower(0))
*mat do xuong va ccao
bayes:  reg fnbmd height
bayesgraph diagnostics { fnbmd : height }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd height
bayesgraph diagnostics { fnbmd : height }
bayesstats ess
bayestest interval ({ fnbmd : height}, lower(0))
*mat do xuong va luong canxi
bayes:  reg fnbmd ca 
bayesgraph diagnostics { fnbmd : ca }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd ca
bayesgraph diagnostics { fnbmd : ca }
bayesstats ess
bayestest interval ({ fnbmd : ca}, lower(0))
*mat do xuong va hut thuoc
bayes:  reg fnbmd smoking
bayesgraph diagnostics { fnbmd : smoking }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd smoking
bayesgraph diagnostics { fnbmd : smoking }
bayesstats ess
bayestest interval ({ fnbmd : smoking}, upper(0))
*mat do xuong va tu vong
bayes:  reg fnbmd death 
bayesgraph diagnostics { fnbmd : death }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd death
bayesgraph diagnostics { fnbmd : death }
bayesstats ess
bayestest interval ({ fnbmd : death}, upper(0))
*mat do xuong va v25d
bayes:  reg fnbmd v25d 
bayesgraph diagnostics { fnbmd : v25d}
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd v25d
bayesgraph diagnostics { fnbmd : v25d}
bayesstats ess
bayestest interval ({ fnbmd : v25d}, lower(0))
*mat do xuong va pth
bayes:  reg fnbmd pth 
bayesgraph diagnostics { fnbmd : pth }
bayes, mcmcsize(15000)  burnin(5000) thinning(5):  reg fnbmd pth
bayesgraph diagnostics { fnbmd : pth }
bayesstats ess
bayestest interval ({ fnbmd : pth}, lower(0))

