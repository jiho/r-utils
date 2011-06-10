#
#     Generic function to compute pairwise tests
#
#	x       data to be passed to the test
#	g       grouping (factor or coercible as such)
#	fun     function name
#	syntax  syntax of the function call:
#             - none:     pass several arguments
#             - list:     pass a list with the elements to be tested
#             - formula:  create a formula of the form values ~ grouping
#             - grouping: pass two arguments, a vector of values and a vector containing the grouping factor
#	p.adjust.method name of the p-value adjustment method,
#                   passed to p.adjust
#
# (c) Copyright 2011 Jean-Olivier Irisson
#     GNU General Public License v3
#
#------------------------------------------------------------

pairwise <- function(x, g, fun, syntax=c("none", "list", "formula", "grouping"), p.adjust.method="none") {
	# get the actual function
	fun = match.fun(fun)

	# fetch the function syntax
	syntax = match.arg(syntax)

	# find unique groupings
	if (!is.factor(g)) {
		g = factor(g)
	}
	ug = levels(g)
	ng = nlevels(g)

	# prepare output matrices
	stat = matrix(NA, nrow=ng-1, ncol=ng-1, dimnames=list(ug[-1], ug[-ng]))
	pval = stat

	# proceed with all tests
	for (j in 1:(ng-1)) {
		for (i in (j+1):ng) {
			# extract the data we need
			X = x[g == ug[i]]
			Y = x[g == ug[j]]

			# do the test
			if (syntax == "none") {
				res = fun(X, Y)
			} else if (syntax == "list") {
				res = fun(list(X, Y))
			} else {
				val = c(X, Y)
				group = factor(rep(c(ug[i], ug[j]), times=c(length(X), length(Y))))
				if (syntax == "formula") {
					res = fun(val ~ group, data.frame(val, group))
				} else if (syntax == "grouping") {
					res = fun(val, group)
				}
			}

			# extract statistic
			if (!is.null(res$statistic)) {
				stat[i-1,j] = res$statistic
			}
			# and p.value
			if (!is.null(res$p.value)) {
				pval[i-1,j] = res$p.value
			}
			# when they exist
		}
	}

	# adjust p-value
	pval[,] = p.adjust(pval, method=p.adjust.method)

	# prepare output list
	out = list()
	out$method = res$method
	out$p.value = pval
	out$statistic = stat
	out$p.adjust.method = p.adjust.method
	class(out) = "pairwise.htest"

	return(out)
}

