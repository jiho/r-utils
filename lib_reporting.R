#
#     Functions to help view, save, and report results
#
# (c) Copyright 2011 Jean-Olivier Irisson
#     GNU General Public License v3
#
#------------------------------------------------------------

print.signif <- function(x, stars=TRUE, digits=3)
#
#   Print a p-value table highlighting only the significant ones
#   x       p-value table (such as one coming from a pairwise call)
#   stars   boolean, if TRUE print stars instead of numeric values
#   digits  number of significant digits of the p-values
#
{
	if (stars) {
	    # generate star levels
		out = symnum(x, cutpoints=c(0, 0.001, 0.01, 0.05, 1), symbols=c("***", "**", "*", "-"), na=FALSE)
	} else {
	    # round p-values to the requested number of digits
		out = round(x, digits=digits)
		# remove un-significant ones
		sig = x <= 0.05
		out[is.na(sig)] = ""
		out[!sig] = "-"
		class(out) = "noquote"
	}
	return(out)
}

write.out <- function(...)
#
#   Write each of the arguments to a CSV file named after the argument
#   ...     data.frames to write to csv files
#
{
    # get arguments
    vars <- list(...)
    
    # get names of arguments
    varNames <- sapply(substitute(list(...))[-1], deparse)
    
    # write each in a csv file
    for (i in 1:length(vars)) {
        write.table(vars[[i]], file=paste(varNames[i], ".csv", sep=""), sep=",", row.names=FALSE)
    }
    
    # return a dummy value
    return(invisible())        
}
