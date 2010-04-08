require.local <- function(fun, lib, quietly=FALSE)
#
#	Searches for function "fun" and sources "lib" in 
#	case it is not found
#
#	fun 	name of a function, unquoted
#	lib 	path to the R file containing the function, is sourced when the function does not exist
#
{
	if (! (deparse(substitute(fun)) %in% ls(".GlobalEnv") && class(fun) == "function") ) {
	    if (!quietly) {
    		cat("Sourcing", lib,"...\n")	       
	    }
		source(lib)
	}
}
