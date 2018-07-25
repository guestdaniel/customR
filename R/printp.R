#' Return string with formatted p-value
#'
#' @param val A p-value to format.
#' @return A string formatted in APA style with the =/< sign and the p-value

print <- function(val) {
		if (val == 1) {
			return("= 1.00") 
		} else if (val < 0.001) {
			return("< 0.001")
		} else {
			return(paste0("= ", as.character(signif(val, digits=2))))
		}
}

