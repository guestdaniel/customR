#' Take a list containing model/test objects and return them with jointly corrected p-values 
#'
#' @param objs A list of objects to correct
#' @return A list of objects with updated p values
#' @export

correct_p <- function(objs) {
	# Identify number of rows and the column containing p-values in each object
	n_rows = sapply(objs, nrow)
	col_names = sapply(objs, colnames)
	col_arrays = sapply(sapply(col_names, FUN=stringr::str_detect, pattern="Pr\\(>"), as.numeric)
	col_idxs = sapply(col_arrays, which.max)
	# Extract p values
	p_values = numeric()
	for (ii in 1:length(objs)) {
			p_values = c(p_values, objs[[ii]][, col_idxs[ii]])
	}
	# Correct p values
	p_values = p.adjust(p_values, method="holm")
	# Replace p values
	idx = 1
	for (ii in 1:length(objs)) {
			objs[[ii]][, col_idxs[ii]] = p_values[idx:(idx+n_rows[ii]-1)]
			idx = idx + n_rows[[ii]]
	}
	return(objs)
}

