#' Convert degrees minutes seconds string to decimal degrees
#'
#' @param s string of 1-3 coordinate chunks separated by a non-text elements.
#'
#' @return float
#'
#' @description Will automatically look for W, S, N, E to identify positive or negative orientation
#'
#' @examples
#' string <- "113°30′04.24 W"
#' dms2dec(string)
#'
dms2dec <- function(s) {


	if(grepl('W|S', toupper(s))){
		coef = -1
	} else {
		coef <- 1
	}

	ss <- strsplit(s, '[^0-9.-]')[[1]]

	coord <- as.numeric(ss)[1] + as.numeric(ss[2])/60
	if(!is.na(as.numeric(ss[3]))) coord <- coord + as.numeric(ss[3])/60/60
	coord * coef
}



#' Write table to clipboard
#'
#' Helper function for transfering between excel and R
#'
#' @param d any object writable to write.table
#'
#' @return tab separated table copied to clipboard
#'
#'
toclip <- function(d){
	write.table(d, 'clipboard', sep = '\t', row.names=FALSE)
}


#' copied excel to data.frame
#'
#' Helper function for transfering between excel and R
#'
#'
#' @return data.frame of tab-separated array copied from clipboard
#'
#'
fromclip <- function(...){
  read.table('clipboard', sep = '\t', ...)
}


