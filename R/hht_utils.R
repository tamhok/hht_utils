#utilities file for commonly used functions across different sets

#' Trim row and column names out of data
#' 
#' Removes row and column names and assigns them to row and column name
#' @param data matrix to remove rows and columns from
#' @return data with 1st row -> colname and 1st col -> rowname
remove.headers.footers <- function(data) {
  data.n = data[-1,-1]
  rownames(data.n) = data[-1,1]
  colnames(data.n) = data[1,-1] 
  return(data.n)
}

#' Data frame column converter
#" 
#' converts specific columns of a data frame according to fn.
#' @param dframe data frame 
#' @param cols columns (numerically) to convert, default is all
#' @param s if not NA, sets col to s:ncol(dframe) 
#' @param fn what function to apply (default as.numeric)
#' @return converted df
convert.df  <-  function(dframe, cols=1:ncol(dframe), s=NA, fn=as.numeric) {
  if(!is.na(s)) {
	  cols = s:ncol(dframe)
  }
  nums=apply(dframe[,cols, drop=F], 2, fn)
  dframe[,cols]=nums
  return(dframe)
}

#' data frame maker without factors
#' 
#' Makes data frame with strings as factors false
#' @param whatever you would send to data.frame anyways 
df.nf <- function(...) data.frame(..., stringsAsFactors=FALSE)




