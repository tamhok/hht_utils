#utilities file for commonly used functions across different sets

#Trim row and column names out of data
remove.headers.footers <- function(data) {
  data.n = data[-1,-1]
  rownames(data.n) = data[-1,1]
  colnames(data.n) = data[1,-1] 
  return(data.n)
}

#' converts specific columns of a data frame according to fn.
convert.df  <-  function(dframe, cols=1:ncol(dframe), s=NA, fn=as.numeric) {
  if(!is.na(s)) {
	  cols = s:ncol(dframe)
  }
  nums=apply(dframe[,cols, drop=F], 2, fn)
  dframe[,cols]=nums
  return(dframe)
}

# Makes strings as factors false
df.nf <- function(mat) data.frame(mat, stringsAsFactors=FALSE)




