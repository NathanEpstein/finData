#' easily get financial price data
#'
#' @param vector string formatted stock tickers
#' @param string data collection start date
#' @param string data collection end date
#' @return list of matrices with price/trading data
#' @export
#' @examples
#' getData(c("FB","GS"),"2014-12-01","2015-01-01")

getData <- function(tickers,start,end){
  ENDMONTH = toString(as.numeric(format(as.Date(end),"%m")) - 1)
  ENDDAY = format(as.Date(end),"%d")
  ENDYEAR = format(as.Date(end),"%Y")
  STARTMONTH = toString(as.numeric(format(as.Date(start),"%m")) - 1)
  STARTDAY = format(as.Date(start),"%d")
  STARTYEAR = format(as.Date(start),"%Y")

  results = list()
  for (i in 1:length(tickers)){
    tckr = tickers[i]
    URL = paste("http://ichart.finance.yahoo.com/table.csv?s=",tckr,"&d=",ENDMONTH,"&e=",ENDDAY,"&f=",ENDYEAR,"&g=d&a=",STARTMONTH,"&b=",STARTDAY,"&c=",STARTYEAR,"&ignore=.csv",sep="")

    data = as.matrix(read.csv(URL))
    results[[i]] = data
  }
  return (results)
}

#' easily get stock return data
#'
#' @param vector string formatted stock tickers
#' @param number return period (1->daily returns, 21->monthly, 252->annual)
#' @param string data collection start date
#' @param string data collection end date
#' @param boolean get rolling returns? defaults FALSE
#' @return list of matrices with return data
#' @export
#' @examples
#' getReturns(c("FB","GS"),21,"2012-01-01","2015-01-01")

# look_back designates the number of (trading) days to compute returns over
# 1 = daily, 5 ~= weekly,  21 ~= monthly, 252 ~= annual
getReturns <- function(tickers,look_back,start,end,rolling=FALSE){
  data_list <- getData(tickers,start,end) #yahoo data matrix for each ticker
  results = list()
  if (rolling){
    # do this for each matrix in data_list (i in 1:length(data_list))
    for (i in 1:length(data_list)){
      m = dim(data_list[[i]])[1] - look_back
      returns = matrix(NA,m,2)
      for (j in 1:m){
        returns[j,1] = data_list[[i]][j,1]
        returns[j,2] = (as.numeric(data_list[[i]][j,7])/as.numeric(data_list[[i]][j+look_back,7])) - 1 #use adj. close prices
      }
      colnames(returns) = c("Date", "Return")
      results[[i]] = returns
    }
  }
  # if not rolling, we get independent segments of length look_back
  else{
    for (i in 1:length(data_list)){
      m = floor((dim(data_list[[i]])[1] - 1)/look_back)
      returns = matrix(NA,m,2)
      for (j in 1:m){
        returns[j,1] = data_list[[i]][1 + ((j-1)*look_back),1]
        returns[j,2] = (as.numeric(data_list[[i]][1 + ((j-1)*look_back),7])/as.numeric(data_list[[i]][1 + (j*look_back),7])) - 1 #use adj. close prices
      }
      colnames(returns) = c("Date", "Return")
      results[[i]] = returns
    }
  }
  return (results)
}

#' easily get financial asset covariance
#'
#' @param vector string formatted stock tickers
#' @param string data collection start date
#' @param string data collection end date
#' @return matrix with annualized variance/covariance of all assets
#' @export
#' @examples
#' getCovar(c("FB","GS"),"2014-12-01","2015-01-01")

getCovar <-function(tickers,start,end){
  returns = getReturns(tickers,1,start,end)
  M = matrix(NA,dim(returns[[1]])[1],length(returns))
  for (i in 1:length(returns)){
    M[,i] = as.numeric(returns[[i]][,2])
  }
  result = cov(M)
  for (j in 1:dim(result)[1]){
    for (k in 1:dim(result)[2]){
      result[j,k] = result[j,k]*252
    }
  }
  colnames(result) = tickers
  return (result)
}

#' easily get financial asset performance statistics
#'
#' @param vector string formatted stock tickers
#' @param string data collection start date
#' @param string data collection end date
#' @return list with matrices containing annualized return, annualized volatility, and maximum drawdown for each asset
#' @export
#' @examples
#' getStats(c("FB","GS"),"2014-12-01","2015-01-01")

#performance statistics from all assets over period considered
getStats <- function(tickers,start,end){
  returns = getReturns(tickers,1,start,end)
  results = list()
  for (i in 1:length(returns)){
    r = as.numeric(returns[[i]][,2])
    cum_r = prod(1+r)

    # annualized average return
    avg_r = (cum_r^(252/length(r))) - 1
    # annualized volatility
    vol = sqrt(252)*sd(r)

    #compute drawdowns
    dd = c()
    current = 1
    peak = 1
    for (j in length(r):1){
      current = (1 + r[j]) * current
      peak = max(current, peak)
      dd = append(dd, 1 - (current/peak))
    }

    #max drawdown is max of drawdowns
    max_dd = max(dd)

    stats = matrix(NA,1,3)
    stats[1,] = c(avg_r,vol,max_dd)
    colnames(stats) = c(paste(tickers[i],"Return"), paste(tickers[i],"Volatility"), paste(tickers[i],"Max Drawdown"))

    results[[i]] = stats
  }
  return (results)
}