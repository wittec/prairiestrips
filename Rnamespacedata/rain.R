#' Rain data
#' 
#' Contains rain measurements in five-minute intervals
#' on each property during experimental season
#' 
#' @format A data.frame with the following columns:
#'   
#'   \code{watershed}: the watershed were rain measurements were collected. 
#'   Only one watershed per site was used, so this is considered to be the rain
#'   at the site.
#'   
#'   \code{date_time}: date and time of data-point collection 
#'   (YYYY-mm-dd hh:mm:ss)
#'   
#'   \code{rain-m}: rain measured using a tipping bucket-style rain gage. The 
#'   tipping vol of the bucket was 0.001m of rain and thus this is resolution
#'   of the rain measurements.
#'   
#' @description Need a general description here.
#' 
"rain"
