#' Runoff data
#' 
#' Contains surface water runoff measurements in 5 minute intervals as well as 
#' sampleIDs at times when samples of the water were collected.
#' 
#' @format A data.frame with the following columns:
#' 
#'   \code{watershed}: the watershed where the flume was located
#'   \code{date_time}: date and time of data-point collection (YYYY-mm-dd hh:mm:ss)
#'   
#'   \code{level}: measured depth of water in flume (m)
#'   
#'   \code{flow}: rate of water flow based on measured level and dimensions of
#'   flume (gpm)
#'   
#'   \code{sampleID}: the unique water sample identifier
#'   
"runoff"
