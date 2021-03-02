#' Water quality data
#'
#' Contains the analytical results from the Water Quality Research Lab at Iowa
#' State University, except those sampleIDs prefixed with SPL which were 
#' analyzed at the Lakeside Lab in Spirit Lake, IA.
#'
#' @format A data.frame with a number of columns
#'
#'   \code{sampleID}: a character vector of sample identifiers
#'   \code{Nitrate + nitrite (mg N/L)}: nitrogen concentration (detection limit
#'     is 0.003 mg N/L). 
#'   \code{Orthophosphate (mg P/L)}: otherophosphate concentration
#'   \code{TSS (mg/L)}: measured mass of suspended particles
#'   within the collected sample per unit of water  (mg/L)
#'
"water_quality"
