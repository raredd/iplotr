## package things

if (getRversion() >= '2.15.1') {
  utils::globalVariables(c('setScreenSize'))
}


## DEPRECATED functions from qtlcharts

# convert_curves <- function(times, curvedata, group, indID) {
#   ## qtlcharts:::convert_curves
#   list(x = times, data = curvedata, group = group, indID = indID)
# }
# 
# convert_scat <- function(scatdata, group, indID) {
#   ## qtlcharts:::convert_scat
#   if (is.null(scatdata)) 
#     return(NULL)
#   list(data = scatdata, group = group, indID = indID)
# }
