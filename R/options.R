### plotting options
# iplotOpts, iscatterOpts, icorrOpts, idotOpts, itreeOpts, icurveOpts
###


#' Plot options
#' 
#' In additional to the arguments passed directly to the individual plotting
#' functions, certain aesthetics of each can be changed by using the
#' \code{plotOpts} argument. This should be a \emph{list} of options such as
#' \code{height} or \code{width} setting the plot size in pixels;
#' \code{rectcolor} for the background color of the plot; \code{margin} to
#' specify the left, top, right, bottom, and inner margins; or \code{axispos}
#' and \code{titlepos} to specify the label positions in pixels.
#' 
#' Each type of plot has its own additional aesthetic options:
#' 
#' \tabular{rll}{
#' \tab \code{\link{iscatter}} \tab scatter plot (\code{\link{iscatterOpts}}) \cr
#' \tab \code{\link{icorr}} \tab correlation matrix and optional
#' scatterplots (\code{\link{icorrOpts}}) \cr
#' \tab \code{\link{idot}} \tab dot plots (\code{\link{idotOpts}}) \cr
#' \tab \code{\link{itree}} \tab tree plots (\code{\link{itreeOpts}}) \cr
#' \tab \code{\link{icurve}} \tab curves with optional scatterplots
#' (\code{\link{icurveOpts}}) \cr
#' }
#' 
#' See \url{http://kbroman.org/qtlcharts/assets/vignettes/chartOpts.html} or
#' the help pages for more plotting options.
#' 
#' @name plotOpts
#' @aliases iplotOpts
NULL

#' iscatter options
#' 
#' Below is a list of options for scatter plots (\code{\link{iscatter}}).
#' 
#' \tabular{rlll}{
#' \tab \strong{option} \tab \strong{description} \tab \strong{default} \cr
#' \tab \code{height} \tab height of plot in pixels \tab \code{500} \cr
#' \tab \code{width} \tab width of plot in pixels \tab \code{800} \cr
#' \tab \code{title} \tab title for plot \tab \code{''} \cr
#' \tab \code{margin} \tab margins in pixels (left, top, right, bottom, inner)
#' \tab \code{list(left=60, top=40, right=40, bottom=40, inner=5)} \cr
#' \tab \code{xlab} \tab x-axis label \tab \code{'X'} \cr
#' \tab \code{ylab} \tab y-axis label \tab \code{'Y'} \cr
#' \tab \code{axispos} \tab position of axis labels in pixels (xtitle, ytitle,
#' xlabel, ylabel) \tab \code{list(xtitle=25, ytitle=30, xlabel=5, ylabel=5)} \cr
#' \tab \code{titlepos} \tab position of plot title in pixels \tab \code{20} \cr
#' \tab \code{xlim} \tab x-axis limits \tab \code{NULL} \cr
#' \tab \code{xticks} \tab vector of tick positions on x-axis \tab
#' \code{NULL} \cr
#' \tab \code{nxticks} \tab number ticks on x-axis \tab \code{5} \cr
#' \tab \code{ylim} \tab y-axis limits \tab \code{NULL} \cr
#' \tab \code{yticks} \tab vector of tick positions on y-axis \tab
#' \code{NULL} \cr
#' \tab \code{nyticks} \tab number ticks on y-axis \tab \code{5} \cr
#' \tab \code{rectcolor} \tab color of background rectangle \tab
#' \code{'#E6E6E6'} \cr
#' \tab \code{pointcolor} \tab colors for points \tab \code{NULL} \cr
#' \tab \code{pointsize}\tab size of points in pixels \tab \code{3} \cr
#' \tab \code{pointstroke} \tab color of outer circle for points \tab
#' \code{'black'} \cr
#' \tab \code{rotate_ylab} \tab whether to rotate the y-axis label \tab
#' \code{NULL} \cr
#' \tab \code{xNA} \tab treatment of missing values (handle=T/F, force=T/F,
#' width, gap) \tab \code{list(handle=true, force=false, width=15, gap=10)} \cr
#' \tab \code{yNA} \tab treatment of missing values (handle=T/F, force=T/F,
#' width, gap) \tab \code{list(handle=true, force=false, width=15, gap=10)} \cr
#' }
#' @name iscatterOpts
NULL

#' icorr options
#' 
#' Below is a list of options for correlation matrices with optional
#' linked scatter plots (\code{\link{icorr}}).
#' 
#' \tabular{rlll}{
#' \tab \strong{option} \tab \strong{description} \tab \strong{default} \cr
#' \tab \code{height} \tab height of each panel in pixels  \tab \code{560} \cr
#' \tab \code{width} \tab total width of panels \tab \code{1050} \cr
#' \tab \code{margin} \tab margins in pixels (left, top, right, bottom, inner)
#' \tab \code{list(left=70, top=40, right=5, bottom=70, inner=5)} \cr
#' \tab \code{corcolors} \tab heat map colors (same length as \code{zlim}) \tab
#' \code{c('darkslateblue', 'white', 'crimson')} \cr
#' \tab \code{zlim} \tab z-axis limits \tab \code{c(-1, 0, 1)}\cr
#' \tab \code{rectcolor} \tab color of background rectangle \tab
#' \code{'#E6E6E6'} \cr
#' \tab \code{cortitle} \tab title for heat map panel \tab \code{''} \cr
#' \tab \code{scattitle} \tab title for scatterplot panel \tab \code{''} \cr
#' \tab \code{scatcolors} \tab vector of point colors for scatterplot \tab 
#' \code{NULL} \cr
#' }
#' @name icorrOpts
NULL

#' idot options
#' 
#' Below is a list of options for dot plots (\code{\link{idot}}).
#' 
#' \tabular{rlll}{
#' \tab \strong{option} \tab \strong{description} \tab \strong{default} \cr
#' \tab \code{height} \tab height of plot in pixels \tab \code{500} \cr
#' \tab \code{width} \tab width of plot in pixels \tab \code{800} \cr
#' \tab \code{title} \tab title for plot \tab \code{''} \cr
#' \tab \code{margin} \tab margins in pixels (left, top, right, bottom,
#' inner) \tab \code{list(left=60, top=40, right=40, bottom=40, inner=5)} \cr
#' \tab \code{xlab} \tab x-axis label \tab \code{'X'} \cr
#' \tab \code{ylab} \tab y-axis label \tab \code{'Y'} \cr
#' \tab \code{axispos} \tab position of axis labels in pixels (xtitle, ytitle,
#' xlabel, ylabel) \tab \code{list(xtitle=25, ytitle=30, xlabel=5, ylabel=5)} \cr
#' \tab \code{titlepos} \tab position of plot title in pixels \tab
#' \code{20} \cr
#' \tab \code{xjitter} \tab amount of horizontal jittering in pixels
#' \tab \code{NULL} \cr
#' \tab \code{ylim} \tab y-axis limits \tab \code{NULL} \cr
#' \tab \code{yticks} \tab vector of tick positions on y-axis \tab
#' \code{NULL} \cr
#' \tab \code{nyticks} \tab number ticks on y-axis \tab \code{5} \cr
#' \tab \code{rectcolor} \tab color of background rectangle \tab
#' \code{'#E6E6E6'} \cr
#' \tab \code{pointcolor} \tab colors for points \tab \code{'slateblue'} \cr
#' \tab \code{pointsize}\tab size of points in pixels \tab \code{3} \cr
#' \tab \code{pointstroke} \tab color of outer circle for points \tab
#' \code{'black'} \cr
#' \tab \code{yNA} \tab treatment of missing values (handle=T/F, force=T/F,
#' width, gap) \tab \code{list(handle=true, force=false, width=15, gap=10)} \cr
#' }
#' 
#' @name idotOpts
NULL

#' itree options
#' 
#' Below is a list of options for tree plots (\code{\link{itree}}).
#' 
#' \tabular{rlll}{
#' \tab \strong{option} \tab \strong{description} \tab \strong{default} \cr
#' \tab \code{width} \tab width of plot in pixels \tab \code{1000} \cr
#' \tab \code{height} \tab height of plot in pixels \tab \code{600} \cr
#' \tab \code{margin} \tab margins in pixels (left, top, right, bottom,
#' inner) \tab \code{list(left=60, top=40, right=100, bottom=40, inner=10)} \cr
#' \tab \code{axispos} \tab position of axis labels in pixels (xtitle, ytitle,
#' xlabel, ylabel) \tab \code{list(xtitle=25, ytitle=30, xlabel=5, ylabel=5)} \cr
#' \tab \code{titlepos} \tab position of plot title in pixels \tab \code{20} \cr
#' \tab \code{ylim} \tab y-axis limits \tab \code{NULL} \cr
#' \tab \code{nyticks} \tab no. ticks on y-axis \tab \code{5} \cr
#' \tab \code{yticks} \tab vector of tick positions on y-axis \tab
#' \code{NULL} \cr
#' \tab \code{tickwidth} \tab width of tick marks at markers, in pixels
#' \tab \code{10} \cr
#' \tab \code{rectcolor} \tab color of background rectangle \tab
#' \code{'#E6E6E6'} \cr
#' \tab \code{linecolor} \tab color of lines \tab \code{'slateblue'} \cr
#' \tab \code{linecolorhilit} \tab color of lines when highlighted \tab
#' \code{'Orchid'} \cr
#' \tab \code{linewidth} \tab width of lines \tab \code{3} \cr
#' \tab \code{title} \tab title for plot \tab \code{''} \cr
#' \tab \code{xlab} \tab x-axis label \tab \code{''} \cr
#' \tab \code{ylab} \tab y-axis label \tab \code{''} \cr
#' }
#' 
#' @name itreeOpts
NULL

#' icurve options
#' 
#' Below is a list of options for tree plots (\code{\link{icurve}}).
#' 
#' \tabular{rlll}{
#' \tab \strong{option} \tab \strong{description} \tab \strong{default} \cr
#' 
#' 
#' \tab \code{width} \tab width of plot in pixels \tab \code{1000} \cr
#' \tab \code{height} \tab height of plot in pixels \tab \code{600} \cr
#' \tab \code{margin} \tab margins in pixels (left, top, right, bottom,
#' inner) \tab \code{list(left=60, top=40, right=100, bottom=40, inner=10)} \cr
#' \tab \code{axispos} \tab position of axis labels in pixels (xtitle, ytitle,
#' xlabel, ylabel) \tab \code{list(xtitle=25, ytitle=30, xlabel=5, ylabel=5)} \cr
#' \tab \code{titlepos} \tab position of plot title in pixels \tab \code{20} \cr
#' \tab \code{ylim} \tab y-axis limits \tab \code{NULL} \cr
#' \tab \code{nyticks} \tab no. ticks on y-axis \tab \code{5} \cr
#' \tab \code{yticks} \tab vector of tick positions on y-axis \tab
#' \code{NULL} \cr
#' \tab \code{tickwidth} \tab width of tick marks at markers, in pixels
#' \tab \code{10} \cr
#' \tab \code{rectcolor} \tab color of background rectangle \tab
#' \code{'#E6E6E6'} \cr
#' \tab \code{linecolor} \tab color of lines \tab \code{'slateblue'} \cr
#' \tab \code{linecolorhilit} \tab color of lines when highlighted \tab
#' \code{'Orchid'} \cr
#' \tab \code{linewidth} \tab width of lines \tab \code{3} \cr
#' \tab \code{title} \tab title for plot \tab \code{''} \cr
#' \tab \code{xlab} \tab x-axis label \tab \code{''} \cr
#' \tab \code{ylab} \tab y-axis label \tab \code{''} \cr
#' }
#' 
#' @name icurveOpts
NULL
