## plots
# iscatter, idot, icorr, itree, icurve
##


#' iscatter
#' 
#' @description
#' Interactive scatter plot.
#' 
#' If \code{labels} is a vector, points are labeled as-is and recycled if
#' necessary. Optionally, \code{labels} can be a \emph{named} list where each
#' element of the list is a character vector (or one which can be coerced)
#' having the same length of \code{x}; see examples. If \code{x} and \code{y}
#' are named vectors, points will be labeled without needing to use
#' \code{labels}.
#' 
#' @param x,y vectors of x- and y-coordinates; if \code{y} is \code{NULL},
#' \code{x} will be plotted along the y-axis by index of \code{x}
#' @param group vector of group (\code{\link{factor}}-like) variables used for
#' additional aesthetics such as \code{col}
#' @param col a vector of colors for each unique \code{group}; note that colors
#' will be assigned to the sorted levels of \code{group}
#' @param cex point size in pixels
#' @param xlim,ylim x- and y-limits
#' @param xlab,ylab,main the x-, y-, and main labels
#' @param labels optional character vector or named list of character vectors
#' to label each point; if \code{NULL}, points will be labeled by index
#' @param plotOpts list of additional plot options; see
#' \code{\link{iscatterOpts}}
#' 
#' @seealso
#' \code{\link{icorr}}, \code{\link{idot}}, \code{\link{itree}},
#' \code{\link{icurve}}, \code{\link[qtlcharts]{iplot}}
#' 
#' @examples
#' ## basic usage
#' iscatter(1:5, col = 1:5, cex = 5)
#' 
#' ## using named vectors
#' x <- with(mtcars, setNames(mpg, mpg))
#' y <- with(mtcars, setNames(wt, wt))
#' iscatter(x, y)
#' 
#' ## using the labels parameter
#' with(mtcars,
#'   iscatter(wt, mpg, group = cyl, col = 1:3, cex = 5,
#'            main = 'Motor Trend car road tests',
#'            labels = list(model = rownames(mtcars), mpg = mpg, hp = hp)))
#' 
#' ## compare with base graphics
#' with(mtcars, plot(wt, mpg, col = factor(cyl), pch = 19))
#' 
#' ## labels parameter supports additional formatting with html tags
#' with(ivolcano,
#'      iscatter(logFC, -log10(pval), group = pval < 0.05 & abs(logFC) > 1,
#'               col = c('lightgrey','green'),
#'               labels = list(
#'                 ' ' = rownames(ivolcano),
#'                 'log2(FC)' = round(logFC, 2),
#'                 '<i>p</i>-value' = format.pval(ivolcano$pval, digits = 2,
#'                                                eps = .05))))
#' 
#' @export

iscatter <- function(x, y = NULL, group, col, cex = 3,
                     xlim = NULL, ylim = NULL,
                     xlab = NULL, ylab = NULL, main = NULL,
                     labels = NULL, plotOpts = NULL) {
  if (is.null(y)) {
    xl <- 'Index'
    yl <- deparse(substitute(x))
    ll <- names(x)
    y <- x
    x <- seq_along(y)
  } else {
    xl <- deparse(substitute(x))
    yl <- deparse(substitute(y))
    ll <- list(x = names(x), y = names(y))
    if (all(sapply(ll, is.null)))
      ll <- NULL
  }
  if (missing(group)) {
    if (missing(col)) {
      group <- recycle(x, 1)
      col <- NULL
    } else {
      group <- recycle(x, seq_along(col))
      col <- numeric2col(col)
    }
  } else {
    if (missing(col)) {
      col <- NULL
      group <- group2numeric(recycle(x, group))
    } else {
      group <- group2numeric(recycle(x, group))
      col <- numeric2col(col)
    }
  }
  group <- group2numeric(group)
  
  ## if x,y are named, use those names, otherwise use labels or index
  labels <- if (is.null(labels)) {
    if (is.null(ll)) seq_along(x) else get_labels(ll, length(x))
  } else get_labels(labels, length(x))
  names(x) <- names(y) <- NULL
  
  opts <- list(xlab = xlab %||% xl, ylab = ylab %||% yl, title = main,
               pointsize = cex, xlim = xlim, ylim = ylim, pointcolor = col)
  x <- list(data = data.frame(x = x, y = y, group = group, indID = labels),
            chartOpts = rm_alpha_plotOpts(c(opts, plotOpts)))
  defaultAspect <- 1.33
  browsersize <- getPlotSize(defaultAspect)
  
  htmlwidgets::createWidget(name = 'iplot', x = x,
    width = plotOpts$width, height = plotOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height), package = 'qtlcharts')
}

#' idot
#' 
#' @description
#' Interactive dot plot.
#' 
#' Any numeric data can be plotted on the y-axis but the x-axis should be
#' discrete groups. Points will be labeled with the vector names, the
#' \code{labels} parameter, or by index otherwise.
#' 
#' @param y numeric vector
#' @param group vector of grouping (\code{\link{factor}}-like) variables the
#' same length as \code{y}
#' @param cex point size in pixels
#' @param subgroup a logical vector the same length as \code{y}
#' @param xlim,ylim x- and y-limits
#' @param xlab,ylab,main the x-, y-, and main labels
#' @param labels optional character vector or named list of character vectors
#' to label each point; if \code{NULL}, points will be labeled by index
#' @param plotOpts list of additional plot options; see
#' \code{\link{idotOpts}}
#' 
#' @seealso
#' \code{\link{iscatter}}, \code{\link{icorr}}, \code{\link{itree}},
#' \code{\link{icurve}}, \code{\link[qtlcharts]{iplotPXG}}
#' 
#' @examples
#' ## basic usage with named vectors
#' idot(setNames(mtcars$mpg, rownames(mtcars)))
#' 
#' ## using group and subgroup (a vector of logicals to color points)
#' with(mtcars,
#'      idot(mpg, gear, subgroup = am == 1, cex = 5, ylim = c(0, 40),
#'           labels = rownames(mtcars)))
#' 
#' ## labels parameter supports additional formatting with html tags
#' with(ivolcano,
#'      idot(logFC, substr(rownames(ivolcano), 1, 1),
#'           subgroup = pval < 0.05, xlab = 'Treatment arm',
#'           labels = list(
#'             ' ' = rownames(ivolcano),
#'             'log2(FC)' = round(logFC, 2),
#'             '<i>p</i>-value' = format.pval(ivolcano$pval, digits = 2,
#'                                            eps = .05))))
#' 
#' @export

idot <- function(y, group, cex = 3, subgroup,
                 xlim = NULL, ylim = NULL,
                 xlab = NULL, ylab = NULL, main = NULL,
                 labels = NULL, plotOpts = NULL) {
  xl <- if (missing(group)) 'group' else deparse(substitute(group))
  yl <- deparse(substitute(y))
  group <- recycle(y, if (missing(group)) ' ' else group)
  group_levels <- sort(unique(group))
  group <- group2numeric(group)
  ## diff colors are made by sign in matrix - only two groups possible
  wh <- if (!missing(subgroup)) ifelse(subgroup, 1, -1) else recycle(y, 1)
  
  ## named vectors not supported in jsonlite
  if (!is.null(nn <- names(y)))
    names(y) <- NULL
  labels <- if (is.null(labels)) nn else get_labels(labels, length(y))
  
  opts <- list(xlab = xlab %||% xl, ylab = ylab %||% yl, title = main,
               pointsize = cex, xlim = xlim, ylim = ylim)
  x <- list(
    data = list(
      ## change color by sign
      geno = matrix(group, nrow = 1) * wh, pheno = y, indID = labels,
      chrByMarkers = list(group = "un"), chrtype = list(un = "A"),
      genonames = list(A = group_levels)),
    chartOpts = rm_alpha_plotOpts(c(opts, plotOpts)))
  defaultAspect <- 1
  browsersize <- getPlotSize(defaultAspect)
  
  htmlwidgets::createWidget(name = 'iplotPXG', x = x,
    width = plotOpts$width, height = plotOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000,
      knitr.defaultHeight = 1000 / defaultAspect), package = 'qtlcharts')
}

#' icorr
#' 
#' @description
#' Interactive correlation matrices (heat maps) with optional scatter plots.
#' 
#' Variables will be clustered (see details) and reordered by default. Cells
#' will be labeled with the column names of the input data, and if a scatter
#' plot is created, points will be labeled with the row names (this can be
#' over-ridden by using the \code{labels} parameter).
#' 
#' @details
#' If \code{col} is given with no \code{group} variable, the colors for each
#' observation will be recycled in order.
#' 
#' Default cluster method is \code{stats::hclust(dist(x), method = 'average')}
#' which will return a list containing a named vector, \code{"order"}, which is
#' used to reorder the variables.
#' 
#' In order to pass a custom clustering function to \code{cluster}, the
#' function must take a single input (a correlation matrix) and return either
#' a vector or a list with a named vector, \code{"order"}.
#' 
#' @param mat data matrix (observations x variables) of numeric values
#' @param group vector of grouping (\code{\link{factor}}-like) variables for
#' each observation
#' @param col a vector of colors for each unique \code{group} of points in
#' the scatter plots; note that colors will be assigned to the sorted levels
#' of \code{group}
#' @param labels optional character vector or named list of character vectors
#' to label each point; if \code{NULL}, points will be labeled by index
#' @param cluster logical or function; if \code{TRUE}, the variables will be
#' clustered and reordered; if \code{FALSE}, no reordering will be done;
#' otherwise, a custom clustering function may be given; see details
#' @param cor_method character string indicating which correlation coefficient
#' is to be computed; one of \code{'pearson'} (default), \code{'kendall'}, or
#' \code{'spearman'}: can be abbreviated; see \code{\link{cor}}
#' @param scatterplots logical; if \code{TRUE}, scatter plots of the linked
#' underlying data will be included
#' @param plotOpts list of additional plot options; see
#' \code{\link{icorrOpts}}
#' 
#' @seealso
#' \code{\link{iscatter}}, \code{\link{idot}}, \code{\link{itree}},
#' \code{\link{icurve}}, \code{\link[qtlcharts]{iplotCorr}}
#' 
#' @examples
#' ## heatmap only
#' icorr(mtcars, scatterplots = FALSE)
#' 
#' ## with scatter plots
#' icorr(mtcars, group = mtcars$cyl, col = c('blue','red','green'),
#'       plotOpts = list(corcolors = heat.colors(3)))
#' 
#' ## larger matrix example
#' set.seed(1)
#' dat <- replicate(50, mtcars[, sample(1:11, 1), drop = FALSE])
#' dat <- do.call('cbind', dat)
#' icorr(dat, cluster = TRUE, group = mtcars$cyl)
#' 
#' ## using a custom clustering function
#' icorr(dat, cluster = function(x) hclust(dist(x, method = 'maximum')))
#' icorr(dat, cluster = function(x) sample(seq.int(ncol(x))))
#' icorr(dat, cluster = function(x) list(order = order(rowMeans(x))))
#' 
#' @export

icorr <- function(mat, group, col, labels = NULL, cluster = TRUE,
                  cor_method = 'pearson', scatterplots = TRUE,
                  plotOpts = NULL) {
  
  mat <- as.matrix(mat)
  nr <- seq.int(nrow(mat))
  if (!is.null(labels))
    rownames(mat) <- get_labels(labels, nr)
  if (missing(group)) {
    if (missing(col)) {
      group <- recycle(nr, 1)
      col <- NULL
    } else {
      group <- recycle(nr, seq_along(col))
      col <- numeric2col(col)
    }
  } else {
    if (missing(col)) {
      col <- NULL
      group <- group2numeric(group)
    } else {
      group <- group2numeric(group)
      col <- numeric2col(recycle(seq.int(unique(group)), col))
    }
  }
  stopifnot(class(cluster) %in% c('logical','function'))
  if (is.function(cluster)) {
    cFUN <- cluster
    cluster <- TRUE
  } else cFUN <- NULL
  co <- plotOpts
  opts <- list(cortitle = co$cortitle %||% 'Correlation matrix',
               scattitle = co$scattitle %||% 'Scatter plot of values',
               scatcolors = col)
  group <- group2numeric(group)
  
  data_list <- convert4iplotcorr(
    mat, group, reorder = cluster, rows = 1:ncol(mat), cols = 1:ncol(mat),
    corr = stats::cor(mat, use = 'pairwise.complete.obs', method = cor_method),
    scatterplots = scatterplots, corr_was_presubset = FALSE, cFUN = cFUN)
  defaultAspect <- 2
  browsersize <- getPlotSize(defaultAspect)
  
  htmlwidgets::createWidget(name = 'iplotCorr',
    x = list(data = data_list,
             chartOpts = rm_alpha_plotOpts(c(opts, plotOpts))),
    width = plotOpts$width, height = plotOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000,
      knitr.defaultHeight = 1000 / defaultAspect), package = 'qtlcharts')
}

#' itree
#' 
#' @description
#' Interactive tree plot with search bar.
#' 
#' Any numeric data can be plotted on the y-axis but the x-axis should be
#' discrete groups. Points will be labeled with their \emph{unique} names
#' and y-value.
#' 
#' @param y a \emph{uniquely-named} numeric vector
#' @param group vector of grouping (\code{\link{factor}}-like) variables the
#' same length as \code{y}
#' @param ylim y-axis limits
#' @param xlab,ylab,main the x-, y-, and main labels
#' @param plotOpts list of additional plot options; see
#' \code{\link{itreeOpts}}
#' 
#' @seealso
#' \code{\link{iscatter}}, \code{\link{icorr}}, \code{\link{itree}},
#' \code{\link{icurve}}, \code{\link[qtlcharts]{iplotMap}}
#' 
#' @examples
#' ## basic usage with and without groups
#' itree(setNames(rnorm(10), letters[1:10]))
#' itree(setNames(rnorm(20), letters[1:20]), group = 1:4)
#' 
#' ## another example
#' set.seed(1)
#' n <- 100
#' ng <- 5
#' gr <- sort(sample(LETTERS[1:ng], n, replace = TRUE))
#' yv <- kinda_sort(runif(n, -1, 1), n = n / 2) * 100
#' itree(yv, gr, main = 'Subject response from baseline',
#'       ylim = c(-100,100), xlab = 'Treatment arm', ylab = '% change')
#' 
#' ## search is more useful with lots of data
#' set.seed(1)
#' p0 <- function(x) paste0(x, collapse = '')
#' n <- 500
#' gr <- replicate(n, (function()
#'   p0(sample(c('A','T','C','G'), 15, replace = TRUE)))())
#' y <- setNames(rpois(n, 40), gr)
#' 
#' ## hacky way to remove tick labels which start running together
#' gr_labels <- Map(function(x) p0(rep(' ', x)), sequence(n))
#' 
#' itree(y, unlist(gr_labels), ylab = 'Frequency', xlab = 'Sequence',
#'       plotOpts = list(tickwidth = 2, linewidth = 5, width = 2000))
#' 
#' @export

itree <- function(y, group, ylim = NULL, xlab = NULL, ylab = NULL,
                  main = NULL, plotOpts = NULL) {
  xl <- deparse(substitute(group))
  yl <- deparse(substitute(y))
  ## reverse ylim since default axis is reversed
  ylim <- rev(if (is.null(ylim)) range(y) else ylim)
  group <- recycle(y, if (missing(group)) 1 else group)
  
  ## make sure y names are unique--search is not useful without proper names
  ## doesnt seem to work with some characters or names with leading digits
  y <- if (!is.null(names(y))) {
    if (!length(y) == length(unique(names(y)))) {
      message('duplicate names found in \'y\' - creating unique names')
      setNames(y, make.unique(names(y)))
    } else y
  } else setNames(y, sprintf('%s-%s', group,
                             ave(seq_along(y), group, FUN = seq_along)))
  # names(y) <- make.names(names(y))
  sp <- split(y, group)
  lg <- vapply(sp, length, integer(1))
  map <- lapply(sp, function(x) {
    attr(x, 'class') <- 'A'
    x
  })
  class(map) <- 'map'
  map_list <- convert_map(map)
  
  opts <- list(xlab = xlab %||% xl, ylab = ylab %||% yl,
               title = main, ylim = ylim)
  x <- list(data = map_list,
            chartOpts = rm_alpha_plotOpts(c(opts, plotOpts)))
  defaultAspect <- 1.5
  browsersize <- getPlotSize(defaultAspect)
  
  htmlwidgets::createWidget(name = 'iplotMap', x = x,
    width = plotOpts$width, height = plotOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000,
      knitr.defaultHeight = 1000 / defaultAspect), package = 'qtlcharts')
}

#' icurve
#' 
#' @description
#' Interactive curves over time with optional interactive scatter plots.
#' 
#' Each timepoint is a column of \code{mat}, that is, times are treated as
#' discrete and assumed to be uniformly-spaced. Observations will be linked
#' across all plots (curves and optional scatter plots) by row index, so 
#' observations should be in the same order in all data provided.
#' 
#' @param mat a numeric matrix, \code{observations x timepoints}
#' @param labels optional character vector or named list of character vectors
#' to label each point; if \code{NULL}, points will be labeled by index
#' @param group vector of group (\code{\link{factor}}-like) variables
#' @param iscatter1 a numeric matrix or \code{NULL}, \code{nobs x 2}
#' @param iscatter2 see \code{iscatter1}
#' @param plotOpts list of additional plot options; see
#' \code{\link{icurveOpts}}
#' 
#' @seealso
#' \code{\link{icorr}}, \code{\link{idot}}, \code{\link{itree}},
#' \code{\link{icurve}}, \code{\link[qtlcharts]{iplotCurves}}
#' 
#' @examples
#' ## basic usage, no scatter plots
#' icurve(matrix(AirPassengers, ncol = 12, byrow = TRUE), labels = 1949:1960)
#' 
#' ## connecting observations over time with additional linked scatter plots
#' set.seed(1)
#' n <- 25
#' ## 25 observations and 5 timepoints
#' mm <- cbind(runif(n, 0, 10), runif(n, 10, 50), runif(n, 30, 60),
#'             runif(n, 40, 80), runif(n, 60, 100))
#' mm <- mm[order(mm[, 5]), ]
#' 
#' ## two sets of data (25 obs x 2 columns: id and numeric data)
#' x1 <- cbind(1:n, kinda_sort(rnorm(n), n / 2))
#' x2 <- cbind(1:n, kinda_sort(sample(1:100, n), n / 2))
#' 
#' icurve(mat = mm, group = 1:25 %% 5 == 0,
#'        iscatter1 = x1, iscatter2 = x2,
#'        labels = list(
#'          Patient = 1:25,
#'          Disease = sample(c('ALL','CLL'), 25, replace = TRUE)),
#'        plotOpts = list(
#'          curves_xlab = 'Response evaluation time point',
#'          curves_ylab = '% response',
#'          scat1_xlab = 'Patient', scat1_ylab = 'Lab var 1',
#'          scat2_xlab = 'Patient', scat2_ylab = 'Lab var 2'))
#' 
#' @export

icurve <- function(mat, labels, group, iscatter1 = NULL, iscatter2 = NULL,
                   plotOpts = NULL) {
  mat <- as.matrix(mat)
  nr <- nrow(mat)
  nc <- ncol(mat)
  if (!is.null(iscatter1))
    iscatter1 <- as.matrix(iscatter1)
  if (!is.null(iscatter2))
    iscatter2 <- as.matrix(iscatter2)
  if (!is.numeric(iscatter1 %||% numeric(1)) || 
      !is.numeric(iscatter2 %||% numeric(1)))
    stop('Scatter matrices should be numeric')
  if (!ident(nr, nrow(iscatter1) %||% nr, nrow(iscatter2) %||% nr))
    stop('All matrices should have same number of observations (rows)')
  if (!ident(ncol(iscatter1) %||% 2L, ncol(iscatter2) %||% 2L))
    stop('Scatter matrices should have two columns')
  if (is.null(iscatter1) && !is.null(iscatter2)) {
    scatter1 <- scatter2
    scatter2 <- NULL
  }
  times <- seq.int(nc)
  group <- recycle(seq.int(nr), if (missing(group)) 1 else group)
  group <- group2numeric(group)
  indID <- if (!missing(labels)) get_labels(labels, nr) else rownames(mat)
  dimnames(mat) <- dimnames(iscatter) <- dimnames(iscatter2) <-
    names(group) <- names(times) <- NULL
  data_list <- list(curve_data = convert_curves(times, mat, group, indID),
                    scatter1_data = convert_scat(iscatter1, group, indID),
                    scatter2_data = convert_scat(iscatter2, group, indID))
  x <- list(data = data_list, chartOpts = plotOpts)
  defaultAspect <- 1.25
  browsersize <- getPlotSize(defaultAspect)
  
  htmlwidgets::createWidget(name = 'iplotCurves', x = x,
    width = plotOpts$width, height = plotOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height,
      knitr.defaultWidth = 1000,
      knitr.defaultHeight = 1000 / defaultAspect),
    package = 'qtlcharts')
}
