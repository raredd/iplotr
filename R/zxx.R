### utils needed from qtlcharts
# getPlotSize, getScreenSize, setScreenSize, group2numeric, add2chartOpts,
# convert_map, convert4iplotcorr, grabarg
###


getPlotSize <- function(aspectRatio) {
  ## qtlcharts:::getPlotSize
  screensize <- getScreenSize()
  if (screensize$height * aspectRatio <= screensize$width) 
    return(list(height = screensize$height, width = screensize$height * 
                  aspectRatio))
  list(height = screensize$width/aspectRatio, width = screensize$width)
}

getScreenSize <- function() {
  ## qtlcharts:::getScreenSize
  screensize <- getOption("qtlchartsScreenSize")
  if (is.null(screensize)) {
    setScreenSize()
    screensize <- getOption("qtlchartsScreenSize")
  }
  screensize
}

setScreenSize <- function(size = c("normal", "small", "large"), height, width) {
  ## qtlcharts:::setScreenSize
  if (!missing(height) && !is.null(height) && !is.na(height) && 
      height > 0 && !missing(width) && !is.null(width) && !is.na(width) && 
      width > 0) 
    screensize <- list(height = height, width = width)
  else {
    size <- match.arg(size)
    screensize <- switch(size, small = list(height = 600, 
                                            width = 900), normal = list(height = 700, width = 1000), 
                         large = list(height = 1200, width = 1600))
  }
  message("Set screen size to height=", screensize$height, 
          " x width=", screensize$width)
  options(qtlchartsScreenSize = screensize)
}

group2numeric <- function(group, preserveNA = FALSE) {
  ## qtlcharts:::group2numeric
  if (is.null(group)) 
    return(NULL)
  if (is.factor(group)) 
    group <- as.numeric(group)
  else if (!is.numeric(group)) 
    group <- match(group, sort(unique(group)))
  if (!preserveNA && any(is.na(group))) 
    group[is.na(group)] <- max(group, na.rm = TRUE) + 1L
  group
}

add2chartOpts <- function(chartOpts, ...) {
  ## qtlcharts:::add2chartOpts
  dots <- list(...)
  for (newarg in names(dots)) {
    if (!(newarg %in% names(chartOpts))) 
      chartOpts <- c(chartOpts, dots[newarg])
  }
  chartOpts
}

convert_map <- function(map) {
  ## qtlcharts:::convert_map
  chrnames <- names(map)
  map <- lapply(map, unclass)
  chr <- rep(names(map), vapply(map, length, 1))
  names(chr) <- NULL
  pos <- unlist(map)
  names(pos) <- NULL
  mnames <- unlist(lapply(map, names))
  names(mnames) <- NULL
  list(chr = chr, pos = pos, marker = mnames, chrname = chrnames)
}

convert4iplotcorr <- function(dat, group, rows, cols, reorder = FALSE, corr,
                              corr_was_presubset = FALSE, scatterplots = TRUE,
                              cFUN = NULL) {
  ## qtlcharts:::convert4iplotcorr with modifications for cFUN
  cFUN <- if (is.null(cFUN))
    function(x) stats::hclust(dist(x), method = 'average') else cFUN
  indID <- rownames(dat)
  if (is.null(indID)) 
    indID <- paste(1:nrow(dat))
  variables <- colnames(dat)
  if (is.null(variables)) 
    variable <- paste0("var", 1:ncol(dat))
  if (missing(group) || is.null(group)) 
    group <- rep(1, nrow(dat))
  if (nrow(dat) != length(group)) 
    stop("nrow(dat) != length(group)")
  if (!is.null(names(group)) && !all(names(group) == indID)) 
    stop("names(group) != rownames(dat)")
  if (!corr_was_presubset) {
    if (ncol(dat) != nrow(corr) || ncol(dat) != ncol(corr)) 
      stop("corr matrix should be ", ncol(dat), " x ", ncol(dat))
    if (reorder) {
      ord <- cFUN(corr)
      ord <- tryCatch(if (is.vector(ord, mode = 'integer')) ord else ord$order,
                  error = function(e) {
                    warning('Variables not reordered; see \'details\' ',
                            'section on the use of \'cluster\'', domain = NA)
                    seq.int(ncol(corr))
                  },
                  warning = function(w) {
                    warning('Variables not reordered; see \'details\' ',
                            'section on the use of \'cluster\'', domain = NA)
                    seq.int(ncol(corr))
                  })
      variables <- variables[ord]
      dat <- dat[, ord]
      reconstructColumnSelection <- function(ord, cols) {
        cols.logical <- rep(FALSE, length(ord))
        cols.logical[cols] <- TRUE
        which(cols.logical[ord])
      }
      rows <- reconstructColumnSelection(ord, rows)
      cols <- reconstructColumnSelection(ord, cols)
      corr <- corr[ord, ord]
    }
    corr <- corr[rows, cols]
  }
  dimnames(corr) <- dimnames(dat) <- names(group) <- NULL
  
  if (scatterplots)
    list(indID = indID, var = variables, corr = corr,
         rows = rows - 1, cols = cols - 1, dat = t(dat), group = group,
         scatterplots = scatterplots)
  else list(indID = indID, var = variables, corr = corr,
            rows = rows - 1, cols = cols - 1, scatterplots = scatterplots)
}

grabarg <- function(arguments, argname, default) {
  ## qtlcharts:::grabarg
  ifelse(argname %in% names(arguments), arguments[[argname]], default)
}
