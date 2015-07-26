## itime

## this is (kinda) generalized and working but how to use the dot plots
## on the side if all points are plotted for every point along the lines

data(hyper)
hyper <- calc.genoprob(hyper, step=1)
out <- scanone(hyper)

# iplotScanone with no effects
iplotScanone(out, chr=c(1, 4, 6, 7, 15))


# iplotScanone with CIs
iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15))


# iplotScanone with raw phe x gen
iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15), pxgtype='raw')





iplotScanone <- function (scanoneOutput, cross, lodcolumn = 1, pheno.col = 1, 
          chr, pxgtype = c("ci", "raw"), fillgenoArgs = NULL, chartOpts = NULL) {
  if (!any(class(scanoneOutput) == "scanone")) 
    stop("\"scanoneOutput\" should have class \"scanone\".")
  if (!missing(chr) && !is.null(chr)) {
    scanoneOutput <- subset(scanoneOutput, chr = chr)
    if (!missing(cross) && !is.null(cross)) 
      cross <- subset(cross, chr = chr)
  }
  pxgtype <- match.arg(pxgtype)
  if (length(lodcolumn) > 1) {
    lodcolumn <- lodcolumn[1]
    warning("lodcolumn should have length 1; using first value")
  }
  if (lodcolumn < 1 || lodcolumn > ncol(scanoneOutput) - 2) 
    stop("lodcolumn must be between 1 and ", ncol(scanoneOutput) - 
           2)
  scanoneOutput <- scanoneOutput[, c(1, 2, lodcolumn + 2), 
                                 drop = FALSE]
  colnames(scanoneOutput)[3] <- "lod"
  scanone_list <- convert_scanone(scanoneOutput)
  if (missing(cross) || is.null(cross)) {
    pxgtype <- "none"
    pxg_list <- NULL
  } else {
    if (length(pheno.col) > 1) {
      pheno.col <- pheno.col[1]
      warning("pheno.col should have length 1; using first value")
    }
    if (class(cross)[2] != "cross") 
      stop("\"cross\" should have class \"cross\".")
    pxg_list <- convert_pxg(cross, pheno.col, fillgenoArgs = fillgenoArgs)
  }
  x <- list(scanone_data = scanone_list, 
            pxg_data = pxg_list, pxg_type = pxgtype, chartOpts = chartOpts)
  tmpx <<- x
  defaultAspect <- 2
  browsersize <- getPlotSize(defaultAspect)
  htmlwidgets::createWidget("iplotScanone", x, 
                            width = chartOpts$width, height = chartOpts$height, sizingPolicy = htmlwidgets::sizingPolicy(browser.defaultWidth = browsersize$width, 
                                                                                                                         browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000, 
                                                                                                                         knitr.defaultHeight = 1000/defaultAspect), package = "qtlcharts")
}

## plot continuous variable over time (cycle?) for each patient
set.seed(1)
n <- 10
ne <- sample(2:8, n, replace = TRUE)
id <- rep(seq.int(n), ne)
dd <- data.frame(id = id,
                 cycle = ave(id, id, FUN = seq_along),
                 # cycle = paste(id, ave(id, id, FUN = seq_along), sep = '.'),
                 resp = unlist(Map(runif, n = ne, min = 0, max = 1)) * 100)


f <- function(dat, id = 'id', time = 'cycle', values = 'resp') {
  dd <- setNames(dat[, c(id, time, values)], c('chr', 'pos', 'lod'))
  rownames(dd) <- with(dd, paste0(chr, pos))
  whd <- which(!duplicated(dd$chr))
  rownames(dd)[whd] <- paste0('ID: ', seq_along(whd))
  class(dd) <- c('scanone','data.frame')
  
  iplotScanone(dd, chr = 1:5)
}

f(dd)


itime <- function(scanoneOutput, cross, lodcolumn = 1, pheno.col = 1, chr,
            pxgtype = c("ci", "raw"), fillgenoArgs = NULL, chartOpts = NULL) {
  pxgtype <- match.arg(pxgtype) ## none, ci, raw
  # dat <- setNames(dat, c('chr','pos','lod'))
  scanone_list <- convert_scanone(scanoneOutput)
  if (missing(cross) || is.null(cross)) {
    pxgtype <- "none"
    pxg_list <- NULL
  } else {
    if (length(pheno.col) > 1) {
      pheno.col <- pheno.col[1]
      warning("pheno.col should have length 1; using first value")
    }
    if (class(cross)[2] != "cross") 
      stop("\"cross\" should have class \"cross\".")
    pxg_list <- convert_pxg(cross, pheno.col, fillgenoArgs = fillgenoArgs)
  }
  defaultAspect <- 2
  browsersize <- getPlotSize(defaultAspect)
  x <- list(scanone_data = scanone_list, pxg_data = pxg_list,
            pxg_type = pxgtype, chartOpts = chartOpts)
  xx <<- x
  htmlwidgets::createWidget(
    'iplotScanone', x, width = chartOpts$width, height = chartOpts$height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.defaultWidth = browsersize$width,
      browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000,
      knitr.defaultHeight = 1000 / defaultAspect), package = "qtlcharts")
}

convert_scanone <- function(output) {
  mnames <- rownames(output)
  pmarkers <- grep("^c.+\\.loc-*[0-9]+", mnames)
  mnames[pmarkers] <- ""
  chrnames <- as.character(unique(output[, 1]))
  lodnames <- names(output)[-(1:2)]
  if (length(lodnames) != length(unique(lodnames))) 
    warning("lod column names are not unique")
  c(list(chrnames = chrnames, lodnames = lodnames), as.list(output), 
    list(markernames = mnames))
}

convert_pxg <- function(cross, pheno.col = 1, fillgenoArgs = NULL) {
  geno_filled <- getImputedGenotypes(cross, fillgenoArgs = fillgenoArgs, 
                                     imputed_negative = TRUE)
  phe <- qtl::pull.pheno(cross, pheno.col)
  if (!is.numeric(phe)) 
    stop("phenotype ", pheno.col, " is not numeric: ", paste(head(phe), 
                                                             collapse = " "))
  markers <- qtl::markernames(cross)
  sexpgm <- qtl::getsex(cross)
  chrtype <- vapply(cross$geno, class, "")
  names(chrtype) <- qtl::chrnames(cross)
  uchrtype <- unique(chrtype)
  genonames <- vector("list", length(uchrtype))
  names(genonames) <- uchrtype
  for (i in uchrtype) genonames[[i]] <- qtl::getgenonames(class(cross)[1], 
                                                          i, "full", sexpgm, attributes(cross))
  id <- qtl::getid(cross)
  if (is.null(id)) 
    id <- 1:qtl::nind(cross)
  id <- as.character(id)
  dimnames(geno_filled) <- NULL
  chrByMarkers <- rep(qtl::chrnames(cross), qtl::nmar(cross))
  names(chrByMarkers) <- markers
  list(geno = t(geno_filled), pheno = phe, chrByMarkers = as.list(chrByMarkers), 
       indID = id, chrtype = as.list(chrtype), genonames = genonames)
}

getImputedGenotypes <- function(cross, fillgenoArgs = NULL, imputed_negative = TRUE) {
  method <- grabarg(fillgenoArgs, "method", "imp")
  error.prob <- grabarg(fillgenoArgs, "error.prob", 1e-04)
  map.function <- grabarg(fillgenoArgs, "map.function", "haldane")
  geno <- qtl::pull.geno(cross)
  cross_filled <- qtl::fill.geno(cross, method = method, error.prob = error.prob, 
                                 map.function = map.function)
  geno_imp <- qtl::pull.geno(cross_filled)
  chr <- qtl::chrnames(cross)
  chrtype <- vapply(cross$geno, class, "")
  sexpgm <- qtl::getsex(cross)
  if (any(chrtype == "X")) {
    for (i in chr[chrtype == "X"]) {
      geno_X <- qtl::reviseXdata(class(cross)[1], "full", 
                                 sexpgm, geno = qtl::pull.geno(cross, chr = i), 
                                 cross.attr = attributes(cross))
      geno[, colnames(geno_X)] <- geno_X
      geno_imp_X <- qtl::reviseXdata(class(cross)[1], "full", 
                                     sexpgm, geno = qtl::pull.geno(cross_filled, chr = i), 
                                     cross.attr = attributes(cross))
      geno_imp[, colnames(geno_imp_X)] <- geno_imp_X
    }
  }
  if (imputed_negative) {
    imputed <- is.na(geno) | (!is.na(geno_imp) & geno != 
                                geno_imp)
    geno_imp[imputed] <- -geno_imp[imputed]
  }
  geno_imp
}

data(hyper)
hyper <- calc.genoprob(hyper, step=1)
out <- scanone(hyper)

# iplotScanone with no effects
itime(out, chr=c(1, 4, 6, 7, 15),
      chartOpts = list(lod_ylab = 'y axis', lod_xlab = 'cycles'))
noeff <- xx

# iplotScanone with CIs
itime(out, hyper, chr=c(1, 4, 6, 7, 15))
cis <- xx

# iplotScanone with raw phe x gen
itime(out, hyper, chr=c(1, 4, 6, 7, 15), pxgtype='raw')
raws <<- xx

npt <- 10
ncycles <- sequence(npt) + 3
n <- sum(npt * ncycles)
npoints <- n
ncat <- 1:3
ncat <- ncat * rep(c(1,-1), each = length(ncat))
phenos <- c(runif(20), rep(NA, 20))


xxx <- list(
  scanone_data = list(
    ## panel labels
    chrnames = as.character(1:npt),
    ## ??
    lodnames = 'lod',
    ## ?? map to n observations per panel per pt -- must match chrnames?
    chr = factor(rep(as.character(1:npt), times = ncycles)),
    # chr = factor(rep(1:ncycles, each = n / ncycles)),
    ## position for each cycle for pt1, pt2, ...
    pos = sequence(ncycles),
    ## values for each pos per pt per cycle
    lod = unlist(lapply(1:npt, function(x)
      rawr::kinda_sort(runif(ncycles[x])))),
    ## must match chrnames?
    # markernames = c(rbind(letters[1:ncycles], matrix('', neach - 1, ncycles))),
    markernames = get_labels(list(ID = rep(1:npt, times = ncycles),
                                  cycle = sequence(ncycles)), sum(ncycles))
  ),
  pxg_data = list(
    ## set group and color of pheno (pts x npoints)
    geno = matrix(sample(ncat, n * sum(ncycles), replace = TRUE), sum(ncycles), n),
    ## values for dots
    # pheno = runif(n, 1, 2),
    pheno = sample(phenos, size = n, replace = TRUE),
    ## labels above side panels and cycle mapping to == number of total cycles
    chrByMarkers = setNames(as.list(as.character(rep(1:npt, times = ncycles))),
                            gsub('<br />', ', ',
                                 get_labels(list(Pt = rep(1:npt, ncycles),
                                                 Cyc = sequence(ncycles)), sum(ncycles)))),
    
    ## labels on each point of side panel
    indID = get_labels(list(ID = rep(seq.int(npt), times = npt * ncycles),
                            cycle = sequence(ncycles)), n),
    ## chart type per pt
    chrtype = setNames(as.list(sample(LETTERS[1:4], size = npt, replace = TRUE)),
                       seq.int(npt)),
    genonames = list(
      ## names for sets of dots in side panels
      ## letter maps to chart type for differnt labels
      A = letters[seq_along(unique(abs(ncat)))],
      B = LETTERS[seq_along(unique(abs(ncat)))],
      C = c(1:5)[seq_along(unique(abs(ncat)))],
      D = c('CR','PR','SD','PD')[seq_along(unique(abs(ncat)))]
    )
  ),
  # pxg_type = 'ci',
  # pxg_type = 'none',
  pxg_type = 'raw',
  # pxg_data = NULL,
  chartOpts = NULL
  )

chartOpts <- NULL
defaultAspect <- 2
browsersize <- getPlotSize(defaultAspect)
htmlwidgets::createWidget(
  'iplotScanone', xxx, width = chartOpts$width, height = chartOpts$height,
  sizingPolicy = htmlwidgets::sizingPolicy(
    browser.defaultWidth = browsersize$width,
    browser.defaultHeight = browsersize$height, knitr.defaultWidth = 1000,
    knitr.defaultHeight = 1000 / defaultAspect), package = "qtlcharts")
