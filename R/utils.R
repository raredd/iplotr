### utils
# numeric2col, rm_alpha, rm_alpha_plotOpts, recycle, get_labels
###


numeric2col <- function(x) {
  ## numeric2col(1:10)
  ## numeric2col(c('red','green','#00ff00'))
  paste0('#', apply(apply(col2rgb(x), 2, function(xx)
    format(as.hexmode(xx), width = 2)), 2, paste, collapse = ''))
}

rm_alpha <- function(x) {
  ## colors dont work with transparency so use this to remove from hex
  ## rm_alpha(c('#000000ff', 'red', '#nohexcol'))
  if (is.null(x)) NULL else gsub('(^#[A-Fa-f0-9]{6})[A-Fa-f0-9]{2}$', '\\1', x)
}

rm_alpha_plotOpts <- function(co) {
  # co <- list(pointcolor = NULL, rectcolor = '#f7f7f7', scatcolor = 'red',
  #            width = 700, height = 500)
  # rm_alpha_plotOpts(co)
  co[] <- lapply(seq_along(co), function(x)
    if (grepl('color', names(co[x]))) rm_alpha(co[[x]]) else co[[x]])
  co
}

recycle <- function(x, y) {
  ## for a vector y, repeats y to length of x
  ## recycle(1:5, 1:2)
  ## recycle(1:2, c('red','blue','green'))
  lx <- length(x)
  ly <- length(y)
  rep(y, ceiling(lx / ly))[seq_along(x)]
}

get_labels <- function(x, len) {
  ## create labels from a list of label info
  ## if any elements of the list is < length(x), items are recycled to len
  ## if x is not a list, returns null which will label points as seq_along(x)
  ## l <- with(mtcars, list(' ' = rownames(mtcars), mpg = mpg, hp = hp))
  ## get_labels(l, 32)
  if (!is.list(x))
    return(recycle(seq.int(len), x))
  lx <- seq_along(x)
  x <- lapply(x, function(x) recycle(seq.int(len), x))
  nx <- names(x)
  names(x) <- ifelse(nzchar(nx), nx, seq_along(nx))
  fmt <- paste0(rep('%s$:$ %s', length(lx)), collapse ='<br />')
  idx <- interleave(lx + length(lx), lx)
  out <- do.call('sprintf', c(list(fmt = fmt), c(x, names(x))[idx]))
  ## if white space is given as name, remove the "x: y" and just use "y"
  ## add an extra unique string to catch possible exceptions
  gsub('\\$:\\$', ':', gsub('\\s+\\$:\\$\\s{1}', '', out))
}
