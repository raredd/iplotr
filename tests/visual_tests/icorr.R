## visual tests
library('iplotr')
data(geneExpr)

## alpha trans colors not working in iplotCorr
iplotCorr(geneExpr$expr, geneExpr$genotype, reorder = TRUE,
          chartOpts=list(cortitle = "Correlation matrix",
                         scattitle = "Scatterplot", height = 400, width = 800,
                         corcolors = heat.colors(3),
                         scatcolors = c('#0000ff','#00ff00','#ff0000')))

icorr(geneExpr$expr, geneExpr$genotype, cluster = TRUE, col = 1:3,
      labels = list(' ' = paste0('Subject ', sample(491)),
                    Disease = sample(LETTERS[1:4], 491, replace = TRUE)),
          chartOpts=list(cortitle="Correlation matrix",
                         scattitle="Scatterplot", height = 400, width = 800,
                         corcolors = heat.colors(3)))

## normal usage
icorr(mtcars, group = mtcars$cyl, col = c('red', 'blue','green'))

## both missing
icorr(mtcars, chartOpts = list(corcolors = heat.colors(3)))

## one missing
icorr(mtcars, group = mtcars$cyl)
icorr(mtcars, col = c('red', 'blue','green')) ## col recycled without group

icorr(mtcars, group = mtcars$cyl[-1]) ## should throw error

## if ng != nc
icorr(mtcars, group = mtcars$cyl, col = c('red', 'blue'))
icorr(mtcars, group = mtcars$cyl, col = c('red', 'blue','green','pink'))

## other options
icorr(mtcars, cluster = FALSE,
      labels = with(mtcars, list(' ' = rownames(mtcars), mpg = mpg,
                  '<i>p</i>-value' = rawr::pvalr(1 / exp(c(1:10,1:10,1:2))))))


## cluster functions
## expect identical
icorr(mtcars, cluster = function(x) x) ## with warning
icorr(mtcars, cluster = FALSE)

icorr(mtcars, cluster = 1:11) ## error
icorr(mtcars, cluster = c(TRUE, FALSE))  ## warning
