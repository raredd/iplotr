## iboxplot

n.ind <- 500
n.gene <- 10000
expr <- matrix(rnorm(n.ind * n.gene, (1:n.ind)/n.ind*3), ncol=n.gene)
dimnames(expr) <- list(paste0("ind", 1:n.ind),
                       paste0("gene", 1:n.gene))

iboxplot(expr, chartOpts=list(xlab="Mice", ylab="Gene expression"))
