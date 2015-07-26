## generate fc and pvals
## example from limma
library('limma')
set.seed(1)
n <- 500
sd <- 0.3 * sqrt(4 / rchisq(n / 2, df = 3))
y <- matrix(rnorm(n / 2 * 6, sd = sd), n / 2, 6)
rownames(y) <- paste("Gene", 1:(n / 2))
y[1:2, 4:6] <- y[1:2, 4:6] + 1.5

design <- cbind(Grp1 = 1, Grp2vs1 = c(0,0,0,1,1,1))
fit <- eBayes(lmFit(y, design))

proteins <- replicate(n, rawr::rgene())
ivolcano <- data.frame(logFC = c(fit$coefficients),
                       pval = c(fit$p.value),
                       row.names = make.unique(proteins))

with(ivolcano, plot(logFC, -log10(pval)))

with(ivolcano,
     iscatter(logFC, -log10(pval),
              # group = ((pval < 0.05) & (abs(logFC) > 1)) + L,
              col = c('blue','red')[((pval < 0.05) & (abs(logFC) > 1)) + 1L],
              labels = list(' ' = rownames(ivolcano),
                            'log2(Fold-change)' = round(logFC, 2),
                            ' ' = rawr::pvalr(pval, show.p = TRUE))))

## save(ivolcano, file = 'ivolcano.rda')
