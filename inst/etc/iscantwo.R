## iscantwo

library('qtl')
data(fake.f2)

fake.f2 <- calc.genoprob(fake.f2, step=5)
out <- scantwo(fake.f2, method="hk", verbose=FALSE)

iplotScantwo(out, fake.f2)
