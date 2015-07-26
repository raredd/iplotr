## iplotRF

library('qtl')
data(fake.f2)

fake.f2 <- est.rf(fake.f2)

iplotRF(fake.f2)
