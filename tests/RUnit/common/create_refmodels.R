## Creates reference models to compare later implementations with

## The models were created with R 3.3.2 an pls 2.5-0

require(pls)
sessionInfo()

## Single response model:
rm(list = ls(all = TRUE))
mydata <- gasoline
form <- octane ~ NIR
nc <- 27                            # Max on MSWin: 36, Linux/64: 27
refmod <-  mvr(form, nc = nc, data = mydata, method = "oscorespls")
refmod$method <- refmod$call <- refmod$fit.time <- NULL
save.image(file = "ref_singresp.RData")

## Multi-response model:
rm(list = ls(all = TRUE))
mydata <- oliveoil
form <- sensory ~ chemical
nc <- 4                             # 5 == max
## Note: the algs give sign differences, in general, for scores
## and loadings
refmod <- mvr(form, nc = nc, data = mydata, method = "oscorespls")
refmod$method <- refmod$call <- refmod$fit.time <-NULL
for (i in 2:7) refmod[[i]] <- abs(refmod[[i]])
save.image(file = "ref_multiresp.RData")
