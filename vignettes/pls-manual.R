## ----setup, echo=FALSE, results='hide'----------------------------------------
library(pls)
knitr::opts_chunk$set(fig.align = "center")
# Corresponds to pdf.options(pointsize=10) and options(digits = 4) in original
options(digits = 4)

## ----load-lib-----------------------------------------------------------------
library(pls)

## ----load-data----------------------------------------------------------------
data(yarn)
data(oliveoil)
data(gasoline)
data(mayonnaise)

## ----fig-NIR, echo=FALSE, fig.height=1.5, fig.cap="Gasoline NIR spectra"------
par(mar = c(2, 4, 0, 1) + 0.1)
matplot(t(gasoline$NIR), type = "l", lty = 1, ylab = "log(1/R)", xaxt = "n")
ind <- pretty(seq(from = 900, to = 1700, by = 2))
ind <- ind[ind >= 900 & ind <= 1700]
ind <- (ind - 898) / 2
axis(1, ind, colnames(gasoline$NIR)[ind])

## ----split-data---------------------------------------------------------------
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

## ----fit-gas1-----------------------------------------------------------------
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")

## ----summary-gas1-------------------------------------------------------------
summary(gas1)

## ----plot-RMSEP-code, eval=FALSE----------------------------------------------
# plot(RMSEP(gas1), legendpos = "topright")

## ----fig-RMSEP, echo=FALSE, fig.height=2.5, fig.cap="Cross-validated RMSEP curves for the gasoline data"----
par(mar = c(4, 4, 2.5, 1) + 0.1)
plot(RMSEP(gas1), legendpos = "topright")

## ----plot-pred-code, eval=FALSE-----------------------------------------------
# plot(gas1, ncomp = 2, asp = 1, line = TRUE)

## ----fig-cvpreds, echo=FALSE, fig.width=3.5, fig.height=3.7, fig.cap="Cross-validated predictions for the gasoline data"----
par(mar = c(4, 4, 2.5, 1) + 0.1)
plot(gas1, ncomp = 2, asp = 1, line = TRUE)

## ----plot-scores-code, eval=FALSE---------------------------------------------
# plot(gas1, plottype = "scores", comps = 1:3)

## ----fig-scores, echo=FALSE, fig.cap="Score plot for the gasoline data"-------
plot(gas1, plottype = "scores", comps = 1:3)

## ----explvar------------------------------------------------------------------
explvar(gas1)

## ----fig-loadings, fig.cap="Loading plot for the gasoline data", fig.height=2.5----
par(mar = c(4, 4, 0.3, 1) + 0.1)
plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")
abline(h = 0)

## ----fig-scores-cv, fig.cap="Calibrated and cross-validated score plots for the gasoline data", fig.height=2.5, fig.width=5----
par.old <- par(mfrow=c(1,2))
scoreplot(gas1)
scoreplot(gas1, estimate="CV")
par(par.old)

## ----predict------------------------------------------------------------------
predict(gas1, ncomp = 2, newdata = gasTest)

## ----rmsep-test---------------------------------------------------------------
RMSEP(gas1, newdata = gasTest)

## ----dens1--------------------------------------------------------------------
dens1 <- plsr(density ~ NIR, ncomp = 5, data = yarn)

## ----olive1-------------------------------------------------------------------
dim(oliveoil$sensory)
plsr(sensory ~ chemical, data = oliveoil)

## ----update-------------------------------------------------------------------
trainind <- which(yarn$train == TRUE)
dens2 <- update(dens1, subset = trainind)

## ----update2------------------------------------------------------------------
dens3 <- update(dens1, ncomp = 10)

## ----olive2-------------------------------------------------------------------
olive1 <- plsr(sensory ~ chemical, scale = TRUE, data = oliveoil)

## ----gas2---------------------------------------------------------------------
gas2 <- plsr(octane ~ msc(NIR), ncomp = 10, data = gasTrain)

## ----predict-gas2, eval=FALSE-------------------------------------------------
# predict(gas2, ncomp = 3, newdata = gasTest)

## ----selectNcomp-code, eval=FALSE---------------------------------------------
# ncomp.onesigma <- selectNcomp(gas2, method = "onesigma", plot = TRUE,
#                               ylim = c(.18, .6))
# ncomp.permut <- selectNcomp(gas2, method = "randomization", plot = TRUE,
#                             ylim = c(.18, .6))

## ----fig-NComp, echo=FALSE, fig.height=4.5, fig.width=10, fig.cap="The two strategies for suggesting optimal model dimensions: the left plot shows the one-sigma strategy, the right plot the permutation strategy."----
par(mfrow = c(1,2))
ncomp.onesigma <- selectNcomp(gas1, "onesigma", plot = TRUE,
                              ylim = c(.18, .6))
ncomp.permut <- selectNcomp(gas1, "randomization", plot = TRUE,
                            ylim = c(.18, .6))

## ----crossval-----------------------------------------------------------------
gas2.cv <- crossval(gas2, segments = 10)
plot(MSEP(gas2.cv), legendpos="topright")
summary(gas2.cv, what = "validation")

## ----plot-coef-code, eval=FALSE-----------------------------------------------
# plot(gas1, plottype = "coef", ncomp=1:3, legendpos = "bottomleft",
#      labels = "numbers", xlab = "nm")

## ----fig-gascoefs, echo=FALSE, fig.height=3, fig.cap="Regression coefficients for the gasoline data"----
par(mar = c(4, 4, 2.5, 1) + 0.1)
plot(gas1, plottype = "coef", ncomp=1:3, legendpos = "bottomleft",
     labels = "numbers", xlab = "nm")

## ----fig-corrplot, echo=FALSE, fig.width=3.5, fig.height=3.4, fig.cap="Correlation loadings plot for the gasoline data"----
par(mar = c(4, 4, 0, 1) + 0.1)
plot(gas1, plottype = "correlation")

## ----predict2-----------------------------------------------------------------
predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,])

## ----predict3-----------------------------------------------------------------
predict(gas1, comps = 2, newdata = gasTest[1:5,])

## ----predict4-----------------------------------------------------------------
drop(predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,]))

## ----predplot-code, eval=FALSE------------------------------------------------
# predplot(gas1, ncomp = 2, newdata = gasTest, asp = 1, line = TRUE)

## ----fig-testPreds, echo=FALSE, fig.width=3.5, fig.height=3.7, fig.cap="Test set predictions"----
par(mar = c(4, 4, 2.5, 1))
predplot(gas1, ncomp = 2, newdata = gasTest, asp = 1, line = TRUE)

## ----pls.options--------------------------------------------------------------
pls.options()

## ----pls.options-set----------------------------------------------------------
pls.options(plsralg = "oscorespls")

## ----manual-cv----------------------------------------------------------------
X <- gasTrain$NIR
Y <- gasTrain$octane
ncomp <- 5
cvPreds <- matrix(nrow = nrow(X), ncol = ncomp)
for (i in 1:nrow(X)) {
    fit <- simpls.fit(X[-i,], Y[-i], ncomp = ncomp, stripped = TRUE)
    cvPreds[i,] <- (X[i,] - fit$Xmeans) %*% drop(fit$coefficients) +
        fit$Ymeans
}

## ----manual-cv-rmsep----------------------------------------------------------
sqrt(colMeans((cvPreds - Y)^2))

