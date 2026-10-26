## ----setup, include=FALSE-----------------------------------------------------
library(pls)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width=5)

## ----fit-model-gg-------------------------------------------------------------
data(yarn)
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")

## ----ggplot-score-loadings, eval=TRUE-----------------------------------------
gg_scoreplot(mod)
gg_loadingplot(mod, scatter = TRUE)
gg_loadingplot(mod)

## ----optional-interactive, eval=FALSE-----------------------------------------
# ids <- gg_scoreplot(mod, select = TRUE)
# mod_drop <- refit(mod, selection = ids, exclude = TRUE)
# mod_keep <- refit(mod, selection = ids, exclude = FALSE)

## ----ggplot-bi-corr, eval=TRUE------------------------------------------------
gg_biplot(mod)
gg_corrplot(mod)

## ----ggplot-pred, eval=TRUE---------------------------------------------------
gg_predplot(mod, ncomp = 1:2)

## ----ggplot-coef, eval=TRUE---------------------------------------------------
gg_coefplot(mod, ncomp = 1:2)

## ----ggplot-val, eval=TRUE----------------------------------------------------
gg_valplot(mod, val.type = "R2", estimate = c("train", "CV"))

## ----ggplot-raw, eval=TRUE----------------------------------------------------
gg_rawdataplot(mod, plottype = "line")

## ----optional-autoplot, eval=FALSE--------------------------------------------
# autoplot(mod, type = "scores")
# autoplot(mod, type = "loadings")
# autoplot(mod, type = "biplot")
# autoplot(mod, type = "correlation")
# autoplot(mod, type = "prediction")
# autoplot(mod, type = "coefficients")
# autoplot(mod, type = "validation", val.type = "R2", estimate = c("train", "CV"))
# autoplot(mod, type = "rawdata", plottype = "line")

## ----ggplot-dashboard, eval=TRUE----------------------------------------------
# Static dashboard (draws in interactive sessions, returns a gtable invisibly)
gg_dashboard(
  mod,
  panel_args = list(prediction = list(ncomp = 2))
)

# HTML dashboard with Plotly panels
# Object selection is turned on by default.
gg_dashboard(
  mod,
  plotly = TRUE,
  panel_args = list(prediction = list(ncomp = 2))
)

## ----tidy-components----------------------------------------------------------
comp_tbl <- tidy(mod, what = c("weights", "loadings", "coefficients"))
head(comp_tbl)

## ----tidy-validation----------------------------------------------------------
rmsep_tbl <- tidy(RMSEP(mod, estimate = c("train", "CV")))
head(rmsep_tbl)

## ----glance-summary-----------------------------------------------------------
glance(mod)

## ----augment-train------------------------------------------------------------
# Add fitted values and residuals for the training data.
aug_train <- augment(mod)
aug_train[1:5, -2]

## ----augment-newdata----------------------------------------------------------
# Add fitted values for new data.
aug_new <- augment(mod, data = d[1:5, , drop = FALSE])
aug_new[, -1]

