### extract.R:  Extraction functions
### $Id$

## coef.mvr: Extract the base variable regression coefficients from
## an mvr object.
coef.mvr <- function(object, comps = object$ncomp, intercept = FALSE,
                       cumulative = TRUE, ...)
{
    if (cumulative) {
        ## Cumulative coefficients:
        B <- object$coefficients[,,comps, drop=FALSE]
        if (intercept == TRUE) {      # Intercept has only meaning for
                                      # cumulative coefficients
            dB <- dim(B)
            dB[1] <- dB[1] + 1
            dnB <- dimnames(B)
            dnB[[1]] <- c("(Intercept)", dnB[[1]]) 
            BInt <- array(dim = dB, dimnames = dnB)
            BInt[-1,,] <- B
            for (i in seq(along = comps))
                BInt[1,,i] <- object$Ymeans - object$Xmeans %*% B[,,i]
            B <- BInt
        }
    } else {
        ## Individual coefficients:
        B <- object$coefficients[,,comps, drop=FALSE]
        g1 <- which(comps > 1)
        ## Indiv. coef. must be calculated since object$coefficients is
        ## cumulative coefs.
        B[,,g1] <- B[,,g1, drop=FALSE] -
            object$coefficients[,,comps[g1] - 1, drop=FALSE]
        dimnames(B)[[3]] <- paste("Comp", comps)
    }
    return(B)
}

## fitted.default is in stats.

## loadings is in stats

## scores: Return the scores (also works for prcomp/princomp objects):
scores <- function(object, ...) UseMethod("scores")
scores.default <- function(object, ...) object$scores
scores.prcomp <- function(object, ...) object$x

## Yscores: Return the Yscores
Yscores <- function(object) object$Yscores

## loading.weights: Return the loading weights:
loading.weights <- function(object) object$loading.weights

## Yloadings: Return the Yloadings
Yloadings <- function(object) object$Yloadings

## model.matrix.mvr: Extract the model matrix from an `mvr' object.
## Modelled after model.matrix.lm.
model.matrix.mvr <- function(object, ...) 
{
    if (n_match <- match("x", names(object), 0)) 
        object[[n_match]]
    else {
        data <- model.frame(object, ...)
        NextMethod("model.matrix", data = data)
    }
}
