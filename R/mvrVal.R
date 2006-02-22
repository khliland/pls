### mvrVal.R: Functions for calculating validation statistics, such
### as MSEP, RMSEP and R2, for mvr objects.
###
### $Id$

MSEP <- function(object, ...) UseMethod("MSEP")
MSEP.mvr <- function(object, estimate, newdata, comps = 1:object$ncomp,
                     cumulative = TRUE, intercept = cumulative, se = FALSE, ...)
{
    allEstimates <- c("all", "train", "CV", "adjCV", "test")
    if (missing(estimate)) {
        ## Decide what type of estimate to calculate
        if (!missing(newdata)) {
            estimate = "test"
        } else {
            if (!is.null(object$validation)) {
                estimate = c("CV", "adjCV")
            } else {
                estimate = "train"
            }
        }
    } else {
        estimate <- allEstimates[pmatch(estimate, allEstimates)]
        if (any(is.na(estimate))) stop("`estimate' should be a subset of ",
                                       paste(allEstimates, collapse = ", "))
        if (any(estimate == "all")) {
            estimate <- allEstimates[-1] # Try all estimates (except "all")
            if(missing(newdata)) estimate <- setdiff(estimate, "test")
            if(is.null(object$validation) || !cumulative)
                estimate <- setdiff(estimate, c("CV", "adjCV"))
        }
    }
    z <- array(dim = c(length(estimate), dim(fitted(object))[2],
               if(cumulative) 1+length(comps) else 2),
               dimnames = list(estimate = estimate,
               response = dimnames(object$fitted)[[2]],
               model = if (cumulative) c("(Intercept)", paste(comps, "comps")) else c("(Intercept)", paste("(Intercept), Comp", paste(comps, collapse = ", ")))))

    for (i in seq(along = estimate)) {
        z[i,,] <-
            switch(estimate[i],
                   train = {
                       resp <- as.matrix(model.response(model.frame(object)))
                       res <- if (cumulative)
                           residuals(object, ...)[,,comps, drop = FALSE]
                       else
                           resp - predict(object, comps = comps,
                                          cumulative = FALSE, ...)
                       n <- dim(res)[1]
                       cbind(apply(resp, 2, var) * (n - 1) / n,
                             colMeans(res^2))
                   },
                   test = {
                       if (missing(newdata)) stop("Missing `newdata'.")
                       resp <- as.matrix(model.response(model.frame(formula(object), data = newdata)))
                       pred <- predict(object, comps = comps,
                                       newdata = newdata,
                                       cumulative = cumulative, ...)
                       n <- nrow(newdata)
                       cbind(apply(resp, 2, var) * (n - 1) / n,
                             colMeans(sweep(pred, 1:2, resp)^2))
                   },
                   CV = {
                       if (is.null(object$validation))
                           stop("`object' has no `validation' component.")
                       if (!cumulative) stop("`cumulative = FALSE' not supported with cross-validation")
                       cbind(object$validation$MSEP0,
                             object$validation$MSEP[,comps, drop = FALSE])
                   },
                   adjCV = {
                       if (is.null(object$validation))
                           stop("`object' has no `validation' component.")
                       if (!cumulative) stop("`cumulative = FALSE' not supported with cross-validation")
                       MSEPtrain <-
                           colMeans(residuals(object, ...)[,,comps, drop = FALSE]^2)
                       cbind(object$validation$MSEP0,
                             object$validation$MSEP[,comps, drop = FALSE] + MSEPtrain -
                             object$validation$adj[,comps, drop = FALSE])
                   }
                   )
    }

    ## Either remove the intercept MSEP or add a "zeroth" component:
    if (intercept)
        comps <- c(0, comps)
    else
        z <- z[,,-1, drop = FALSE]

    return(structure(list(val = z, type = "MSEP", comps = comps,
                          call = match.call()),
                     class = "mvrVal"))
}


# RMSEP: A wrapper around MSEP to calculate RMSEPs
RMSEP <- function(object, ...) UseMethod("RMSEP")
RMSEP.mvr <- function(object, ...) {
    cl <- match.call()
    cl[[1]] <- as.name("MSEP")
    z <- eval(cl, parent.frame())
    z$val <- sqrt(z$val)
    z$type <- "RMSEP"
    z$call[[1]] <- as.name("RMSEP")
    z
}


## R2:  Return R2
R2 <- function(object, estimate, newdata, comps = 1:object$ncomp,
               cumulative = TRUE, intercept = cumulative, se = FALSE, ...) {
    allEstimates <- c("all", "train", "CV", "test")
    if (missing(estimate)) {
        ## Decide what type of estimate to calculate
        if (!missing(newdata)) {
            estimate = "test"
        } else {
            if (!is.null(object$validation)) {
                estimate = "CV"
            } else {
                estimate = "train"
            }
        }
    } else {
        estimate <- allEstimates[pmatch(estimate, allEstimates)]
        if (any(is.na(estimate))) stop("`estimate' should be a subset of ",
                                       paste(allEstimates, collapse = ", "))
        if (any(estimate == "all")) {
            estimate <- allEstimates[-1] # Try all estimates (except "all")
            if(missing(newdata)) estimate <- setdiff(estimate, "test")
            if(is.null(object$validation) || !cumulative)
                estimate <- setdiff(estimate, "CV")
        }
    }
    z <- array(0, dim = c(length(estimate), dim(fitted(object))[2],
               if(cumulative) 1+length(comps) else 2),
               dimnames = list(estimate = estimate,
               response = dimnames(object$fitted)[[2]],
               model = if (cumulative) c("(Intercept)", paste(comps, "comps")) else c("(Intercept)", paste("(Intercept), Comp", paste(comps, collapse = ", ")))))
    for (i in seq(along = estimate)) {
        switch(estimate[i],
               train = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   pred <- if (cumulative)
                       fitted(object)[,,comps, drop = FALSE]
                   else
                       predict(object, comps = comps,
                               cumulative = FALSE, ...)
                   for (j in 1:dim(resp)[2])
                       z[i,j,-1] <- cor(pred[,j,], resp[,j])^2
               },
               test = {
                   if (missing(newdata)) stop("Missing `newdata'.")
                   resp <- as.matrix(model.response(model.frame(formula(object), data = newdata)))
                   pred <- predict(object, comps = comps,
                                   newdata = newdata,
                                   cumulative = cumulative, ...)
                   for (j in 1:dim(resp)[2])
                       z[i,j,-1] <- cor(pred[,j,], resp[,j])^2
               },
               CV = {
                   if (is.null(object$validation))
                       stop("`object' has no `validation' component.")
                   if (!cumulative) stop("`cumulative = FALSE' not supported with cross-validation")
                   z[i,,-1] <- object$validation$R2[,comps, drop = FALSE]
               }
               )
    }

    ## Either remove the intercept R2 or add a "zeroth" component:
    if (intercept)
        comps <- c(0, comps)
    else
        z <- z[,,-1, drop = FALSE]

    return(structure(list(val = z, type = "R2", comps = comps,
                          call = match.call()),
                     class = "mvrVal"))
}
