### mvrVal.R: Functions for calculating validation statistics, such
### as MSEP, RMSEP and R2, for mvr objects.
###
### $Id$

MSEP <- function(object, ...) UseMethod("MSEP")
MSEP.mvr <- function(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
                     intercept = cumulative, se = FALSE, ...)
{
    ## Makes the code slightly simpler:
    cumulative <- missing(comps) || is.null(comps)

    ## Figure out which estimate(s) to calculate:
    allEstimates <- c("all", "train", "CV", "adjCV", "test")
    if (missing(estimate)) {
        ## Select the `best' available estimate
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
    if (any(estimate %in% c("CV", "adjCV"))) {
        ## Check that cross-validation is possible:
        if (!cumulative)
            stop("Cross-validation is not supported when `comps' is specified")
        if (is.null(object$validation))
            stop("`object' has no `validation' component")
    }

    ## The calculated estimates:
    z <- array(dim = c(length(estimate), dim(fitted(object))[2],
                       if(cumulative) 1 + length(ncomp) else 2),
               dimnames = list(estimate = estimate,
               response = dimnames(object$fitted)[[2]],
               model = if (cumulative) {
                   c("(Intercept)", paste(ncomp, "comps"))
               } else {
                   c("(Intercept)", paste("(Intercept), Comp",
                                          paste(comps, collapse = ", ")))
               }
               ))

    ## Calculate the estimates:
    for (i in seq(along = estimate)) {
        z[i,,] <-
            switch(estimate[i],
                   train = {
                       resp <- as.matrix(model.response(model.frame(object)))
                       n <- dim(resp)[1]
                       if (inherits(object$na.action, "exclude"))
                           resp <- napredict(object$na.action, resp)
                       res <- if (cumulative)
                           residuals(object, ...)[,,ncomp, drop=FALSE]
                       else
                           resp - predict(object, comps = comps, ...)
                       cbind(apply(resp, 2, var, na.rm = TRUE) * (n - 1) / n,
                             colMeans(res^2, na.rm = TRUE))
                   },
                   test = {
                       if (missing(newdata)) stop("Missing `newdata'.")
                       resp <-
                           as.matrix(model.response(model.frame(formula(object),
                                                                data=newdata)))
                       pred <- if (cumulative)
                           predict(object, ncomp = ncomp, newdata = newdata,...)
                       else
                           predict(object, comps = comps, newdata = newdata,...)
                       n <- nrow(newdata)
                       cbind(apply(resp, 2, var) * (n - 1) / n,
                             colMeans(sweep(pred, 1:2, resp)^2))
                   },
                   CV = {
                       cbind(object$validation$MSEP0,
                             object$validation$MSEP[,ncomp, drop=FALSE])
                   },
                   adjCV = {
                       MSEPtrain <-
                           colMeans(residuals(object,...)[,,ncomp,drop=FALSE]^2,
                                    na.rm = TRUE)
                       cbind(object$validation$MSEP0,
                             object$validation$MSEP[,ncomp, drop=FALSE] +
                             MSEPtrain -
                             object$validation$adj[,ncomp, drop=FALSE])
                   }
                   )
    }

    if (cumulative) comps <- ncomp
    ## Either remove the intercept MSEP or add a "zeroth" component:
    if (intercept)
        comps <- c(0, comps)
    else
        z <- z[,,-1, drop=FALSE]

    return(structure(list(val = z, type = "MSEP", comps = comps,
                          cumulative = cumulative, call = match.call()),
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
R2 <- function(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
               intercept = cumulative, se = FALSE, ...) {
    ## Makes the code slightly simpler:
    cumulative <- missing(comps) || is.null(comps)

    ## Figure out which estimate(s) to calculate:
    allEstimates <- c("all", "train", "CV", "test")
    if (missing(estimate)) {
        ## Select the `best' available estimate
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

    ## The calculated estimates:
    z <- array(0, dim = c(length(estimate), dim(fitted(object))[2],
                          if(cumulative) 1 + length(ncomp) else 2),
               dimnames = list(estimate = estimate,
               response = dimnames(object$fitted)[[2]],
               model = if (cumulative) {
                   c("(Intercept)", paste(ncomp, "comps"))
               } else {
                   c("(Intercept)", paste("(Intercept), Comp",
                                          paste(comps, collapse = ", ")))
               }
               ))

    ## Calculate the estimates:
    for (i in seq(along = estimate)) {
        switch(estimate[i],
               train = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   if (inherits(object$na.action, "exclude"))
                       resp <- napredict(object$na.action, resp)
                   pred <- if (cumulative)
                       fitted(object)[,,ncomp, drop=FALSE]
                   else
                       predict(object, comps = comps, ...)
                   for (j in 1:dim(resp)[2])
                       z[i,j,-1] <- cor(pred[,j,], resp[,j],
                                        use = "complete.obs")^2
               },
               test = {
                   if (missing(newdata)) stop("Missing `newdata'.")
                   resp <- as.matrix(model.response(model.frame(formula(object),
                                                                data=newdata)))
                   pred <- if (cumulative)
                       predict(object, ncomp = ncomp, newdata = newdata,...)
                   else
                       predict(object, comps = comps, newdata = newdata,...)
                   for (j in 1:dim(resp)[2])
                       z[i,j,-1] <- cor(pred[,j,], resp[,j])^2
               },
               CV = {
                   if (is.null(object$validation))
                       stop("`object' has no `validation' component.")
                   if (!cumulative)
                       stop("Cross-validation is not supported when `comps' is specified")
                   z[i,,-1] <- object$validation$R2[,ncomp, drop=FALSE]
               }
               )
    }

    if (cumulative) comps <- ncomp
    ## Either remove the intercept R2 or add a "zeroth" component:
    if (intercept)
        comps <- c(0, comps)
    else
        z <- z[,,-1, drop=FALSE]

    return(structure(list(val = z, type = "R2", comps = comps,
                          cumulative = cumulative, call = match.call()),
                     class = "mvrVal"))
}
