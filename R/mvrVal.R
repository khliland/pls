### mvrVal.R: Functions for calculating validation statistics, such
### as MSEP, RMSEP and R2, for mvr objects.
###
### $Id$

## Calculate the validation statistics needed for (R)MSEP and R^2.
## Note that it accepts any values for `estimate', but only calculates
## statistics for "train", "test" and "CV".
mvrValstats <- function(object, estimate,
                        newdata, ncomp = 1:object$ncomp, comps,
                        intercept = cumulative, se = FALSE, ...)
{
    ## Makes the code slightly simpler:
    cumulative <- missing(comps) || is.null(comps)

    if (any(estimate == "CV")) {
        ## Check that cross-validation is possible:
        if (!cumulative)
            stop("Cross-validation is not supported when `comps' is specified")
        if (is.null(object$validation))
            stop("`object' has no `validation' component")
    }

    ## The calculated stuff:
    nestimates <- length(estimate)
    nresp <- dim(fitted(object))[2]
    respnames <- dimnames(fitted(object))[[2]]
    SSE <- array(dim = c(nestimates, nresp,
                         if(cumulative) 1 + length(ncomp) else 2),
                 dimnames = list(estimate = estimate,
                 response = respnames,
                 model = if (cumulative) {
                     c("(Intercept)", paste(ncomp, "comps"))
                 } else {
                     c("(Intercept)", paste("(Intercept), Comp",
                                            paste(comps, collapse = ", ")))
                 }
                 ))
    SST <- array(dim = c(nestimates, nresp),
                 dimnames = list(estimate = estimate, response = respnames))
    nobj <- numeric(nestimates)
    names(nobj) <- estimate

    ## Calculate the statistics:
    for (i in seq(along = estimate)) {
        switch(estimate[i],
               train = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   nobj[i] <- nrow(resp)
                   if (inherits(object$na.action, "exclude")) {
                       resp <- napredict(object$na.action, resp) # inserts NAs
                   }
                   res <- if (cumulative)
                       residuals(object, ...)[,,ncomp, drop=FALSE]
                   else
                       resp - predict(object, comps = comps, ...)

                   SST[i,] <- apply(resp, 2, var, na.rm = TRUE) *
                       (nobj[i] - 1)
                   SSE[i,,] <- cbind(SST[i,], colSums(res^2, na.rm = TRUE))
               },
               test = {
                   if (missing(newdata)) stop("Missing `newdata'.")
                   ## Remove any observations with NAs:
                   newdata <- model.frame(formula(object), data = newdata)
                   resp <- as.matrix(model.response(newdata))
                   pred <- if (cumulative)
                       predict(object, ncomp = ncomp, newdata = newdata,...)
                   else
                       predict(object, comps = comps, newdata = newdata,...)
                   nobj[i] <- nrow(newdata)
                   SST[i,] <- apply(resp, 2, var) * (nobj[i] - 1)
                   SSE[i,,] <- cbind(colSums(sweep(resp, 2, object$Ymeans)^2),
                                    colSums((pred - c(resp))^2))
               },
               CV = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   nobj[i] <- nrow(resp)
                   SST[i,] <- apply(resp, 2, var) * (nobj[i] - 1)
                   SSE[i,,] <-
                       cbind(object$validation$PRESS0,
                             object$validation$PRESS[,ncomp, drop=FALSE])
               }
               )
    }

    if (cumulative) comps <- ncomp
    ## Either remove the intercept or add a "zeroth" component:
    if (intercept)
        comps <- c(0, comps)
    else
        SSE <- SSE[,,-1, drop=FALSE]

    return(list(SSE = SSE, SST = SST, nobj = nobj, comps = comps,
                cumulative = cumulative))
}


## R2: Return R^2
R2 <- function(object, ...) UseMethod("R2")
R2.mvr <- function(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
               intercept = cumulative, se = FALSE, ...) {
    ## Makes the code slightly simpler:  FIXME: maybe remove
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

    ## Get the needed validation statistics:
    cl <- match.call(expand.dots = FALSE)
    cl$estimate <- estimate             # update estimate argument
    cl[[1]] <- as.name("mvrValstats")
    valstats <- eval(cl, parent.frame())

    ## Calculate the R^2s:
    R2 <- 1 - valstats$SSE / c(valstats$SST)

    return(structure(list(val = R2, type = "R2", comps = valstats$comps,
                          cumulative = valstats$cumulative, call = match.call()),
                     class = "mvrVal"))
}


## MSEP: Return MSEP
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

    ## adjCV needs the statistics for CV and train, so we optionally
    ## have to add them:
    if (adjCV <- any(estimate == "adjCV")) {
        ## Note: this removes any duplicate elements
        calcestimates <- union(estimate, c("train", "CV"))
    } else {
        calcestimates <- estimate
    }
    ## Get the needed validation statistics:
    cl <- match.call(expand.dots = FALSE)
    cl$estimate <- calcestimates        # update estimate argument
    cl[[1]] <- as.name("mvrValstats")
    valstats <- eval(cl, parent.frame())

    ## Calculate the MSEPs:
    MSEP <- valstats$SSE / valstats$nobj
    if (adjCV) {
        ## Calculate the adjusted CV
        MSEP["adjCV",,] <- MSEP["CV",,]
        if (intercept) {
            MSEP["adjCV",,-1] <- MSEP["adjCV",,-1] + MSEP["train",,-1] -
                object$validation$adj[,ncomp]
        } else {
            MSEP["adjCV",,] <- MSEP["adjCV",,] + MSEP["train",,] -
                object$validation$adj[,ncomp]
        }
        ## Remove any specially added estimates (this also adds back any
        ## duplicate elements):
        MSEP <- MSEP[estimate,,, drop=FALSE]
    }

    return(structure(list(val = MSEP, type = "MSEP", comps = valstats$comps,
                          cumulative = valstats$cumulative, call = match.call()),
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
