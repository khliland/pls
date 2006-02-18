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

### Old stuff:




# ## MSEP.bootstrap: estimate MSEP with different bootstrap estimates:
# MSEP.bootstrap <- function (obj, A.max = obj$A, A = 1:A.max,
#                             R = 100, trace = FALSE) {
#   if (!require ("boot")) stop ("Could not load required package boot.\n")
#   ## Multiple responses is not supported sofar:
#   if (is.matrix (obj$Y.mean))
#     stop ("Multiresponse regression is not supported.")

#   ## I'm using the terminology of section 6.5 in Davison & Hinkley.
#   data <- model.frame (obj)
#                                         # Because of R's scoping, I skip
#                                         # sending obj and A to boot.fun.
#   boot.fun <- function (data, ind, trace) {
#     if (trace) cat ("*")
#     fit <- update (obj, data = data[ind,])# This takes time!
#     D.hatFstar.hatFstar <- MSEP (fit, A = A)
#     pred <- drop (predict (fit, newdata = data, A = A))
#     resp <- model.response (model.frame (formula (obj), data = data))
#     D.hatF.hatFstar <- apply (sweep (pred, 1, resp)^2, 2, mean)
#     vekk <- unique (ind)
#     D.hatFm.hatFstar <- if (length (vekk) < nrow (data))
#       apply (sweep (pred[-vekk,], 1, resp[-vekk])^2, 2, mean)
#     else rep (0, length (D.hatF.hatFstar))
#     return (c (D.hatFstar.hatFstar, D.hatF.hatFstar, D.hatFm.hatFstar, pred))
#   }

#   boot.res <- boot (data, boot.fun, R, trace = trace)
#   if (trace) cat ("\n")

#   nA <- length (A)
#   D.hatF.hatF <- boot.res$t0[1:nA]      # t0 starts with two copies of the apparent MSEP
#   D.hatFstar.hatFstar <- apply (boot.res$t[,1:nA], 2, mean)
#   naive.bootstrap <- apply (boot.res$t[,nA+(1:nA)], 2, mean)#(6.49)
#   D.BCV2 <- apply (boot.res$t[,2*nA+(1:nA)], 2, mean)
#   basic.bootstrap <- naive.bootstrap - D.hatFstar.hatFstar + D.hatF.hatF#(6.51)
#   basic2.bootstrap <- D.BCV2 - D.hatFstar.hatFstar + D.hatF.hatF

#   ## Restructure the preds into an R x Ntrain x nA array of squared errors
#   sqerr <- boot.res$t[,-seq (1, 3*nA)]        # Skip the D's
#   Ntrain <- dim (sqerr)[2] / nA
#   dim (sqerr) <- c (R, Ntrain, nA)
#   resp <- model.response (model.frame (obj))
#   sqerr <- sweep (sqerr, 2, resp)^2

#   ## Calculate the Bootstrap smoothed Cross Validation estimate:
#   D.BCV <- rep (0, nA)                  #(6.54)
#   freq <- boot.array (boot.res)
#   Ncv <- Ntrain                         # The number of objects used
#   for (i in 1:Ntrain) {
#     ind <- which (freq[,i] == 0)
#     if (length (ind) > 1)
#       D.BCV <- D.BCV + apply (sqerr[ind,i,], 2, mean)
#     else if (length (ind) == 1)
#       D.BCV <- D.BCV + sqerr[ind,1,]
#     else
#       Ncv <- Ncv - 1                    # Skip this object
#   }
#   D.BCV <- D.BCV / Ncv

#   ## The original 0.632 estimate:
#   D.632o <- 0.632 * D.BCV2 + 0.368 * D.hatF.hatF

#   ## The 0.632 estimate:
#   D.632 <- 0.632 * D.BCV + 0.368 * D.hatF.hatF#(6.55)

#   ## The 2/3 estimates:
#   D.667 <- 2/3 * D.BCV + 1/3 * D.hatF.hatF
#   D.667o <- 2/3 * D.BCV2 + 1/3 * D.hatF.hatF

#   ## The 0.632+ estimate (Efron & Tibshirani, 1995)
#   pred <- as.matrix (drop (fitted (obj)[,A,]))
#   gamma <- apply (pred^2 - 2*obj$Y.mean*pred, 2, mean) + mean (resp^2)
#   ## FIXME: This is not robust:
#   Rhat <-(D.BCV - D.hatF.hatF) / (gamma - D.hatF.hatF)
#   D.632p <- D.632 + (D.BCV - D.hatF.hatF) * 0.368*0.632*Rhat / (1 - 0.368*Rhat)

#   return (list (MSEP = cbind (apparent = D.hatF.hatF,
#                   naive.boot = naive.bootstrap,
#                   basic.boot = basic.bootstrap,
#                   basic2.boot = basic2.bootstrap,
#                   BCV = D.BCV,
#                   BCV2 = D.BCV2,
#                   "632" = D.632,
#                   "632o" = D.632o,
#                   "667" = D.667,
#                   "667o" = D.667o,
#                   "632+" = D.632p),
#                 boot = boot.res))
# }
