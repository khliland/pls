### crossval.R: Genereral cross-validation function.
### $Id$

crossval <- function(object, segments = 10,
                     segment.type = c("random", "consecutive", "interleaved"),
                     length.seg, jackknife = FALSE, trace = 15, ...)
{
    if(!inherits(object, "mvr")) stop("`object' not an mvr object.")
    ## Get data frame
    fitCall <- object$call
    data <- eval(fitCall$data, parent.frame())
    if (is.null(data)) stop("`object' must be fit with a `data' argument.")

    if (!is.null(fitCall$subset)) {
        ## Handle "subset" argument
        data <- data[eval(fitCall$subset, parent.frame()),]
        object$call$subset <- NULL
    }

    ## Handle NAs (according to na.action)
    if (is.na(match("na.action", names(fitCall)))) {
        ## Cannot use is.null(fitCall$na.action) here, since the meaning of
        ## `na.action = NULL' is not the same as that of a missing na.action
        ## argument.
        mf <- model.frame(formula(object), data = data)
    } else {
        mf <- model.frame(formula(object), data = data,
                          na.action = fitCall$na.action)
    }
    if(!is.null(NAs <- attr(mf, "na.action"))) {
        ## Some observations were dropped due to NAs.  Skip the same in data:
        data <- data[-NAs,]
    }

    ## Get response:
    Y <- as.matrix(model.response(mf))
    nresp <- dim(Y)[2]
    npred <- length(object$Xmeans)
    ## Calculate effective number of observations
    nobj <- nrow(data)

    ## Set up segments
    if (is.list(segments)) {
        if (is.null(attr(segments, "type")))
            attr(segments, "type") <- "user supplied"
    } else {
        if (missing(length.seg)) {
            segments <- cvsegments(nobj, k = segments, type = segment.type)
        } else {
            segments <- cvsegments(nobj, length.seg = length.seg,
                                   type = segment.type)
        }
    }

    ncomp <- object$ncomp
    if (ncomp > nobj - max(sapply(segments, length)) - 1)
        stop("`ncomp' too large for cross-validation.",
             "\nPlease refit with `ncomp' less than ",
             nobj - max(sapply(segments, length)))
    cvPred <- array(dim = c(nobj, nresp, ncomp))
    adj <- matrix(0, nrow = nresp, ncol = ncomp)
    if (jackknife <- isTRUE(jackknife))
        cvCoef <- array(dim = c(npred, nresp, ncomp, length(segments)))

    ## Run cv, using update and predict
    for (n.seg in 1:length(segments)) {
        if (n.seg == 1) trace.time <- proc.time()[3] # Can't use system.time!
        seg <- segments[[n.seg]]
        fit <- update(object, data = data[-seg,])
        ## Optionally collect coefficients:
        if (jackknife) cvCoef[,,,n.seg] <- fit$coefficients
        pred <- predict(fit, newdata = data)
        ## Update the adjMSEP adjustment:
        adj <- adj + length(seg) * colSums((pred - c(Y))^2)
        ## Save the cross-validated predictions:
        cvPred[seg,,] <- pred[seg,,]
        if (n.seg == 1) {
            if (is.numeric(trace)) {
                trace.time <- proc.time()[3] - trace.time
                trace <- trace.time * length(segments) > trace
            }
            if (trace) cat("Segment: ")
        }
        if (trace) cat(n.seg, "")
    }
    if (trace) cat("\n")

    ## Calculate validation statistics:
    PRESS0 <- apply(Y, 2, var) * nobj^2 / (nobj - 1) # FIXME: Only correct for loocv!
    PRESS <- colSums((cvPred - c(Y))^2)

    ## Add dimnames:
    objnames <- rownames(data)
    if (is.null(objnames)) objnames <- rownames(Y)
    dimnames(cvPred) <- c(list(objnames), dimnames(fitted(object))[-1])
    if (is.null(names(PRESS0))) names(PRESS0) <- dimnames(object$Yloadings)[[1]]
    dimnames(PRESS) <- dimnames(adj)
    if (jackknife)
        dimnames(cvCoef) <- c(dimnames(coef(object)),
                              list(paste("Seg", seq.int(along = segments))))

    ## Return the original object, with a component `validation' added
    object$validation <- list(method = "CV", pred = cvPred,
                              coefficients = if (jackknife) cvCoef,
                              PRESS0 = PRESS0, PRESS = PRESS,
                              adj = adj / nobj^2,
                              segments = segments, ncomp = ncomp)
    return(object)
}
