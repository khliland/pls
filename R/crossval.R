### crossval.R: Genereral cross-validation function.
### $Id$

crossval <- function(object, segments = 10,
                     segment.type = c("random", "consecutive", "interleaved"),
                     length.seg, trace = 15, ...)
{
    if(!inherits(object, "mvr")) stop("`object' not an mvr object.")
    ## Get data frame
    fitCall <- object$call
    data <- eval(fitCall$data, parent.frame())

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
    npred <- dim(Y)[2]
    ## Calculate effective number of observations
    n <- nrow(data)

    ## Set up segments
    if (is.list(segments)) {
        attr(segments, "type") <- "user supplied"
    } else {
        if (missing(length.seg)) {
            segments <- cvsegments(n, k = segments, type = segment.type)
        } else {
            segments <- cvsegments(n, length.seg = length.seg,
                                   type = segment.type)
        }
    }

    ncomp <- object$ncomp
    cvPred <- array(dim = c(n, npred, ncomp))
    adj <- numeric(ncomp)

    ## Run cv, using update and predict
    for (n.seg in 1:length(segments)) {
        if (n.seg == 1) trace.time <- proc.time()[3] # Can't use system.time!
        seg <- segments[[n.seg]]
        fit <- update(object, data = data[-seg,])
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

    ## Calculate MSEP
    MSEP0 <- apply(Y, 2, var) * n / (n - 1) # FIXME: Only correct for loocv!
    MSEP <- colMeans((cvPred - c(Y))^2)

    ## Calculate R2:
    R2 <- matrix(nrow = npred, ncol = ncomp)
    for (i in 1:npred) R2[i,] <- cor(cvPred[,i,], Y[,i])^2

    ## Add dimnames:
    objnames <- rownames(data)
    if (is.null(objnames)) objnames <- rownames(Y)
    dimnames(cvPred) <- c(list(objnames), dimnames(fitted(object))[-1])
    dimnames(MSEP) <- dimnames(R2) <- dimnames(adj)

    ## Return the original object, with a component `CV' added
    object$CV <- list(pred = cvPred,
                      MSEP0 = MSEP0, MSEP = MSEP, adj = adj / n^2,
                      R2 = R2, segments = segments)
    return(object)
}
