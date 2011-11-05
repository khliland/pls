### mvrCv.R: The basic cross-validation function
### $Id$

mvrCv <- function(X, Y, ncomp, Y.add = NULL, weights = NULL,
                  method = pls.options()$mvralg,
                  scale = FALSE, segments = 10,
                  segment.type = c("random", "consecutive", "interleaved"),
                  length.seg, jackknife = FALSE, trace = FALSE, SMP = NULL, ...)
{
    ## Initialise:
    Y <- as.matrix(Y)
	if(!(missing(Y.add) || is.null(Y.add)))
		Y.add <- as.matrix(Y.add)
    ## Save dimnames:
    dnX <- dimnames(X)
    dnY <- dimnames(Y)
    ## Remove dimnames for performance (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations).
    ## dimnames(X) <- dimnames(Y) <- NULL

    nobj <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    ## Check the `scale' parameter:
    if (!is.logical(scale) || length(scale) != 1)
        stop("'scale' must be 'TRUE' or 'FALSE'")

    ## Set up segments:
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

	## Test for constant response in segments
	cv.sd <- matrix(0, length(segments), nresp)
	for(a in 1:nresp){ # Main response(s)
		cv.sd[,a] <- sapply(1:length(segments),function(i) sd(Y[setdiff(1:nobj,segments[[i]]),a]))
	}
	cv.sd[is.na(cv.sd)] <- 0
	cv.const <- apply(cv.sd==0,1,all)
	if(any(cv.const)){
		cv.num <- which(cv.const)
		stop(paste("Constant response(s) in CV segment: ", paste(cv.num, sep="",collapse=","), sep=""))
	}
	if(!(missing(Y.add) || is.null(Y.add))){ # Additional response(s)
		cv.sd <- matrix(0, length(segments), nresp)
		for(a in 1:dim(Y.add)[2]){ # Main response(s)
			cv.sd[,a] <- sapply(1:length(segments),function(i) sd(Y.add[setdiff(1:nobj,segments[[i]]),a]))
		}
		cv.sd[is.na(cv.sd)] <- 0
		cv.const <- apply(cv.sd==0,1,all)
		if(any(cv.const)){
			cv.num <- which(cv.const)
			stop(paste("Constant additional response(s) in CV segment: ", paste(cv.num, sep="",collapse=","), sep=""))
		}
	}

    ## Reduce ncomp, if neccessary:
    ncomp <- min(ncomp, nobj - max(sapply(segments, length)) - 1)

    ## Select fit function:
    method <- match.arg(method,c("kernelpls", "widekernelpls", "simpls", "oscorespls", "cppls", "plsda", "svdpc"))
    fitFunc <- switch(method,
                      kernelpls = kernelpls.fit,
                      widekernelpls = widekernelpls.fit,
                      simpls = simpls.fit,
                      oscorespls = oscorespls.fit,
                      cppls = cppls.fit,
                      plsda = plsda.fit,
                      svdpc = svdpc.fit)

    ## Variables to save CV results in:
    adj <- matrix(0, nrow = nresp, ncol = ncomp)
    cvPred <- pred <- array(0, dim = c(nobj, nresp, ncomp))
    if (jackknife)
        cvCoef <- array(dim = c(npred, nresp, ncomp, length(segments)))
    if (method == "cppls") gammas <- list()

    if (trace) cat("Segment: ")

	## Optionally set up SMP:
	doTheSMP <- FALSE
	w <- NULL
	if(!is.null(SMP)){
		if(SMP[1] %in% c("doSMP","doMC","doSNOW","doMPI","doRedis")){
			OK.SMP <- FALSE
			if(SMP[1] == "doSMP")
				OK.SMP <- require(doSMP)
			if(SMP[1] == "doMC")
				OK.SMP <- require(doMC)
			if(SMP[1] == "doSNOW")
				OK.SMP <- require(doSNOW)
			if(SMP[1] == "doMPI")
				OK.SMP <- require(doMPI)
			if(SMP[1] == "doRedis")
				OK.SMP <- require(doRedis)
			if(OK.SMP){
				w <- startWorkers(workerCount=SMP[2])
				registerDoSMP(w)
				doTheSMP <- TRUE
			} else {
				error('SMP setup failed not available')
			}
		} else {
			error('Only doSMP, doMC, doSNOW, doMPI and doRedis are supported')
		}
	}

	# Perform SMP
	if(doTheSMP){
		cvSMP <- function(n.seg, segments, X, scale=FALSE, Y, Y.add, weights, jackknife, method, nobj, nresp, ncomp, fitFunc, gammas, ...){
			# if (trace) cat(n.seg, "")

			## Set up train data:
			seg <- segments[[n.seg]]
			Xtrain <- X[-seg,]
			if (scale) {
				ntrain <- nrow(Xtrain)
				## This is faster than sd(X), but cannot handle missing values:
				sdtrain <-
					sqrt(colSums((Xtrain - rep(colMeans(Xtrain), each = ntrain))^2) /
						 (ntrain - 1))
				if (any(abs(sdtrain) < .Machine$double.eps^0.5))
					warning("Scaling with (near) zero standard deviation")
				Xtrain <- Xtrain / rep(sdtrain, each = ntrain)
			}

			## Fit the model:
			fit <- fitFunc(Xtrain, Y[-seg,], ncomp, Y.add = Y.add[-seg,], stripped = TRUE, weights = weights[-seg], ...)

			## Optionally collect coefficients:
			if (jackknife) cvCoef <- fit$coefficients

			## Optionally collect gamma-values from CPPLS
			if (method == "cppls") gammas[[n.seg]] <- fit$gammas

			## Set up test data:
			Xtest <- X
			if (scale) Xtest <- Xtest / rep(sdtrain, each = nobj)
			Xtest <- Xtest - rep(fit$Xmeans, each = nobj)

			## Predict test data:
			Ymeansrep <- rep(fit$Ymeans, each = nobj)
			pred <- array(0, dim = c(nobj, nresp, ncomp))
			for (a in 1:ncomp)
				pred[,,a] <- Xtest %*% fit$coefficients[,,a] + Ymeansrep

			## Save the cross-validated predictions:
			cvPred <- pred[seg,,, drop=FALSE]
			adj <- length(seg) * colSums((pred - c(Y))^2)

			# Output
			list(cvCoef=ifelse(jackknife,cvCoef,0), gammas=ifelse(method=="cppls",gammas,0), cvPred=cvPred, adj=adj)
		}

		SMPs <- foreach(n.seg = 1:length(segments)) %dopar% cvSMP(n.seg, segments=segments, X=X, scale=scale, Y=Y, Y.add=Y.add, weights=weights, jackknife=jackknife, method=method, nobj=nobj, nresp=nresp, ncomp=ncomp, fitFunc=fitFunc, gammas=ifelse(method=="cppls",gammas,0), ...)
		for (n.seg in 1:length(segments)) {
			if(jackknife)
				cvCoef[,,,n.seg] <- SMPs[[n.seg]]$cvCoef
			if(method=="cppls")
				gammas[[n.seg]] <- SMPs[[n.seg]]$gammas
			cvPred[segments[[n.seg]],,]  <- SMPs[[n.seg]]$cvPred
			adj <-  adj + SMPs[[n.seg]]$adj
		}
		stopWorkers(w)

	# No SMP
	} else {
		for (n.seg in 1:length(segments)) {
			if (trace) cat(n.seg, "")

			## Set up train data:
			seg <- segments[[n.seg]]
			Xtrain <- X[-seg,]
			if (scale) {
				ntrain <- nrow(Xtrain)
				## This is faster than sd(X), but cannot handle missing values:
				sdtrain <-
					sqrt(colSums((Xtrain - rep(colMeans(Xtrain), each = ntrain))^2) /
						 (ntrain - 1))
				if (any(abs(sdtrain) < .Machine$double.eps^0.5))
					warning("Scaling with (near) zero standard deviation")
				Xtrain <- Xtrain / rep(sdtrain, each = ntrain)
			}

			## Fit the model:
			fit <- fitFunc(Xtrain, Y[-seg,], ncomp, Y.add = Y.add[-seg,],
				stripped = TRUE, weights = weights[-seg], ...)

			## Optionally collect coefficients:
			if (jackknife) cvCoef[,,,n.seg] <- fit$coefficients

			## Set up test data:
			Xtest <- X
			if (scale) Xtest <- Xtest / rep(sdtrain, each = nobj)
			Xtest <- Xtest - rep(fit$Xmeans, each = nobj)

			## Predict test data:
			Ymeansrep <- rep(fit$Ymeans, each = nobj)
			for (a in 1:ncomp)
				pred[,,a] <- Xtest %*% fit$coefficients[,,a] + Ymeansrep

			## Save the cross-validated predictions:
			cvPred[seg,,] <- pred[seg,,, drop=FALSE]
			adj <- adj + length(seg) * colSums((pred - c(Y))^2)
		}
	}
    if (trace) cat("\n")

    ## Calculate validation statistics:
    PRESS0 <- apply(Y, 2, var) * nobj^2 / (nobj - 1) # FIXME: Only correct for loocv!
    PRESS <- colSums((cvPred - c(Y))^2)

    ## Add dimnames:
    objnames <- dnX[[1]]
    if (is.null(objnames)) objnames <- dnY[[1]]
    respnames <- dnY[[2]]
    nCompnames <- paste(1:ncomp, "comps")
    names(PRESS0) <- respnames
    dimnames(adj) <- dimnames(PRESS) <-
        list(respnames, nCompnames)
    dimnames(cvPred) <- list(objnames, respnames, nCompnames)
    if (jackknife)
        dimnames(cvCoef) <- list(dnX[[2]], respnames, nCompnames,
                                 paste("Seg", seq.int(along = segments)))

    list(method = "CV", pred = cvPred, coefficients = if (jackknife) cvCoef,
         gammas = if (method == "cppls") gammas,
         PRESS0 = PRESS0, PRESS = PRESS, adj = adj / nobj^2,
         segments = segments, ncomp = ncomp)
}
