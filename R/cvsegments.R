### cvsegments.R: A utility function to generate segments for k-fold
### cross-validation.
### $Id$

cvsegments <- function(N, k, length.seg = ceiling(N / k), nrep = 1,
                       type = c("random", "consecutive", "interleaved")) {
    ## length.seg overrides k:
    if (!missing(length.seg)) k <- ceiling(N / length.seg)

    ## Check arguments:
    if (k > N) stop("More segments than observations requested")
    if (N %% nrep != 0)
        stop("The number of replicates does not divide ",
             "the number of observations")
    if (length.seg %% nrep != 0)
        warning("Segment length is not a multiple of the number of ",
                "replicates.\n  A best effort segment size will be used.")
    if (!missing(length.seg) && N %% length.seg != 0)
        warning("Required segment length does not divide the number of ",
                "observations.\n  A best effort segment size will be used.")

    ## The idea is to generate a k times length.seg matrix with indices, and
    ## use each coloum as a segment.  If k*length.seg > N, the last element of
    ## the N - k*length.seg last rows will be NA.  Any NAs are stripped when
    ## the matrix is converted to a list of vectors.

    ## If nrep > 1, N and length.seg is first divided by nrep, and the matrix
    ## of indices is created as above.  The matrix is then expanded by
    ## replacing each element i with a coloumn vector nrep * (i - 1) + 1:nrep,
    ## before using the coloumns as segments.

    ## Reduce N and length.seg if needed
    if (nrep > 1) {
        N <- N / nrep
        length.seg <- ceiling(N / k)
    }

    incomplete <- k * length.seg - N    # Number of incomplete segments
    complete <- k - incomplete          # Number of complete segments

    ## Create matrix of indices
    type <- match.arg(type)
    switch(type,
           random = {
               inds <- matrix(c(sample(1:N), rep(NA, incomplete)),
                              nrow = length.seg, byrow = TRUE)
           },
           consecutive = {
               if (complete < k) {
                   inds <- cbind(matrix(1:(length.seg*complete),
                                        nrow = length.seg),
                                 rbind(matrix((length.seg*complete+1):N,
                                              nrow = length.seg-1), NA))
               } else {
                   inds <- matrix(1:N, nrow = length.seg)
               }
           },
           interleaved = {
               inds <- matrix(c(1:N, rep(NA, incomplete)),
                              nrow = length.seg, byrow = TRUE)
           }
           )

    ## Expand matrix if needed
    if (nrep > 1) {
        inds <- outer(1:nrep, nrep * (inds - 1), '+')
        dims <- dim(inds)
        dim(inds) <- c(dims[1] * dims[2], dims[3])
    }

    ## Convert to list of segments defined by the columns, and add attributes
    res <- lapply(as.data.frame(inds), function(x) c(na.omit(x)))
    attr(res, "incomplete") <- incomplete
    attr(res, "type") <- if (length.seg == 1) "leave-one-out" else type
    res
}
