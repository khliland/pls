### cvsegments.R: A utility function to generate segments for k-fold
### cross-validation.



#' @title Generate segments for cross-validation
#'
#' @description The function generates a list of segments for cross-validation.  It can
#' generate random, consecutive and interleaved segments, and supports keeping
#' replicates in the same segment.
#'
#' @details If \code{length.seg} is specified, it is used to calculate the number of
#' segments to generate.  Otherwise \code{k} must be specified.  If
#' \eqn{k*length.seg \ne N}{k*length.seg <> N}, the \eqn{k*length.seg - N} last
#' segments will contain only \eqn{length.seg - 1} indices.
#'
#' If \code{type} is \code{"random"}, the indices are allocated to segments in
#' random order.  If it is \code{"consecutive"}, the first segment will contain
#' the first \eqn{length.seg} indices, and so on.  If \code{type} is
#' \code{"interleaved"}, the first segment will contain the indices \eqn{1,
#' length.seg+1, 2*lenght.seg+1, \ldots, (k-1)*length.seg+1}, and so on.
#'
#' If \eqn{nrep > }, it is assumed that each \code{nrep} consecutive rows are
#' replicates (repeated measurements) of the same object, and care is taken
#' that replicates are never put in different segments.
#'
#' Warning: If \code{k} does not divide \code{N}, a specified \code{length.seg}
#' does not divide \code{N}, or \code{nrep} does not divide \code{length.seg},
#' the number of segments and/or the segment length will be adjusted as needed.
#' Warnings are printed for some of these cases, and one should always inspect
#' the resulting segments to make sure they are as expected.
#'
#' Stratification of samples is enabled by the \code{stratify} argument.  This
#' is useful if there are sub-groups in the data set that should have a
#' proportional representation in the cross-validation segments or if the
#' response is categorical (classifiation). If \code{stratify} is combined with
#' \code{nrep}, \code{stratify} corresponds to the sets of replicates (see
#' example).
#'
#' @param N Integer.  The number of rows in the data set.
#' @param k Integer.  The number of segments to return.
#' @param length.seg Integer.  The length of the segments.  If given, it
#' overrides \code{k}.
#' @param nrep Integer.  The number of (consecutive) rows that are replicates
#' of the same object.  Replicates will always be kept in the same segment.
#' @param type One of \code{"random"}, \code{"consecutive"} and
#' \code{"interleaved"}.  The type of segments to generate.  Default is
#' \code{"random"}.
#' @param stratify Either a \code{list} of indices or an integer \code{vector}
#' indicating which stratum each sample (or set of replicates) belongs to (see
#' Details).
#' @return A list of vectors.  Each vector contains the indices for one
#' segment.  The attribute \code{"incomplete"} contains the number of
#' incomplete segments, and the attribute \code{"type"} contains the type of
#' segments.
#' @author Bjørn-Helge Mevik, Ron Wehrens and Kristian Hovde Liland
#' @keywords models
#' @examples
#'
#' ## Segments for 10-fold randomised cross-validation:
#' cvsegments(100, 10)
#'
#' ## Segments with four objects, taken consecutive:
#' cvsegments(60, length.seg = 4, type = "cons")
#'
#' ## Incomplete segments
#' segs <- cvsegments(50, length.seg = 3)
#' attr(segs, "incomplete")
#'
#' ## Leave-one-out cross-validation:
#' cvsegments(100, 100)
#' ## Leave-one-out with variable/unknown data set size n:
#' n <- 50
#' cvsegments(n, length.seg = 1)
#'
#' ## Data set with replicates
#' cvsegments(100, 25, nrep = 2)
#' ## Note that rows 1 and 2 are in the same segment, rows 3 and 4 in the
#' ## same segment, and so on.
#'
#' ## Stratification
#' cvsegments(10, 3, type = "consecutive", stratify = c(rep(1,7), rep(2,3)))
#' ## Note that the last three samples are spread across the segments
#' ## according to the stratification vector.
#' cvsegments(20, 3, type = "consecutive", nrep = 2, stratify = c(rep(1,7), rep(2,3)))
#' ## Note the length of stratify matching number of replicate sets, not samples.
#'
#' @export
cvsegments <- function(N, k, length.seg = ceiling(N / k), nrep = 1,
                            type = c("random", "consecutive", "interleaved"), stratify = NULL)
{
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
    ## use each column as a segment.  If k*length.seg > N, the last element of
    ## the N - k*length.seg last rows will be NA.  Any NAs are stripped when
    ## the matrix is converted to a list of vectors.

    ## If nrep > 1, N and length.seg is first divided by nrep, and the matrix
    ## of indices is created as above.  The matrix is then expanded by
    ## replacing each element i with a column vector nrep * (i - 1) + 1:nrep,
    ## before using the columns as segments.

    ## Reduce N and length.seg if needed
    if (nrep > 1) {
        N <- N / nrep
        length.seg <- ceiling(N / k)
    }

    incomplete <- k * length.seg - N    # Number of incomplete segments
    complete   <- k - incomplete        # Number of complete segments

    ## Create matrix of indices
    type <- match.arg(type)
    if (is.null(stratify)) { # No stratification
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
    } else {

        ## Prepare stratification
        if (!is.list(stratify)) {
            stratify <- lapply(1:max(stratify), function(i)which(stratify==i))
        }
        stratVec <- unlist(lapply(1:length(stratify), function(i)rep(i,length(stratify[[i]]))))
        indVec   <- unlist(stratify)
        segs     <- integer(N)
        blinds   <- rep(1:k, length.out=N)
        switch(type,
               random = {
                   stratRand <- sample(length(stratify))
                   stratify  <- stratify[stratRand]
                   indVec    <- unlist(stratify)
                   stratVec  <- unlist(lapply(1:length(stratify), function(i)rep(i,length(stratify[[i]]))))
                   for(i in 1:length(stratify)){
                       segs[stratify[[i]]] <- blinds[indVec[stratVec==i]]
                   }
                   inds <- lapply(1:k,function(i)indVec[segs==i])
                   inds <- lapply(inds, function(i)i[sample(length(i))])
                   if(incomplete>0)
                       inds[-(1:complete)] <- lapply(inds[-(1:complete)], function(i){i[length.seg] <- NA;i})
                   inds <- do.call(cbind, inds)
                   inds <- inds[,sample(k)]
               },
               consecutive = {
                   for(i in 1:length(stratify)){
                       blinds[stratVec==i] <- sort(blinds[stratVec==i])
                   }
                   for(i in 1:length(stratify)){
                       segs[stratify[[i]]] <- blinds[stratVec==i]
                   }
                   inds <- lapply(1:k,function(i)which(segs==i))
                   if(incomplete>0)
                       inds[-(1:complete)] <- lapply(inds[-(1:complete)], function(i){i[length.seg] <- NA;i})
                   inds <- do.call(cbind, inds)
               },
               interleaved = {
                   for(i in 1:length(stratify)){
                       segs[stratify[[i]]] <- blinds[indVec[stratVec==i]]
                   }
                   inds <- lapply(1:k,function(i)indVec[segs==i])
                   if(incomplete>0)
                       inds[-(1:complete)] <- lapply(inds[-(1:complete)], function(i){i[length.seg] <- NA;i})
                   inds <- do.call(cbind, inds)
               }
        )
    }

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
