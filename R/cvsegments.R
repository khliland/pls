### cvsegments.R: A utility function to generate segments for k-fold
### cross-validation.
### $Id$

cvsegments <- function(N, k, length.seg = ceiling(N / k),
                       type = c("random", "consecutive", "interleaved")) {
    ## length.seg overrides k:
    if (!missing(length.seg)) k <- ceiling(N / length.seg)
    incomplete <- k * length.seg - N    # Number of incomplete segments
    complete <- k - incomplete          # Number of complete segments

    ## The idea is to generate a k times length.seg matrix with indices, and
    ## use each row as a segment.  If k*length.seg > N, the last element of
    ## the N - k*length.seg last rows will be NA.  Any NAs are stripped when
    ## the matrix is converted to a list of vectors.

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
    res <- lapply(as.data.frame(inds), function(x) c(na.omit(x)))
    attr(res, "incomplete") <- incomplete
    attr(res, "type") <- if (length.seg == 1) "leave-one-out" else type
    res
}
