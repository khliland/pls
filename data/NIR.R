### NIR.R: set up the NIR data set
### $Id$

train <- data.frame(X = I(matrix(scan("xtrainNIR", quiet = TRUE),
                                 nrow = 21, byrow = TRUE)),
                    y = scan("ytrainNIR", quiet = TRUE))
test <- data.frame(X = I(matrix(scan("xtestNIR", quiet = TRUE),
                                nrow = 7, byrow = TRUE)),
                   y = scan("ytestNIR", quiet = TRUE))
NIR <- rbind(train, test)
NIR$train <- c(rep(TRUE, nrow(train)), rep(FALSE, nrow(test)))

rm(train, test)
