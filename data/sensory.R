### sensory.R: set up the sensory data set
### $Id$

sensory <- data.frame(Quality = I(as.matrix(read.table("quality"))),
                      Panel = I(as.matrix(read.table("panel"))))
