### da.R: Discriminant analysis functions
### $Id: da.R 75 2006-07-17 21:05:34Z bhm $

## Efficient quadratic discriminant analysis (LDA)
.lda <- function(X, Y, Xnew, Prior, posterior=FALSE){
	# Create dummy response and numeric response from factor
	Yd <- class.ind(Y)
	Y  <- as.numeric(Y)

	if(missing(Xnew))
		Xnew <- X

	# Dimensions
	n     <- dim(X)[1]
	g     <- dim(Yd)[2]
	n.new <- dim(Xnew)[1]

	if(missing(Prior))
		Prior <- apply(Yd,2,sum)/n

	Xs  <- solve(crossprod(Yd),t(Yd))%*%X # Mean for each group
	Xc  <- X-Xs[Y,,drop=FALSE]            # Centering
	S   <- solve(crossprod(Xc)/(n-g))     # Common inverse covariance matrix
	SXs <- tcrossprod(S,Xs) 			  # Common element across all observations
	d   <- Xnew%*%SXs + matrix(-0.5*diag(Xs%*%SXs) + log(Prior), n.new, g, byrow=TRUE)

	# Full LDA with posterior values
	if(posterior){
		d <- exp( d + matrix(-0.5*diag(Xnew%*%tcrossprod(S,Xnew)),n.new,g) )
		d <- d / matrix(apply(d,1,sum),n.new,g)
	}

	group <- apply(d,1,which.max)  # Finds the most probable group
	if(posterior){
		return(list(group=group,posterior=d))
	} else {
		return(list(group=group))
	}
}

## Efficient quadratic discriminant analysis (QDA)
.qda <- function(X, Y, Xnew, Prior, posterior=FALSE){
	# Create dummy response and numeric response from factor
	Yd <- class.ind(Y)
	Y  <- as.numeric(Y)

	if(missing(Xnew))
		Xnew <- X

	# Dimensions
	n     <- dim(X)[1]
	p     <- dim(X)[2]
	g     <- dim(Yd)[2]
	n.new <- dim(Xnew)[1]

	if(missing(Prior))
		Prior <- apply(Yd,2,sum)/n

	Xs  <- solve(crossprod(Yd),t(Yd))%*%X # Mean for each group
	Xc  <- X-Xs[Y,,drop=FALSE]            # Centering
	# Inverse covariance matrices
	S   <- vapply(1:g, function(i) solve(crossprod(Xc[Y==i,])/(sum(Y==i)-1)), matrix(0,ncol=p,nrow=p))
	SXs       <- vapply(1:g, function(i) S[,,i]%*%Xs[i,], numeric(p))
	SXnew     <- vapply(1:g, function(i) tcrossprod(S[,,i],Xnew), matrix(0,nrow=p,ncol=n.new))
	XnewSXnew <- vapply(1:g, function(i) diag(Xnew%*%SXnew[,,i]), numeric(n.new))
	XnewSXs   <- vapply(1:g, function(i) Xnew%*%SXs[,i], numeric(n.new))
	dets <- sqrt(vapply(1:g, function(i) det(S[,,i]), 0))
	d    <- -0.5*XnewSXnew + matrix(-0.5*diag(Xs%*%SXs) + log(Prior) + log(dets), n.new, g, byrow=TRUE) + XnewSXs

	# Full QDA with posterior values
	if(posterior){
		d <- exp(d)
		d <- d / matrix(apply(d,1,sum),n.new,g)
	}

	group <- apply(d,1,which.max)  # Finds the most probable group
	if(posterior){
		return(list(group=group,posterior=d))
	} else {
		return(list(group=group))
	}
}

### Alternative help function from Solve's ST-PLS
class.ind <- function(x){
	x <- model.matrix(~x-1)
	mostattributes(x) <- list(dim=dim(x), dimnames=list(NULL, 1:dim(x)[2]))
	x
}
