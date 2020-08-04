%% $Id$
\name{NEWS}
\title{News for Package 'pls'}
\encoding{UTF-8}
%% \section{Changes in pls version FIXME}{
%%   \subsection{New features}{
%%     \itemize{
%%       \item
%%     }
%%   }
%%   \subsection{User-visible changes}{
%%     \itemize{
%%       \item
%%     }
%%   }
%%   \subsection{Bug fixes}{
%%     \itemize{
%%       \item
%%     }
%%   }
%%   \subsection{Internal changes}{
%%     \itemize{
%%       \item
%%     }
%%   }
%%   \subsection{Documentation}{
%%     \itemize{
%%       \item
%%     }
%%   }
%% }
\section{Changes in pls version 2.7-3}{
  \subsection{Internal changes}{
    \itemize{
      \item Small code cleanups and standardization of white space,
      indentation, etc.
      \item Fixes in test suite for MKL.
    }
  }
}
\section{Changes in pls version 2.7-2}{
  \subsection{Bug fixes}{
    \itemize{
      \item mvr() did not send the "center" argument to mvrCv(),
      resulting in wrong cross-validation results with 'center = FALSE'.
    }
  }
}
\section{Changes in pls version 2.7-1}{
  \subsection{Internal changes}{
    \itemize{
      \item Added temporary workaround in package tests for changes in RNG.
    }
  }
}
\section{Changes in pls version 2.7-0}{
  \subsection{New features}{
    \itemize{
      \item Now supports turning off the default centering of predictor
      and response matrices, through a new argument `center' to mvr()
      etal.  Suggested and implemented by Paul Hiemstra.
      \item Added argument `maxit' to oscorespls.fit(), to avoid a
      possible infinite loop for some near-orthoginal predictor
      matrices.  Suggested by Martin Binder.
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item Added a test suite.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Clarification of selectNcomp() documentation.
    }
  }
}
\section{Changes in pls version 2.6-0}{
  \subsection{New features}{
    \itemize{
      \item Added selectNcom() to automatically select the optimal
      number of components, optionally plotting the selection.
      \item Added argument "ask" to the plot functions that can generate
      multi-page plots, mimicking plot.lm().
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Added description of selectNcomp() in the vignette.
    }
  }
}
\section{Changes in pls version 2.5-0}{
  \subsection{New features}{
    \itemize{
      \item Cross-validation can now keep all replicates in the same
      segment, through a new argument `nrep' to cvsegments().
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item legend did not work with plot types "b" or "p" for
      validation plots.
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item Changes to comply with cran rules for imports, suggests and
      description
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Added a vignette.
      \item Added a NEWS.Rd file that can be viewed with the news() function.
    }
  }
}
\section{Changes in pls version 2.4-3}{
  \subsection{New features}{
    \itemize{
      \item Added a mechanism for running cross-validations in parallel
      using the facilities of the 'parallel' package.
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item In order to comply with current CRAN submission policies, 
        pls.options() no longer stores the modified option list in the global 
        environment.  This has the effect that the options will have to be set
        every time R is started, even if the work space was saved and loaded.
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item 'drop=FALSE' was missing a couple of places.
      \item Calling pls::plsr, pls::pcr or pls::cppls would fail if the package
        was not attached.
    }
  }
}
\section{Changes in pls version 2.3-0}{
  \subsection{New features}{
    \itemize{
      \item New analysis method CPPLS implmemented
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item The wide kernel PLS method would fail with 1 component
      \item predict() would fail if newdata was a matrix without coloumn names.
        Discovered by Egon Willighagen.
    }
  }
}
\section{Changes in pls version 2.2-0}{
  \subsection{New features}{
    \itemize{
      \item coefplot() can now plot whiskers at +/-1 SE.
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item Defaults for lty, pch and col in coefplot() when separate =
      TRUE has been changed.
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item The PRESS0 component returned by crossval() could be missing names
        (typically for uni-response models).
      \item scores.default and loadings.default would set the class of
      the scores or components even if they were a list.
      \item Small bugfixes in the call component of wrappers pcr() and plsr().
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item The package now has a name space.
      \item DESCRIPTION now has LazyLoad, LazyData and ZipData directives.
      \item predplotXy() now has arguments "labels" and "type".
      \item R2 is now generic.
    }
  }
}
\section{Changes in pls version 2.1-0}{
  \subsection{New features}{
    \itemize{
      \item Jackknife variance estimation of regression coefficients are
      now available.  (Note that the actual implementation is
      experimental, and could change in the future.)
      \item The `wide kernel' PLS algorithm has been implemented.  It is
      faster than the other algorithms for very wide data.
      \item The plot functions for coefficients, predictions and
      validation results (R2, (R)MSEP) have gained an argument `main' to
      set the main title of the plot.
      \item mvr() and mvrCv() now check for near zero standard deviation
      when autoscaling (`scale = TRUE')
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item plots that go over several pages now only set `par(ask =
      TRUE)' if the plot device is interactive (suggested by Kevin Wright).
      \item The definition of R^2 has been changed to  1 - SSE/SST for
      all estimators, so R2() will give different results than before
      for test sets and cross-validation.
      \item The handling of NAs in prediction, R2, (R)MSEP and
      cross-validation has been improved (and several bugs fixed).
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item coefplot(), predplot.mvr() and validationplot():
      xlab/ylab/main were added too early, so they disappeared on the
      last plot when nCols == nRows == 1.
      \item loadingplot.default() didn't check for NULL rownames().
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item The (R)MSEP/R^2 calculations have been reorganised.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item A couple of functions have been given the keyword
      "internal", and so are hidden from the index.
    }
  }
}
\section{Changes in pls version 2.0-1}{
  \subsection{Bug fixes}{
    \itemize{
      \item Fixed 1-component bugs in oscorespls.fit() and svdpc.fit()
      \item Fix to recognise negative numbers as numeric variable names in
        loadingplot and coefplot
      \item Removed non-ASCII characters from comments in R files (these gave
        warnings/errors when checking on Macosx)
    }
  }
}
\section{Changes in pls version 2.0-0}{
  \subsection{New features}{
    \itemize{
      \item There is now an options mechanism for selecting default fit
      algorithms.  See ?pls.options.
      \item loadingplot() and coefplot() now try to be more intelligent
      when plotting x axis labels.
      \item The handling of factors in X has been improved, by changing
      the way the intercept is removed from the model matrix.
      \item A new data set 'gasoline' has been included.
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item All PLSR and PCR algorithms, as well as mvrCv(), have been
      optimised.  Depending on the algorithm used, the size of the
      matrices, and the number of components used, one can expect from
      5\% to 65\% reduction in computation time.
      \item Scaling of scores and loadings of kernel PLSR and svd PCR
      algorithm has changed.  They are now scaled using the `classic'
      scaling found in oscorespls.
      \item The arguments `ncomp' now always means
      "number of components", and `comps' always means
      "component number".  The argument `cumulative' has been removed.
      \item cvsegments() now gives an error if k > n.
      \item The 'NIR' and 'sensory' data sets have been renamed to
      'yarn' and 'oliveoil'.
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item Fixed bugs in MSEP.mvr and R2 that gave 'NA's and errors
      with 'na.exclude'.
      \item Numerous small fixes in the documentation.
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item New utility function delete.intercept().
      \item Standardised variable names in several fit functions.
    }
  }
}
\section{Changes in pls version 1.2-1}{
  \subsection{New features}{
    \itemize{
      \item It is now possible to select the radii of the circles in corrplot
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item Fixed bug in kernelpls.fit() that resulted in incorrect
      results when fitting mulitresponse models with fewer responses
      than predictors
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item The default convergence tolerance limit (when fitting
      multiresponse models) in oscorespls.fit() has been changed
      \item Changed default radii in corrplot()
      \item kernelpls.fit() now returns loading.weights
      \item crossval() now checks whether the model was fit with a
      `data' argument
    }
  }
}
\section{Changes in pls version 1.2-0}{
  \subsection{New features}{
    \itemize{
      \item Line plot parameter arguments have been added to
      predplotXy(), so one can control the properties of the target line
      in predplot().
      \item The `mvr' method for predict() now handles missing data like
      the `lm' method does (the default is to predict `NA').
      \item fitted() and residuals() now return NA for observations with
      missing values, if na.action is na.exclude.
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item `ncomp' is now reduced when it is too large for the requested 
        cross-validation.
      \item The (optional) plot parametres `cex' and `font' were
      incorrectly matched with `cex.main' and `font.main' in predplot().
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item The `mvr' method for predict() now checks the number of coloumns of
        `newdata' when it is a matrix.
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item MSEP() and RMSEP() are now generic, with methods for `mvr' objects.
      \item loadingplot() is now generic, with a default method that
      handles matrices and objects whose loadings() method return a matrix.
      \item A new function loadings().  It is generic, with a default
      method that handles `prcomp' objects and anything with a
      `loadings' component.
      \item scoreplot() is now generic, with a default method that
      handles matrices and objects whose scores() method return a matrix.
      \item The `prcomp' method for scores() has been removed.  The
      default method now handles `prcomp' objects as an exception.
      scores() now also attaches the explained variances (if available)
      as the attribute `explvar'.
      \item compnames() now handles matrices as well as objects with
      score matrices
      \item explvar() now also hanles score and loadings matrices
      \item mvr() now returns a `na.action' component.
      \item new internal functions fitted.mvr(), residuals.mvr() and
      naExcludeMvr() to handle observations with missing values.
    }
  }
}
\section{Changes in pls version 1.1-0}{
  \subsection{New features}{
    \itemize{
      \item mvr, mvrCv and predict.mvr now has support for scaling of X.
      \item A new function stdize (with predict and makepredictcall methods) for
        explicit centering and/or scaling.
      \item Correlation loadings plot (corrplot).
      \item New argument `varnames' in coefplot, to label the x tick
      marks with the variable names. 
      \item loadingplot now uses the `labels' argument to label x tick
      marks when 'scatter = FALSE'.
      \item loadingplot, coefplot and plot.mvrVal can now display
      legends, with the argument 'legendpos'.
    }
  }
  \subsection{User-visible changes}{
    \itemize{
      \item mvr and model.matrix.mvr now removes the prepended term name
      from colnames of X when there is only one term and it has colnames.
      \item The plot functions now avoid setting defaults for graphical
      parametres explicitly, wherever possible (e.g. lwd = NULL instead
      of lwd = 1).  That way, par() settings are respected.
      \item Most "handled" graphical parametres have been added to the
      formal argument list (for most functions: xlab and ylab).
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item New functions compnames, prednames and respnames, used in
      plot and summary functions.
      \item Some minor internal changes in the plot and summary functions.
      \item Allows `method = "model.frame"' in mvr, plsr and pcr.
      \item New method model.frame.mvr.
    }
  }
  \subsection{Documentation}{
    \itemize{
      \item Added missing output component `Yloadings' to oscorepls.fit.Rd
      \item Added/fixed a couple of references.
    }
  }
}
\section{Changes in pls version 1.0-3}{
  \itemize{
    \item mvrCv.R: Bugfix; added ^2 to R2 calculation
    \item predict.mvr.R: Fix handling of `newdata' such that checks of
    the dimensions of the variables found, are performed.  Also allow a
    matrix (without any checks).  In addition, a small fix for matrices
    without row names.
    \item loop revision in mvrCv.R
    \item Store data sets in binary form
    \item mvr.Rd: Added more explicit information about the formula argument.
    \item Numerous small clean-ups in the .Rd files
  }
}
\section{Changes in pls version 1.0-2}{
  \itemize{
    \item Added encoding information to DESCRIPTION and *.Rd
  }
}
