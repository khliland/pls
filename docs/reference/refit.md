# Refit a PLSR/PCR model after selecting observations

Refit an existing `mvr` model after selecting observations from the
training set. The selection can be interpreted either negatively
(exclude selected observations) or positively (keep only selected
observations).

## Usage

``` r
refit(object, ...)

# S3 method for class 'mvr'
refit(object, selection = NULL, exclude = TRUE, ...)
```

## Arguments

- object:

  an object to refit.

- ...:

  additional arguments passed to methods.

- selection:

  character or numeric vector identifying selected observations. Values
  are matched against row names in the model frame used for fitting.

- exclude:

  logical. If `TRUE` (default), selected observations are removed before
  refitting. If `FALSE`, only selected observations are kept.

## Value

A refitted model object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
ids <- rownames(model.frame(mod))[1:5]
mod_excluded <- refit(mod, selection = ids, exclude = TRUE)
mod_included <- refit(mod, selection = ids, exclude = FALSE)
#> Warning: `ncomp` reduced to 4 for refit data.
```
