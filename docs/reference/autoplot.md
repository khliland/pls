# Autoplot for mvr Models

`autoplot` is a convenience wrapper that calls
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
This makes `autoplot(mod, ...)` available directly from pls when ggplot2
is installed.

## Usage

``` r
autoplot(object, ...)
```

## Arguments

- object:

  an object to autoplot.

- ...:

  further arguments passed to
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Value

A plot object returned by
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  autoplot(mod, type = "scores")
}
```
