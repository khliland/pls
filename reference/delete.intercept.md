# Delete intercept from model matrix

A utility function to delete any intercept column from a model matrix,
and adjust the `"assign"` attribute correspondingly. It is used by
formula handling functions like `mvr` and `model.matrix.mvr`.

## Usage

``` r
delete.intercept(mm)
```

## Arguments

- mm:

  Model matrix.

## Value

A model matrix without intercept column.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`model.matrix.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md)

## Author

Bjørn-Helge Mevik and Ron Wehrens
