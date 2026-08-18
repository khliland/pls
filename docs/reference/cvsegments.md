# Generate segments for cross-validation

The function generates a list of segments for cross-validation. It can
generate random, consecutive and interleaved segments, and supports
keeping replicates in the same segment.

## Usage

``` r
cvsegments(
  N,
  k,
  length.seg = ceiling(N/k),
  nrep = 1,
  type = c("random", "consecutive", "interleaved"),
  stratify = NULL
)
```

## Arguments

- N:

  Integer. The number of rows in the data set.

- k:

  Integer. The number of segments to return.

- length.seg:

  Integer. The length of the segments. If given, it overrides `k`.

- nrep:

  Integer. The number of (consecutive) rows that are replicates of the
  same object. Replicates will always be kept in the same segment.

- type:

  One of `"random"`, `"consecutive"` and `"interleaved"`. The type of
  segments to generate. Default is `"random"`.

- stratify:

  Either a `list` of indices or an integer `vector` indicating which
  stratum each sample (or set of replicates) belongs to (see Details).

## Value

A list of vectors. Each vector contains the indices for one segment. The
attribute `"incomplete"` contains the number of incomplete segments, and
the attribute `"type"` contains the type of segments.

## Details

If `length.seg` is specified, it is used to calculate the number of
segments to generate. Otherwise `k` must be specified. If
\\k\*length.seg \ne N\\, the \\k\*length.seg - N\\ last segments will
contain only \\length.seg - 1\\ indices.

If `type` is `"random"`, the indices are allocated to segments in random
order. If it is `"consecutive"`, the first segment will contain the
first \\length.seg\\ indices, and so on. If `type` is `"interleaved"`,
the first segment will contain the indices \\1, length.seg+1,
2\*lenght.seg+1, \ldots, (k-1)\*length.seg+1\\, and so on.

If \\nrep \> \\, it is assumed that each `nrep` consecutive rows are
replicates (repeated measurements) of the same object, and care is taken
that replicates are never put in different segments.

Warning: If `k` does not divide `N`, a specified `length.seg` does not
divide `N`, or `nrep` does not divide `length.seg`, the number of
segments and/or the segment length will be adjusted as needed. Warnings
are printed for some of these cases, and one should always inspect the
resulting segments to make sure they are as expected.

Stratification of samples is enabled by the `stratify` argument. This is
useful if there are sub-groups in the data set that should have a
proportional representation in the cross-validation segments or if the
response is categorical (classifiation). If `stratify` is combined with
`nrep`, `stratify` corresponds to the sets of replicates (see example).

## See also

Converting a factor to segments:
[`fac2seg`](https://khliland.github.io/pls/reference/fac2seg.md).

## Author

Bjørn-Helge Mevik, Ron Wehrens and Kristian Hovde Liland

## Examples

``` r

## Segments for 10-fold randomised cross-validation:
cvsegments(100, 10)
#> $V1
#>  [1] 61 19 13 39 89 56 37  6  3  5
#> 
#> $V2
#>  [1] 87 82 46 23 70 10 92 26 29 63
#> 
#> $V3
#>  [1] 64 27 66  7 36 97 16 38 60 53
#> 
#> $V4
#>  [1] 43 20 84 15 93  2 12  4 67 86
#> 
#> $V5
#>  [1] 33 69 71 94 11 25 77 45 98 32
#> 
#> $V6
#>  [1] 18 51 55 35 76  9 88 42 17  8
#> 
#> $V7
#>  [1] 14 96 44 34 48 50 78 68 80 54
#> 
#> $V8
#>  [1] 30  1 90 22 59 62 40 31 47 57
#> 
#> $V9
#>  [1]  41  52  28  83  49  73  24  58  21 100
#> 
#> $V10
#>  [1] 72 65 79 74 75 81 91 95 85 99
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "random"

## Segments with four objects, taken consecutive:
(segs <- cvsegments(60, length.seg = 4, type = "cons"))
#> $V1
#> [1] 1 2 3 4
#> 
#> $V2
#> [1] 5 6 7 8
#> 
#> $V3
#> [1]  9 10 11 12
#> 
#> $V4
#> [1] 13 14 15 16
#> 
#> $V5
#> [1] 17 18 19 20
#> 
#> $V6
#> [1] 21 22 23 24
#> 
#> $V7
#> [1] 25 26 27 28
#> 
#> $V8
#> [1] 29 30 31 32
#> 
#> $V9
#> [1] 33 34 35 36
#> 
#> $V10
#> [1] 37 38 39 40
#> 
#> $V11
#> [1] 41 42 43 44
#> 
#> $V12
#> [1] 45 46 47 48
#> 
#> $V13
#> [1] 49 50 51 52
#> 
#> $V14
#> [1] 53 54 55 56
#> 
#> $V15
#> [1] 57 58 59 60
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "consecutive"
data(gasoline)
plsr(octane ~ NIR, data=gasoline, ncomp=5, validation="CV", segments=segs)
#> Partial least squares regression, fitted with the kernel algorithm.
#> Cross-validated using 15 consecutive segments.
#> Call:
#> pls::plsr(formula = octane ~ NIR, ncomp = 5, data = gasoline,     validation = "CV", segments = segs)

## Incomplete segments
segs <- cvsegments(50, length.seg = 3)
#> Warning: Required segment length does not divide the number of observations.
#>   A best effort segment size will be used.
attr(segs, "incomplete")
#> [1] 1

## Leave-one-out cross-validation:
cvsegments(100, 100)
#> $V1
#> [1] 90
#> 
#> $V2
#> [1] 38
#> 
#> $V3
#> [1] 30
#> 
#> $V4
#> [1] 56
#> 
#> $V5
#> [1] 28
#> 
#> $V6
#> [1] 73
#> 
#> $V7
#> [1] 81
#> 
#> $V8
#> [1] 1
#> 
#> $V9
#> [1] 64
#> 
#> $V10
#> [1] 89
#> 
#> $V11
#> [1] 29
#> 
#> $V12
#> [1] 13
#> 
#> $V13
#> [1] 95
#> 
#> $V14
#> [1] 49
#> 
#> $V15
#> [1] 42
#> 
#> $V16
#> [1] 99
#> 
#> $V17
#> [1] 88
#> 
#> $V18
#> [1] 11
#> 
#> $V19
#> [1] 60
#> 
#> $V20
#> [1] 68
#> 
#> $V21
#> [1] 61
#> 
#> $V22
#> [1] 12
#> 
#> $V23
#> [1] 23
#> 
#> $V24
#> [1] 71
#> 
#> $V25
#> [1] 83
#> 
#> $V26
#> [1] 69
#> 
#> $V27
#> [1] 78
#> 
#> $V28
#> [1] 80
#> 
#> $V29
#> [1] 75
#> 
#> $V30
#> [1] 72
#> 
#> $V31
#> [1] 17
#> 
#> $V32
#> [1] 26
#> 
#> $V33
#> [1] 50
#> 
#> $V34
#> [1] 4
#> 
#> $V35
#> [1] 7
#> 
#> $V36
#> [1] 53
#> 
#> $V37
#> [1] 18
#> 
#> $V38
#> [1] 74
#> 
#> $V39
#> [1] 63
#> 
#> $V40
#> [1] 77
#> 
#> $V41
#> [1] 8
#> 
#> $V42
#> [1] 33
#> 
#> $V43
#> [1] 48
#> 
#> $V44
#> [1] 2
#> 
#> $V45
#> [1] 67
#> 
#> $V46
#> [1] 59
#> 
#> $V47
#> [1] 6
#> 
#> $V48
#> [1] 27
#> 
#> $V49
#> [1] 98
#> 
#> $V50
#> [1] 46
#> 
#> $V51
#> [1] 22
#> 
#> $V52
#> [1] 92
#> 
#> $V53
#> [1] 94
#> 
#> $V54
#> [1] 76
#> 
#> $V55
#> [1] 70
#> 
#> $V56
#> [1] 3
#> 
#> $V57
#> [1] 57
#> 
#> $V58
#> [1] 31
#> 
#> $V59
#> [1] 96
#> 
#> $V60
#> [1] 15
#> 
#> $V61
#> [1] 55
#> 
#> $V62
#> [1] 35
#> 
#> $V63
#> [1] 91
#> 
#> $V64
#> [1] 45
#> 
#> $V65
#> [1] 41
#> 
#> $V66
#> [1] 25
#> 
#> $V67
#> [1] 84
#> 
#> $V68
#> [1] 10
#> 
#> $V69
#> [1] 39
#> 
#> $V70
#> [1] 21
#> 
#> $V71
#> [1] 100
#> 
#> $V72
#> [1] 97
#> 
#> $V73
#> [1] 93
#> 
#> $V74
#> [1] 40
#> 
#> $V75
#> [1] 32
#> 
#> $V76
#> [1] 14
#> 
#> $V77
#> [1] 52
#> 
#> $V78
#> [1] 9
#> 
#> $V79
#> [1] 5
#> 
#> $V80
#> [1] 62
#> 
#> $V81
#> [1] 82
#> 
#> $V82
#> [1] 47
#> 
#> $V83
#> [1] 16
#> 
#> $V84
#> [1] 86
#> 
#> $V85
#> [1] 20
#> 
#> $V86
#> [1] 87
#> 
#> $V87
#> [1] 44
#> 
#> $V88
#> [1] 51
#> 
#> $V89
#> [1] 43
#> 
#> $V90
#> [1] 79
#> 
#> $V91
#> [1] 37
#> 
#> $V92
#> [1] 65
#> 
#> $V93
#> [1] 54
#> 
#> $V94
#> [1] 85
#> 
#> $V95
#> [1] 24
#> 
#> $V96
#> [1] 34
#> 
#> $V97
#> [1] 66
#> 
#> $V98
#> [1] 36
#> 
#> $V99
#> [1] 19
#> 
#> $V100
#> [1] 58
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"
## Leave-one-out with variable/unknown data set size n:
n <- 50
cvsegments(n, length.seg = 1)
#> $V1
#> [1] 10
#> 
#> $V2
#> [1] 36
#> 
#> $V3
#> [1] 24
#> 
#> $V4
#> [1] 23
#> 
#> $V5
#> [1] 15
#> 
#> $V6
#> [1] 47
#> 
#> $V7
#> [1] 3
#> 
#> $V8
#> [1] 40
#> 
#> $V9
#> [1] 12
#> 
#> $V10
#> [1] 19
#> 
#> $V11
#> [1] 42
#> 
#> $V12
#> [1] 22
#> 
#> $V13
#> [1] 6
#> 
#> $V14
#> [1] 14
#> 
#> $V15
#> [1] 41
#> 
#> $V16
#> [1] 31
#> 
#> $V17
#> [1] 4
#> 
#> $V18
#> [1] 48
#> 
#> $V19
#> [1] 46
#> 
#> $V20
#> [1] 43
#> 
#> $V21
#> [1] 49
#> 
#> $V22
#> [1] 7
#> 
#> $V23
#> [1] 29
#> 
#> $V24
#> [1] 8
#> 
#> $V25
#> [1] 35
#> 
#> $V26
#> [1] 26
#> 
#> $V27
#> [1] 27
#> 
#> $V28
#> [1] 34
#> 
#> $V29
#> [1] 38
#> 
#> $V30
#> [1] 18
#> 
#> $V31
#> [1] 39
#> 
#> $V32
#> [1] 11
#> 
#> $V33
#> [1] 25
#> 
#> $V34
#> [1] 32
#> 
#> $V35
#> [1] 1
#> 
#> $V36
#> [1] 17
#> 
#> $V37
#> [1] 2
#> 
#> $V38
#> [1] 16
#> 
#> $V39
#> [1] 44
#> 
#> $V40
#> [1] 20
#> 
#> $V41
#> [1] 9
#> 
#> $V42
#> [1] 37
#> 
#> $V43
#> [1] 30
#> 
#> $V44
#> [1] 5
#> 
#> $V45
#> [1] 21
#> 
#> $V46
#> [1] 50
#> 
#> $V47
#> [1] 33
#> 
#> $V48
#> [1] 13
#> 
#> $V49
#> [1] 45
#> 
#> $V50
#> [1] 28
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"

## Data set with replicates
cvsegments(100, 25, nrep = 2)
#> $V1
#> [1] 95 96 55 56
#> 
#> $V2
#> [1] 57 58 79 80
#> 
#> $V3
#> [1] 83 84 69 70
#> 
#> $V4
#> [1] 39 40 73 74
#> 
#> $V5
#> [1] 27 28 63 64
#> 
#> $V6
#> [1] 45 46 91 92
#> 
#> $V7
#> [1]  53  54  99 100
#> 
#> $V8
#> [1] 75 76 61 62
#> 
#> $V9
#> [1] 29 30 49 50
#> 
#> $V10
#> [1] 65 66 59 60
#> 
#> $V11
#> [1] 85 86  3  4
#> 
#> $V12
#> [1] 11 12 13 14
#> 
#> $V13
#> [1] 21 22  9 10
#> 
#> $V14
#> [1] 23 24 43 44
#> 
#> $V15
#> [1] 77 78 33 34
#> 
#> $V16
#> [1] 47 48 67 68
#> 
#> $V17
#> [1] 87 88 35 36
#> 
#> $V18
#> [1] 89 90 25 26
#> 
#> $V19
#> [1] 15 16 19 20
#> 
#> $V20
#> [1] 81 82  1  2
#> 
#> $V21
#> [1] 41 42 97 98
#> 
#> $V22
#> [1] 71 72 51 52
#> 
#> $V23
#> [1] 7 8 5 6
#> 
#> $V24
#> [1] 93 94 31 32
#> 
#> $V25
#> [1] 17 18 37 38
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "random"
## Note that rows 1 and 2 are in the same segment, rows 3 and 4 in the
## same segment, and so on.

## Stratification
cvsegments(10, 3, type = "consecutive", stratify = c(rep(1,7), rep(2,3)))
#> $V1
#> [1] 1 2 3 8
#> 
#> $V2
#> [1] 4 5 9
#> 
#> $V3
#> [1]  6  7 10
#> 
#> attr(,"incomplete")
#> [1] 2
#> attr(,"type")
#> [1] "consecutive"
## Note that the last three samples are spread across the segments
## according to the stratification vector.
cvsegments(20, 3, type = "consecutive", nrep = 2, stratify = c(rep(1,7), rep(2,3)))
#> Warning: Segment length is not a multiple of the number of replicates.
#>   A best effort segment size will be used.
#> $V1
#> [1]  1  2  3  4  5  6 15 16
#> 
#> $V2
#> [1]  7  8  9 10 17 18
#> 
#> $V3
#> [1] 11 12 13 14 19 20
#> 
#> attr(,"incomplete")
#> [1] 2
#> attr(,"type")
#> [1] "consecutive"
## Note the length of stratify matching number of replicate sets, not samples.

## Converting a factor to segments
fac <- factor(c("a", "b", "a", "b", "c", "c"))
fac2seg(fac)
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 2 4
#> 
#> [[3]]
#> [1] 5 6
#> 
## Starting from a numeric vector
num <- c(1, 2, 1, 2, 3, 3)
fac2seg(factor(num))
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 2 4
#> 
#> [[3]]
#> [1] 5 6
#> 
```
