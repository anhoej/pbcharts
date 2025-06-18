2025-06-18

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Process Behaviour Charts

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An R package for run charts and individuals control charts for
statistical quality control and improvement.

pbcharts implements the I prime (I’ or normalised I) control chart
suggested by Taylor
<https://variation.com/normalized-individuals-control-chart/>.

The I’ chart adjusts control limits to varying subgroup sizes making
pbcharts useful for a wide range of measurement and count data and a
convenient replacement for the classic Shewhart control charts.

pbcharts uses only functions from base R making in fast and robust.

pbcharts is currently able to:

- test for special cause variation using runs analysis and control
  limits;
- signal special causes using clear visual clues;
- freeze calculations of centre line and control limits to a baseline
  period;
- split charts into separate periods;
- exclude individual data points from calculations;
- facet plots (small multiples) on one categorical variable.

pbcharts is in early develpment. Please report any issues at
<https://github.com/anhoej/pbcharts/issues>

## Installation

You can install the development version of pbcharts from
[GitHub](https://github.com/anhoej/pbcharts) with:

    devtools::install_github("anhoej/pbcharts")

## Examples

Draw a run chart of 24 random normal values:

``` r
library(pbcharts)
pbc(rnorm(24))
```

<img src="man/figures/README-unnamed-chunk-2-1.svg" width="100%" />

Draw an individuals (I) control chart:

``` r
pbc(rnorm(24), chart = 'i')
```

<img src="man/figures/README-unnamed-chunk-3-1.svg" width="100%" />

Signal special causes from data points outside control limits (red
points) and(or) unusually long or few runs (red and dashed centre line):

``` r
pbc(1:11, chart = 'i')
```

<img src="man/figures/README-unnamed-chunk-4-1.svg" width="100%" />

Standard I chart (subgroup size = 1) of average decision to delivery
times for grade 2 caesarian sections (C-section):

``` r
pbc(month, avg_delay, 
    data  = csection, 
    chart = 'i')
```

<img src="man/figures/README-unnamed-chunk-5-1.svg" width="100%" />

I’ chart of C-section data taking varying subgroup sizes into account:

``` r
pbc(month, avg_delay * n, n,  # multiply numerator and denominator to keep scale
    data = csection,
    chart = 'i')
```

<img src="man/figures/README-unnamed-chunk-6-1.svg" width="100%" />

Standard I chart of average HbA1c in children with diabetes:

``` r
pbc(month, avg_hba1c,
    data  = hba1c,
    chart = 'i',
    title = 'I chart of average HbA1c in children with diabetes',
    ylab  = 'mmol/mol',
    xlab  = 'Month')
```

<img src="man/figures/README-unnamed-chunk-7-1.svg" width="100%" />

I’ chart of average HbA1c in children with diabetes using subgroups
(number of children):

``` r
pbc(month, avg_hba1c * n, n,
    data  = hba1c,
    chart = 'i',
    title = "I' chart of average HbA1c in children with diabetes",
    ylab  = 'mmol/mol',
    xlab  = 'Month')
```

<img src="man/figures/README-unnamed-chunk-8-1.svg" width="100%" />

Structure and summary of a pbc object:

``` r
# save pbc object while suppressing plotting
p <- pbc(month, avg_hba1c * n, n,
         data  = hba1c,
         chart = 'i',
         title = 'I\' chart of average HbA1c in children with diabetes',
         ylab  = 'mmol/mol',
         xlab  = 'Month',
         plot = FALSE)

# print structure
str(p)
#> List of 10
#>  $ title  : chr "I' chart of average HbA1c in children with diabetes"
#>  $ xlab   : chr "Month"
#>  $ ylab   : chr "mmol/mol"
#>  $ ncol   : NULL
#>  $ yfixed : logi TRUE
#>  $ freeze : NULL
#>  $ split  : NULL
#>  $ exclude: NULL
#>  $ chart  : chr "i"
#>  $ data   :'data.frame': 43 obs. of  20 variables:
#>   ..$ facet          : num [1:43] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ part           : int [1:43] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ x              : Date[1:43], format: "2019-03-01" "2019-04-01" ...
#>   ..$ num            : num [1:43] 12695 12229 12310 14381 5988 ...
#>   ..$ den            : num [1:43] 214 203 212 238 96 248 234 168 194 226 ...
#>   ..$ y              : num [1:43] 59.3 60.2 58.1 60.4 62.4 ...
#>   ..$ longest.run    : int [1:43] 6 6 6 6 6 6 6 6 6 6 ...
#>   ..$ longest.run.max: num [1:43] 8 8 8 8 8 8 8 8 8 8 ...
#>   ..$ n.crossings    : num [1:43] 18 18 18 18 18 18 18 18 18 18 ...
#>   ..$ n.crossings.min: num [1:43] 16 16 16 16 16 16 16 16 16 16 ...
#>   ..$ lcl            : num [1:43] 56.1 56 56 56.3 54 ...
#>   ..$ cl             : num [1:43] 60.3 60.3 60.3 60.3 60.3 ...
#>   ..$ ucl            : num [1:43] 64.6 64.7 64.6 64.3 66.6 ...
#>   ..$ runs.signal    : logi [1:43] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ sigma.signal   : logi [1:43] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ freeze         : logi [1:43] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ include        : logi [1:43] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ base           : logi [1:43] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ n.obs          : int [1:43] 43 43 43 43 43 43 43 43 43 43 ...
#>   ..$ n.useful       : int [1:43] 43 43 43 43 43 43 43 43 43 43 ...
#>  - attr(*, "class")= chr [1:2] "pbc" "list"

# print summary
summary(p)
#>   facet part n.obs n.useful  avg_lcl       cl  avg_ucl sigma.signal runs.signal
#> 1     1    1    43       43 55.47002 60.31032 65.15062            0           0
#>   longest.run longest.run.max n.crossings n.crossings.min
#> 1           6               8          18              16

# plot chart
plot(p)
```

<img src="man/figures/README-unnamed-chunk-9-1.svg" width="100%" />

Hospital infection rates:

``` r
pbc(month, n, days,
    data     = cdi,
    multiply = 10000,
    title    = 'Hospital associated C. diff. infections',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')
```

<img src="man/figures/README-unnamed-chunk-10-1.svg" width="100%" />

Freeze calculation of centre and control lines to period before
intervention:

``` r
pbc(month, n, days,
    data     = cdi,
    chart    = 'i',
    multiply = 10000,
    freeze   = 24,
    title    = 'Hospital associated C. diff. infections',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')
```

<img src="man/figures/README-unnamed-chunk-11-1.svg" width="100%" />

Split chart after intervention:

``` r
pbc(month, n, days,
    data     = cdi,
    chart    = 'i',
    multiply = 10000,
    split    = 24,
    title    = 'Hospital associated C. diff. infections',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')
```

<img src="man/figures/README-unnamed-chunk-12-1.svg" width="100%" />

Ignore freak data point \#20:

``` r
pbc(month, n, days,
    data     = cdi,
    chart    = 'i',
    multiply = 10000,
    split    = 24,
    exclude  = 20,
    title    = 'Hospital associated C. diff. infections',
    ylab     = 'Count per 10,000 risk days',
    xlab     = 'Month')
```

<img src="man/figures/README-unnamed-chunk-13-1.svg" width="100%" />

Faceted I’ chart of bacteremia mortality in six hospitals:

``` r
pbc(month, deaths, cases,
    facet    = hospital,                # facet plot by hospital
    data     = bacteremia_mortality,
    chart    = 'i',
    multiply = 100,                     # show percent rather than proportions
    ylim     = c(0, NA),                # suppress negative control limits
    title    = 'Bacteremia mortality',
    ylab     = '%',
    xlab     = 'Month')
```

<img src="man/figures/README-unnamed-chunk-14-1.svg" width="100%" />

## Procedure for calculating centre line and conrol limits

We use the following symbols:

- n = numerators
- d = denominators
- o = number of data values
- i = i<sup>th</sup> data value

Values to plot:

$$
y = \frac{n}{d}
$$

Centre line:

$$
CL = \frac{\sum{n}}{\sum{d}}
$$

Standard deviation of i<sup>th</sup> data point:

$$
s_i = \sqrt{\frac{\pi}{2}}\frac{\vert{}y_i-y_{i-1}\vert{}}{\sqrt{\frac{1}{d_i}+\frac{1}{d_{i-1}}}}
$$

Average standard deviation:

$$
\bar{s} = \frac{\sum{s}}{o}
$$

Control limits:

$$
\text{control limits} = CL \pm 3 \frac{\bar{s}}{\sqrt{d_i}}
$$

## Tests for special cause variation

pbc applies three tests to detect special cause variation:

- **Data points outside the control limits**<br> Any point falling
  outside the control limits indicates potential special cause
  variation.

- **Unusually long runs**<br> A run consists of one or more consecutive
  data points on the same side of the centre line. Data points that fall
  exactly on the centre line neither contribute to nor interrupt a run.
  For a random process, the upper 95% prediction limit for the longest
  run is approximately `log₂(n) + 3`, rounded to the nearest whole
  number, where n is the number of useful data points (i.e. those not on
  the centre line).

- **Unusually few crossings**\<br\< A crossing occurs when two
  consecutive data points fall on opposite sides of the centre line. In
  a random process, the number of crossings follows a binomial
  distribution. The lower 5% prediction limit can be found using the
  cumulative distribution function:
  `qbinom(p = 0.05, size = n - 1, prob = 0.5)`.

Data points outside the control limits are highlighted, and the centre
line is dashed and coloured if either of the two runs tests is positive.

In run charts, the centre line represents the median; in control charts,
it represents the (weighted) mean.

Critical values for runs and crossing:

| Number of useful observations | Upper limit for longest run | Lower limit for number of crossings |
|---:|---:|---:|
| 10 | 6 | 2 |
| 11 | 6 | 2 |
| 12 | 7 | 3 |
| 13 | 7 | 3 |
| 14 | 7 | 4 |
| 15 | 7 | 4 |
| 16 | 7 | 4 |
| 17 | 7 | 5 |
| 18 | 7 | 5 |
| 19 | 7 | 6 |
| 20 | 7 | 6 |
| 21 | 7 | 6 |
| 22 | 7 | 7 |
| 23 | 8 | 7 |
| 24 | 8 | 8 |
| 25 | 8 | 8 |
| 26 | 8 | 8 |
| 27 | 8 | 9 |
| 28 | 8 | 9 |
| 29 | 8 | 10 |
| 30 | 8 | 10 |
| 31 | 8 | 11 |
| 32 | 8 | 11 |
| 33 | 8 | 11 |
| 34 | 8 | 12 |
| 35 | 8 | 12 |
| 36 | 8 | 13 |
| 37 | 8 | 13 |
| 38 | 8 | 14 |
| 39 | 8 | 14 |
| 40 | 8 | 14 |
| 41 | 8 | 15 |
| 42 | 8 | 15 |
| 43 | 8 | 16 |
| 44 | 8 | 16 |
| 45 | 8 | 17 |
| 46 | 9 | 17 |
| 47 | 9 | 17 |
| 48 | 9 | 18 |
| 49 | 9 | 18 |
| 50 | 9 | 19 |
| 51 | 9 | 19 |
| 52 | 9 | 20 |
| 53 | 9 | 20 |
| 54 | 9 | 21 |
| 55 | 9 | 21 |
| 56 | 9 | 21 |
| 57 | 9 | 22 |
| 58 | 9 | 22 |
| 59 | 9 | 23 |
| 60 | 9 | 23 |
| 61 | 9 | 24 |
| 62 | 9 | 24 |
| 63 | 9 | 25 |
| 64 | 9 | 25 |
| 65 | 9 | 25 |
| 66 | 9 | 26 |
| 67 | 9 | 26 |
| 68 | 9 | 27 |
| 69 | 9 | 27 |
| 70 | 9 | 28 |
| 71 | 9 | 28 |
| 72 | 9 | 29 |
| 73 | 9 | 29 |
| 74 | 9 | 29 |
| 75 | 9 | 30 |
| 76 | 9 | 30 |
| 77 | 9 | 31 |
| 78 | 9 | 31 |
| 79 | 9 | 32 |
| 80 | 9 | 32 |
| 81 | 9 | 33 |
| 82 | 9 | 33 |
| 83 | 9 | 34 |
| 84 | 9 | 34 |
| 85 | 9 | 34 |
| 86 | 9 | 35 |
| 87 | 9 | 35 |
| 88 | 9 | 36 |
| 89 | 9 | 36 |
| 90 | 9 | 37 |
| 91 | 10 | 37 |
| 92 | 10 | 38 |
| 93 | 10 | 38 |
| 94 | 10 | 39 |
| 95 | 10 | 39 |
| 96 | 10 | 39 |
| 97 | 10 | 40 |
| 98 | 10 | 40 |
| 99 | 10 | 41 |
| 100 | 10 | 41 |
