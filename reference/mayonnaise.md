# NIR measurements and oil types of mayonnaise

Raw NIR measurements (351 wavelengths, 1100-2500 nm in steps of 4 nm)
taken on 54 samples of mayonnaise based on six different oil types
(soybean, sunflower, canola, olive, corn, and grapeseed). The resulting
54 samples were measured in triplicates, resulting in 54 x 3 = 162
different spectra (120/42 training/test).

## Format

A data frame with 162 observations on the following 4 variables.

- NIR:

  a matrix with 351 columns

- oil.type:

  a numeric vector

- design:

  a matrix with 5 columns

- train:

  a logical vector

## Source

Indahl U, Sahni NS, Kirkhus B, Næs T. Multivariate strategies for
classification based on NIR-spectra-with application to mayonnaise.
Chemometr. Intell. Lab. Sys. 1999; 49: 19-31.
