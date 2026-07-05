# The Bayes Factor Reversal (BFR) Paradox

**Same Data. Same Hypothesis. OPPOSITE Conclusions.**

For any statistically significant result under a point-null hypothesis, there
exists a prior scale — the "flip point" — at which the Bayes factor equals
exactly one. Below it, evidence favors H₁; above it, evidence favors H₀. The
direction of Bayesian evidence can therefore be reversed by prior choice alone.

Published in the *Brazilian Journal of Probability and Statistics*:
**"The Bayes Factor Reversal Paradox: A Unified Theory Across Statistical Tests"**
DOI: [10.1214/26-BJPS657](https://doi.org/10.1214/26-BJPS657)

## Overview

Two researchers analyze the same data using Bayes factors with "default"
priors. One concludes the evidence favors the alternative; the other concludes
it favors the null. Neither made an error — they simply chose different default
prior scales from the range offered by standard software.

This paper proves the BFR paradox is **universal** across **thirteen** standard
test families: one- and two-sample *z*/*t*-tests, paired samples, one-way
ANOVA, chi-squared, one- and two-proportion tests, correlation, simple and
multiple regression, and nonparametric tests (Mann–Whitney and Wilcoxon
signed-rank).

## Repository Contents

```
BFR-Paradox/
├── README.md                  # This file
├── LICENSE                    # MIT License
├── BFR_Unified_Code.R         # R functions: flip points for all 13 test families
└── Flip-Point-Calculator.html # Interactive browser-based flip-point calculator
```

## Quick Start

**Interactive calculator:** Open `Flip-Point-Calculator.html` in any modern
browser — no installation required. Computes Bayes factors and flip points for
all thirteen test families.

**R code:**

```r
source("BFR_Unified_Code.R")
flip_point_t(t_stat = 2.10, n1 = 30, n2 = 30)  # two-sample t-test flip point
```

## Citation

```bibtex
@article{lovric2026bfr,
  author  = {Lovric, Miodrag M.},
  title   = {The {B}ayes Factor Reversal Paradox: A Unified Theory Across Statistical Tests},
  journal = {Brazilian Journal of Probability and Statistics},
  year    = {2026},
  doi     = {10.1214/26-BJPS657}
}
```

## Author

Miodrag M. Lovric, Ph.D.
Professor of Statistics, Department of Mathematics and Statistics
Radford University, Virginia, USA
📧 mlovric@radford.edu

## Acknowledgements

Christian Robert, for his 2016 critique of Bayes factors that inspired this
investigation.

## License

MIT License — see the LICENSE file for details.
