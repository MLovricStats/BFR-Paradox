# The Bayes Factor Reversal (BFR) Paradox

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D%203.6-blue.svg)](https://www.r-project.org/)
[![Biometrika](https://img.shields.io/badge/Submitted-Biometrika%202025-red.svg)](https://academic.oup.com/biomet)

**Same Data. Same Hypothesis. OPPOSITE Conclusions.**

A newly discovered statistical paradox revealing that two Bayesians using different prior scales can reach opposite conclusions from the same statistically significant data.

---

## Overview

In 1957, Dennis Lindley published "A Statistical Paradox" in *Biometrika*, revealing a conflict between frequentist and Bayesian inference as sample size approaches infinity.

**The BFR Paradox is arguably more fundamental:** it reveals a conflict of *Bayesian inference with itself*.

### The Key Result

For **any statistically significant result** at the 0.05 level, there exist reasonable prior scales τ₁ and τ₂ such that:

- **BF₀₁(τ₁) < 1** → Evidence for H₁ (alternative hypothesis)
- **BF₀₁(τ₂) > 1** → Evidence for H₀ (null hypothesis)

This answers Christian Robert's 2016 call to investigate the impact of the prior scale on Bayes factors, confirming his suspicion that "this choice involves arbitrariness to a rather high degree."

---

## Example: The Paradox in Action

**Data:** z = 2.0, n = 50, p-value = 0.046 (statistically significant)

**Flip point:** k* = 49.44, τ* = 0.99

| | Bayesian A | Bayesian B |
|---|---|---|
| **Prior SD (τ)** | 0.8 | 1.5 |
| **k = nτ²** | 32.0 | 112.5 |
| **Bayes Factor** | BF₀₁ = 0.83 | BF₀₁ = 1.47 |
| **Conclusion** | Favours H₁ | Favours H₀ |

*Both priors are scientifically reasonable, yet the conclusions are contradictory!*

---

## The "Perfect Storm" Scenario

The paradox is most dramatic with large samples and marginally significant results.

**Data:** n = 5000, z = 1.96 (exactly p = 0.05), effect size d = 0.028

| Prior Type | τ | BF₀₁ | Interpretation |
|------------|---|------|----------------|
| Ultra-Skeptical | 0.05 | 0.62 | Weak evidence for H₁ |
| JZS Default | 0.707 | 7.33 | Moderate evidence for H₀ |
| Diffuse | 1.0 | 10.36 | Strong evidence for H₀ |
| Very Diffuse | 2.0 | 20.72 | Strong evidence for H₀ |

**A 33-fold swing** in the Bayes factor across scientifically defensible prior choices!

---

## The Mathematics

The Bayes factor in favour of H₀ for the normal model with known variance:

```
BF₀₁(z; k) = √(1+k) · exp{-z²k / [2(1+k)]}
```

where k = nτ².

### The Flip Point Theorem

For |z| > 1, there exists a unique **flip point** k* satisfying:

```
(1+k*)·ln(1+k*) = z²·k*
```

At this point BF₀₁(k*) = 1. For k < k*, the Bayes factor favours H₁; for k > k*, it favours H₀.

### Universality Corollary

Since every significant result at α = 0.05 has |z| > 1.96 > 1, **every significant result exhibits the paradox**.

---

## Table of Flip Points

| z | z² | p-value | k* | τ* (n=50) | τ* (n=100) |
|---|---|---|---|---|---|
| 1.50 | 2.25 | 0.134 | 5.82 | 0.34 | 0.24 |
| **1.96** | **3.84** | **0.050** | **41.58** | **0.91** | **0.64** |
| 2.00 | 4.00 | 0.046 | 49.44 | 0.99 | 0.70 |
| 2.50 | 6.25 | 0.012 | 510.72 | 3.20 | 2.26 |
| 3.00 | 9.00 | 0.003 | 8093.08 | 12.72 | 9.00 |

---

## Repository Contents

```
BFR-Paradox/
├── README.md                              # This file
├── LICENSE                                # MIT License
├── BFR_Paradox_Biometrika.tex             # Main paper (LaTeX)
├── Supplementary_Material_BFR_Paradox.tex # Supplementary material (LaTeX)
├── BFR_Paradox_Supplementary_Code.R       # Complete R code
└── BFR_Paradox_Figure.R                   # Figure generation code
```

---

## Quick Start

### Using the Interactive Tool

Visit: **[BFR Paradox Interactive Demo](https://sites.radford.edu/~mlovric/BFR-Paradox-Interactive-Tool.html)**

### Using the R Code

```r
# Source the code
source("BFR_Paradox_Supplementary_Code.R")

# Compute Bayes factor
BF01(z = 2.0, k = 32)  # k = n * tau^2

# Find flip point
find_flip_point(z = 2.0)  # Returns 49.44

# Demonstrate the paradox
demonstrate_paradox(z = 2.0, n = 50)

# Generate Table 1 from the paper
generate_table1()

# Perfect Storm scenario
perfect_storm_scenario()
```

### Generate Figure 1

```r
source("BFR_Paradox_Figure.R")
# Creates BFR_Paradox_Figure.pdf
```

---

## Citation

If you use this work, please cite:

```bibtex
@article{lovric2025bfr,
  author  = {Lovric, Miodrag M.},
  title   = {The {B}ayes factor reversal paradox},
  journal = {Biometrika},
  year    = {2025},
  note    = {Forthcoming}
}
```

---

## Links

- **Interactive Tool:** [https://sites.radford.edu/~mlovric/BFR-Paradox-Interactive-Tool.html](https://sites.radford.edu/~mlovric/BFR-Paradox-Interactive-Tool.html)
- **GitHub Repository:** [https://github.com/MLovricStats/BFR-Paradox](https://github.com/MLovricStats/BFR-Paradox)
- **Author's Website:** [https://sites.radford.edu/~mlovric/](https://sites.radford.edu/~mlovric/)

---

## Historical Context

| Year | Event |
|------|-------|
| **1957** | Lindley publishes "A Statistical Paradox" in *Biometrika* |
| **1987** | Berger & Sellke argue p-values should be replaced by Bayes factors |
| **2016** | Robert publishes "The Expected Demise of the Bayes Factor," calling for research on prior scale impact |
| **2025** | **The BFR Paradox answers Robert's call** — proving the scale can reverse conclusions entirely |

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## Author

**Miodrag M. Lovric, Ph.D.**  
Professor of Statistics  
Department of Mathematics and Statistics  
Radford University, Virginia, USA  
📧 mlovric@radford.edu

---

## Acknowledgements

- **Christian Robert** for his prescient 2016 critique that inspired this investigation
- **Dennis Lindley** whose 1957 paradox laid the foundation
---

<p align="center">
  <b>The BFR Paradox</b><br>
  <i>Proving that Bayesians can disagree among themselves</i>
</p>
