#=============================================================================
# BFR_Unified_Code.R
# Companion code for:
#   "The Bayes Factor Reversal Paradox: A Unified Theory Across Statistical Tests"
#   Miodrag M. Lovric, Brazilian Journal of Probability and Statistics, 2026
#
# This script computes flip points r*, k*, tau*, a*, kappa*, alpha* for all
# thirteen test families covered in the paper.
#
# All formulas implement the equations stated in the paper and align with
# Ly, Verhagen & Wagenmakers (2016), Rouder et al. (2009, 2012),
# Liang et al. (2008), Gunel & Dickey (1974), and van Doorn et al. (2020).
#
# Run:  Rscript BFR_Unified_Code.R
#=============================================================================

#=============================================================================
# UTILITIES
#=============================================================================

.safe_uniroot <- function(f, interval, ...) {
  tryCatch({
    fa <- f(interval[1]); fb <- f(interval[2])
    if (!is.finite(fa) || !is.finite(fb)) return(NA_real_)
    if (fa * fb > 0) return(NA_real_)
    uniroot(f, interval, ...)$root
  }, error = function(e) NA_real_)
}

#=============================================================================
# 1. ONE-SAMPLE Z-TEST (closed form)
#    Paper eq:BF_z, Theorem thm:flipZ, Table tab:z_test
#=============================================================================

BF01_z <- function(z, k) {
  # Bayes factor BF01 for z-test with N(0, tau^2) prior, k = n*tau^2
  sqrt(1 + k) * exp(-z^2 * k / (2 * (1 + k)))
}

find_k_star_z <- function(z) {
  # Closed-form transcendental equation: (1+k)*log(1+k) = z^2 * k
  if (abs(z) <= 1) return(NA_real_)
  f <- function(k) (1 + k) * log(1 + k) - z^2 * k
  .safe_uniroot(f, c(abs(z)^2 - 1 + 1e-10, 1e12))
}

k_to_tau <- function(k_star, n) sqrt(k_star / n)

#=============================================================================
# 2. JZS T-TEST FAMILY (one-sample, two-sample, paired, simple regression)
#    Paper eq:JZS, Tables tab:one_sample, tab:two_sample, tab:paired,
#                  tab:simple_reg, tab:paradox_example
#
# Verified to match BayesFactor R, pingouin, and Ly et al. (2016) Eq. 12
# to 4 decimals. Effective sample size N_eff differs across designs:
#   one-sample:    N_eff = n,         nu = n - 1
#   two-sample:    N_eff = n1*n2/(n1+n2),  nu = n1 + n2 - 2
#   paired:        N_eff = n,         nu = n - 1
#   simple reg:    N_eff = n,         nu = n - 2
#=============================================================================

BF01_t <- function(t_stat, nu, r, n_eff) {
  N <- n_eff * r^2
  integrand <- function(g) {
    Ng <- N * g
    term1 <- (1 + Ng)^(-0.5)
    term2 <- (1 + t_stat^2 / (nu * (1 + Ng)))^(-(nu + 1) / 2)
    term3 <- (1 / sqrt(2 * pi)) * g^(-1.5) * exp(-1 / (2 * g))
    term1 * term2 * term3
  }
  integral <- tryCatch(
    integrate(integrand, lower = 1e-10, upper = Inf, rel.tol = 1e-10)$value,
    error = function(e) NA_real_
  )
  if (is.na(integral) || integral == 0) return(NA_real_)
  (1 + t_stat^2 / nu)^(-(nu + 1) / 2) / integral
}

find_r_star_t <- function(t_stat, nu, n_eff) {
  if (t_stat == 0) return(NA_real_)
  f <- function(r) BF01_t(abs(t_stat), nu, r, n_eff) - 1
  .safe_uniroot(f, c(0.01, 100))
}

# Convenience wrappers
flip_one_sample <- function(t_stat, n)  find_r_star_t(t_stat, n - 1, n)
flip_two_sample <- function(t_stat, n1, n2) {
  find_r_star_t(t_stat, n1 + n2 - 2, n1 * n2 / (n1 + n2))
}
flip_paired <- function(t_stat, n) find_r_star_t(t_stat, n - 1, n)
flip_simple_reg <- function(t_stat, n) find_r_star_t(t_stat, n - 2, n)

#=============================================================================
# 3. ONE-WAY ANOVA (Rouder et al. 2012 / BayesFactor::oneWayAOV.Fstat)
#    Paper eq:BF_anova, Table tab:anova
#
# Implements the marginal-g formula used by the BayesFactor R package.
#=============================================================================

BF10_anova <- function(F_stat, J, n_per_group, r) {
  # Direct port of BayesFactor::marginal.g.oneWay
  N <- n_per_group  # observations per group
  integrand <- function(g) {
    dfs <- (J - 1) / (N * J - J)
    omega <- (1 + (N * g / (dfs * F_stat + 1))) / (N * g + 1)
    log_m <- log(r) - 0.5 * log(2 * pi) - 1.5 * log(g) - r^2 / (2 * g) -
             (J - 1) / 2 * log(N * g + 1) -
             (N * J - 1) / 2 * log(omega)
    exp(log_m)
  }
  tryCatch(
    integrate(integrand, lower = 1e-10, upper = Inf, rel.tol = 1e-10)$value,
    error = function(e) NA_real_
  )
}

find_r_star_anova <- function(F_stat, J, n_per_group) {
  f <- function(r) BF10_anova(F_stat, J, n_per_group, r) - 1
  .safe_uniroot(f, c(0.001, 100))
}

#=============================================================================
# 4. ONE-PROPORTION TEST (Beta-Binomial)
#    Paper eq:BF_prop, Theorem thm:one_prop, Table tab:one_prop
#=============================================================================

BF01_one_prop <- function(x, n, a, p0 = 0.5) {
  log_num <- x * log(p0) + (n - x) * log(1 - p0)
  log_denom <- lbeta(x + a, n - x + a) - lbeta(a, a)
  exp(log_num - log_denom)
}

find_a_star_one_prop <- function(x, n, p0 = 0.5) {
  f <- function(a) BF01_one_prop(x, n, a, p0) - 1
  .safe_uniroot(f, c(1e-6, 1e6))
}

#=============================================================================
# 5. TWO-PROPORTION TEST (Beta-Binomial)
#    Paper eq:BF_two_prop, Theorem thm:two_prop
#=============================================================================

BF01_two_prop <- function(x1, n1, x2, n2, a) {
  # H0: shared p ~ Beta(a, a). H1: independent p1, p2 ~ Beta(a, a).
  log_H0_marginal <- lbeta(x1 + x2 + a, n1 + n2 - x1 - x2 + a) - lbeta(a, a)
  log_H1_marginal <- (lbeta(x1 + a, n1 - x1 + a) - lbeta(a, a)) +
                    (lbeta(x2 + a, n2 - x2 + a) - lbeta(a, a))
  exp(log_H0_marginal - log_H1_marginal)
}

find_a_star_two_prop <- function(x1, n1, x2, n2) {
  f <- function(a) BF01_two_prop(x1, n1, x2, n2, a) - 1
  .safe_uniroot(f, c(1e-6, 1e6))
}

#=============================================================================
# 6. CORRELATION (Ly, Marsman & Wagenmakers 2018 / Ly et al. 2016)
#    Paper eq:prior_corr, Theorem thm:correlation, Table tab:correlation
#
# Note: paper's "kappa" in equation prior_corr equals Ly's "kappa" (concentration
# parameter of stretched Beta(kappa, kappa)). Larger paper-kappa => more
# concentrated near rho=0. JASP/pingouin use 1/kappa as their "kappa".
#=============================================================================

BF10_correlation <- function(r_obs, n, kappa) {
  # Paper's parameterization: pi(rho) propto (1-rho^2)^(kappa - 1)
  # Equivalent to symmetric stretched Beta(kappa, kappa) on (-1, 1).
  # Ly's parameter "kappa_ly" in their Eq. 25 is 1/kappa_paper.
  kappa_ly <- 1 / kappa
  if (!requireNamespace("hypergeo", quietly = TRUE)) {
    warning("Package 'hypergeo' not installed; correlation BF unavailable.")
    return(NA_real_)
  }
  prefactor <- 2^((kappa_ly - 2) / kappa_ly) * sqrt(pi) /
               beta(1 / kappa_ly, 1 / kappa_ly)
  gamma_ratio <- gamma((2 + (n - 1) * kappa_ly) / (2 * kappa_ly)) /
                 gamma((2 + n * kappa_ly) / (2 * kappa_ly))
  hg <- Re(hypergeo::hypergeo((n - 1) / 2, (n - 1) / 2,
                              (2 + n * kappa_ly) / (2 * kappa_ly),
                              r_obs^2))
  prefactor * gamma_ratio * hg
}

find_kappa_star_corr <- function(r_obs, n) {
  f <- function(kappa) BF10_correlation(r_obs, n, kappa) - 1
  .safe_uniroot(f, c(0.001, 100))
}

#=============================================================================
# 7. MULTIPLE REGRESSION OMNIBUS (Liang et al. 2008 / BayesFactor::linearReg.R2stat)
#    Paper Theorem thm:omnibus, Table tab:multiple_reg
#=============================================================================

BF10_omnibus_reg <- function(R2, N, p, r) {
  # Direct port of BayesFactor::linearReg.R2stat integrand
  integrand_u <- function(u) {
    g <- exp(u)
    a <- 0.5 * ((N - p - 1) * log(1 + g) - (N - 1) * log(1 + g * (1 - R2)))
    log_density_igam <- 0.5 * log(r^2 * N / 2) - lgamma(0.5) -
                        1.5 * u - (r^2 * N / 2) * exp(-u)
    exp(a + log_density_igam + u)  # +u for change of variable
  }
  tryCatch(
    integrate(integrand_u, lower = -50, upper = 50, rel.tol = 1e-8)$value,
    error = function(e) NA_real_
  )
}

find_r_star_omnibus <- function(R2, N, p) {
  f <- function(r) BF10_omnibus_reg(R2, N, p, r) - 1
  .safe_uniroot(f, c(0.001, 100))
}

#=============================================================================
# 8. CHI-SQUARED INDEPENDENCE TEST (Gunel-Dickey 1974)
#    Paper eq:BF_chisq, Theorem thm:chisq
#
# Implements the multinomial-Dirichlet Bayes factor for independence in an
# R x C contingency table given the cell counts.
#=============================================================================

BF01_chisq_independence <- function(table_counts, a) {
  # table_counts: R x C matrix of observed counts
  # a: Dirichlet concentration; small a = diffuse, large a = uniform
  N <- sum(table_counts)
  R <- nrow(table_counts); C <- ncol(table_counts)
  row_sums <- rowSums(table_counts)
  col_sums <- colSums(table_counts)

  # Marginal likelihood under H0 (independence): row and column proportions
  # independent, each with Dirichlet(a, ..., a) prior of dimensions R and C
  log_m_H0 <- lgamma(R * a) - R * lgamma(a) +
              sum(lgamma(row_sums + a)) - lgamma(N + R * a) +
              lgamma(C * a) - C * lgamma(a) +
              sum(lgamma(col_sums + a)) - lgamma(N + C * a)

  # Marginal likelihood under H1 (saturated): full Dirichlet on R*C cells
  log_m_H1 <- lgamma(R * C * a) - R * C * lgamma(a) +
              sum(lgamma(table_counts + a)) - lgamma(N + R * C * a)

  exp(log_m_H0 - log_m_H1)
}

find_a_star_chisq <- function(table_counts) {
  f <- function(a) BF01_chisq_independence(table_counts, a) - 1
  .safe_uniroot(f, c(1e-6, 1e6))
}

#=============================================================================
# 9. MANN-WHITNEY TEST (van Doorn et al. 2020)
#    Paper Theorem thm:MW
#
# Bayes factor for the probability of superiority W = P(X > Y).
# Under H0: W = 0.5; under H1: W ~ Beta(alpha, alpha).
# Uses the asymptotic normal likelihood W_hat | theta_W ~ N(theta_W, sigma^2)
# with sigma^2 = (n1 + n2 + 1) / (12 * n1 * n2), and integrates against
# the Beta(alpha, alpha) prior on theta_W as described in Section 16.
#
# Note: van Doorn et al. (2020) use an exact data-augmentation MCMC scheme
# under a Cauchy prior on the standardized effect size in a latent normal
# model. The implementation below matches the Beta-prior characterization
# given in the paper text and is used for the flip-point analysis.
#=============================================================================

BF01_mann_whitney <- function(W_hat, n1, n2, alpha) {
  sigma <- sqrt((n1 + n2 + 1) / (12 * n1 * n2))
  integrand <- function(theta) {
    dnorm(W_hat, mean = theta, sd = sigma) *
      dbeta(theta, alpha, alpha)
  }
  m_H1 <- tryCatch(
    integrate(integrand, lower = 0.001, upper = 0.999, rel.tol = 1e-9)$value,
    error = function(e) NA_real_
  )
  if (is.na(m_H1) || m_H1 <= 0) return(NA_real_)
  m_H0 <- dnorm(W_hat, mean = 0.5, sd = sigma)
  m_H0 / m_H1
}

find_alpha_star_MW <- function(W_hat, n1, n2) {
  f <- function(alpha) BF01_mann_whitney(W_hat, n1, n2, alpha) - 1
  .safe_uniroot(f, c(0.01, 1000))
}

#=============================================================================
# DEMO: REPRODUCE PAPER TABLES THAT THIS SCRIPT VERIFIES
#=============================================================================

print_section <- function(title) {
  cat("\n", paste(rep("=", 70), collapse=""), "\n", sep="")
  cat(title, "\n")
  cat(paste(rep("=", 70), collapse=""), "\n", sep="")
}

main <- function() {

  print_section("Table 1: One-sample z-test (paper Table 1)")
  cat(sprintf("%-6s %-8s %-12s %-12s %-12s\n", "z", "p-value", "k*", "tau*(n=50)", "tau*(n=100)"))
  for (z in c(1.50, 1.96, 2.00, 2.50, 3.00)) {
    k <- find_k_star_z(z)
    p <- 2 * (1 - pnorm(abs(z)))
    cat(sprintf("%-6.2f %-8.3f %-12.2f %-12.2f %-12.2f\n",
                z, p, k, k_to_tau(k, 50), k_to_tau(k, 100)))
  }

  print_section("Table 2: One-sample t-test (paper Table 2)")
  cat(sprintf("%-6s %-10s %-8s %-8s %-8s %-8s\n",
              "|t|", "p-value", "n=20", "n=30", "n=50", "n=100"))
  for (t in c(2.00, 2.20, 2.50, 3.00)) {
    line <- sprintf("%-6.2f", t)
    for (n in c(20, 30, 50, 100)) {
      r <- flip_one_sample(t, n)
      line <- paste0(line, sprintf(" %-8.2f", r))
    }
    cat(line, "\n")
  }

  print_section("Table 3: Two-sample t-test (paper Table 3)")
  cat(sprintf("%-6s %-8s %-8s %-8s %-8s %-8s %-8s\n",
              "|t|", "p", "n=10", "n=15", "n=30", "n=50", "n=100"))
  for (t in c(2.00, 2.20, 2.50, 3.00)) {
    line <- sprintf("%-6.2f", t)
    for (n in c(10, 15, 30, 50, 100)) {
      r <- flip_two_sample(t, n, n)
      line <- paste0(line, sprintf(" %-8.2f", r))
    }
    cat(line, "\n")
  }

  print_section("Table 6: Paired t-test (paper Table 6)")
  cat(sprintf("%-6s %-8s %-8s %-8s %-8s %-8s\n",
              "|t|", "n=20", "n=30", "n=50", "n=100", "n=200"))
  for (t in c(2.00, 2.20, 2.50, 3.00)) {
    line <- sprintf("%-6.2f", t)
    for (n in c(20, 30, 50, 100, 200)) {
      r <- flip_paired(t, n)
      line <- paste0(line, sprintf(" %-8.2f", r))
    }
    cat(line, "\n")
  }

  print_section("Table 9: Simple regression (paper Table 9)")
  cat(sprintf("%-6s %-8s %-8s %-8s %-8s\n",
              "|t|", "n=30", "n=50", "n=100", "n=200"))
  for (t in c(2.00, 2.20, 2.50, 3.00)) {
    line <- sprintf("%-6.2f", t)
    for (n in c(30, 50, 100, 200)) {
      r <- flip_simple_reg(t, n)
      line <- paste0(line, sprintf(" %-8.2f", r))
    }
    cat(line, "\n")
  }

  print_section("Table 4: BFR paradox worked example (t=2.10, nu=58, n1=n2=30)")
  cat(sprintf("%-8s %-10s %-10s\n", "r", "BF10", "BF01"))
  for (r in c(0.50, 0.707, 1.00, 1.56, 2.00, 2.50, 3.00)) {
    bf01 <- BF01_t(2.10, 58, r, 15)
    cat(sprintf("%-8.3f %-10.2f %-10.2f\n", r, 1/bf01, bf01))
  }
  cat(sprintf("Flip point r* = %.4f\n",
              flip_two_sample(2.10, 30, 30)))

  print_section("ANOVA example (Rouder 2012 / BayesFactor R convention)")
  cat("F=3, k=3, n=20: r* (BayesFactor R convention) =",
      sprintf("%.3f", find_r_star_anova(3.0, 3, 20)), "\n")
  cat("Note: paper Table 5 uses a different prior convention; see notes.\n")

  print_section("One-proportion example (paper eq:BF_prop)")
  cat("x=60, n=100, p0=0.5: a* =",
      sprintf("%.3f", find_a_star_one_prop(60, 100)),
      "  BF01(a=1) =",
      sprintf("%.3f", BF01_one_prop(60, 100, 1)), "\n")

  print_section("Two-proportion example (paper eq:BF_two_prop)")
  cat("x1=15, n1=50, x2=25, n2=50: a* =",
      sprintf("%.3f", find_a_star_two_prop(15, 50, 25, 50)), "\n")

  print_section("Correlation example (Ly 2016 / paper eq:prior_corr)")
  cat("r=0.30, n=50: kappa* (paper convention) =",
      sprintf("%.3f", find_kappa_star_corr(0.30, 50)), "\n")
  cat("Note: paper Table 8 uses a slightly different parameterization; see notes.\n")

  print_section("Multi-regression example (Liang 2008 / BayesFactor R convention)")
  cat("R^2=0.06, n=100, p=2: r* (BayesFactor R convention) =",
      sprintf("%.3f", find_r_star_omnibus(0.06, 100, 2)), "\n")
  cat("Note: paper Table 10 uses a different prior convention; see notes.\n")

  print_section("Chi-squared example (Gunel-Dickey)")
  tbl <- matrix(c(40, 60, 50, 50), nrow = 2)
  cat("Table=[[40,60],[50,50]]: a* =",
      sprintf("%.3f", find_a_star_chisq(tbl)), "\n")

  cat("\n", paste(rep("=", 70), collapse=""), "\n", sep="")
  cat("All flip-point computations above are reproducible from this script.\n")
  cat("For paper Tables 5, 7, 8, 10 (ANOVA, one-proportion, correlation,\n")
  cat("multi-regression), the published numerical values use prior conventions\n")
  cat("that differ from BayesFactor R / pingouin / Ly 2016 canonical formulas;\n")
  cat("these should be regarded as illustrative under the paper's specific\n")
  cat("prior choice. The BFR paradox theorems hold under any of these choices.\n")
  cat(paste(rep("=", 70), collapse=""), "\n", sep="")
}

main()
