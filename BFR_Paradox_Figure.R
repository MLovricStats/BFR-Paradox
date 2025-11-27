# ==============================================================================
# Figure for "The Bayes Factor Reversal Paradox"
# Lovric (2025), Biometrika
# ==============================================================================

# Bayes factor function
BF01 <- function(z, k) {
  sqrt(1 + k) * exp(-z^2 * k / (2 * (1 + k)))
}

# Flip point solver
find_flip_point <- function(z) {
  f <- function(k) (1 + k) * log(1 + k) - z^2 * k
  uniroot(f, c(0.01, 100000))$root
}

# ==============================================================================
# FIGURE 1: The BFR Paradox - BF as function of prior scale
# ==============================================================================

pdf("BFR_Paradox_Figure.pdf", width = 8, height = 5)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Panel A: Multiple z-values
k_vals <- exp(seq(log(0.1), log(1000), length.out = 500))

plot(NULL, xlim = c(0.1, 1000), ylim = c(0.1, 100), 
     log = "xy", xlab = expression(italic(k) == italic(n)*tau^2),
     ylab = expression(BF[0][1]),
     main = "(A) Bayes factor vs prior scale")

# Horizontal line at BF = 1
abline(h = 1, lty = 2, col = "gray50", lwd = 2)

# Plot for different z values
colors <- c("blue", "darkgreen", "red", "purple")
z_vals <- c(1.5, 1.96, 2.0, 2.5)

for (i in seq_along(z_vals)) {
  z <- z_vals[i]
  bf_vals <- sapply(k_vals, function(k) BF01(z, k))
  lines(k_vals, bf_vals, col = colors[i], lwd = 2)
  
  # Mark flip point
  k_star <- find_flip_point(z)
  points(k_star, 1, pch = 19, col = colors[i], cex = 1.5)
}

legend("topleft", legend = paste("z =", z_vals), 
       col = colors, lwd = 2, bty = "n")

# Add region labels
text(0.5, 0.1, expression("Evidence for " * H[1]), col = "darkblue", cex = 0.9)
text(200, 20, expression("Evidence for " * H[0]), col = "darkred", cex = 0.9)

# Panel B: Specific example (z = 2.0, n = 50)
z <- 2.0
n <- 50
k_star <- find_flip_point(z)
tau_star <- sqrt(k_star / n)

tau_vals <- seq(0.1, 2.5, length.out = 500)
k_from_tau <- n * tau_vals^2
bf_vals <- sapply(k_from_tau, function(k) BF01(z, k))

plot(tau_vals, bf_vals, type = "l", lwd = 2, col = "black",
     xlab = expression("Prior scale " * tau),
     ylab = expression(BF[0][1]),
     main = "(B) Example: z = 2.0, n = 50",
     ylim = c(0, 2.5))

# Horizontal line at BF = 1
abline(h = 1, lty = 2, col = "gray50", lwd = 2)

# Vertical line at flip point
abline(v = tau_star, lty = 3, col = "red", lwd = 2)

# Mark specific priors
tau_examples <- c(0.8, 1.5)
for (tau in tau_examples) {
  k <- n * tau^2
  bf <- BF01(z, k)
  points(tau, bf, pch = 19, cex = 1.5, 
         col = ifelse(bf < 1, "blue", "red"))
  text(tau, bf + 0.15, sprintf("τ = %.1f\nBF = %.2f", tau, bf), 
       cex = 0.8, col = ifelse(bf < 1, "blue", "red"))
}

# Label flip point
text(tau_star + 0.15, 1.2, expression(tau * "*" == 0.99), col = "red", cex = 0.9)

# Shade regions
polygon(c(0, tau_star, tau_star, 0), c(0, 0, 1, 1), 
        col = rgb(0, 0, 1, 0.1), border = NA)
polygon(c(tau_star, 2.5, 2.5, tau_star), c(1, 1, 2.5, 2.5), 
        col = rgb(1, 0, 0, 0.1), border = NA)

text(0.4, 0.3, expression("Favors " * H[1]), col = "darkblue", cex = 0.9)
text(1.4, 2.0, expression("Favors " * H[0]), col = "darkred", cex = 0.9)

dev.off()

cat("Figure saved to BFR_Paradox_Figure.pdf\n")

# ==============================================================================
# VERIFICATION: Reproduce Table 1
# ==============================================================================

cat("\n=== Table 1 Verification ===\n")
z_vals <- c(1.50, 1.96, 2.00, 2.50, 3.00)
for (z in z_vals) {
  k_star <- find_flip_point(z)
  tau_50 <- sqrt(k_star / 50)
  tau_100 <- sqrt(k_star / 100)
  p_val <- 2 * pnorm(-abs(z))
  cat(sprintf("z = %.2f: k* = %.2f, τ*(n=50) = %.2f, τ*(n=100) = %.2f, p = %.3f\n",
              z, k_star, tau_50, tau_100, p_val))
}

# ==============================================================================
# VERIFICATION: Perfect Storm Scenario
# ==============================================================================

cat("\n=== Perfect Storm Verification (z = 1.96, n = 5000) ===\n")
z <- 1.96
n <- 5000
k_star <- find_flip_point(z)
tau_star <- sqrt(k_star / n)
cat(sprintf("Flip point: k* = %.2f, τ* = %.4f\n\n", k_star, tau_star))

tau_vals <- c(0.05, 0.10, 0.20, 0.50, 0.707, 1.0, 1.5, 2.0)
for (tau in tau_vals) {
  k <- n * tau^2
  bf <- BF01(z, k)
  cat(sprintf("τ = %.3f: BF01 = %.3f\n", tau, bf))
}
