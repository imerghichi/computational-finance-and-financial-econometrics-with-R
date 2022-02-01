# The variable return_matrix is preloaded in your workspace

# Number of observations
n_obs <- dim(return_matrix)[1]

# Estimates of sigma2hat
sigma2hat_vals <- apply(return_matrix, 2, var)

# Standard Error of sigma2hat
se_sigma2hat <-  sigma2hat_vals/sqrt(n_obs/2)
se_sigma2hat

#------------------------------------#
# The variable return_matrix is preloaded in your workspace

# Calculate the correlation matrix
cor_matrix <- cor(return_matrix)

# Get the lower triangular part of that 'cor_matrix'
rhohat_vals <- cor_matrix[lower.tri(cor_matrix)]

# Set the names
names(rhohat_vals) <- c("VBLTX, FMAGX", "VBLTX, SBUX", "FMAGX, SBUX")

# Compute the estimated standard errors for correlation
se_rhohat <- (1 - rhohat_vals^2)/sqrt(dim(return_matrix)[1])
se_rhohat

#------------------------------------#

# The all_returns zoo object is preloaded in your workspace
t.test(all_returns[, "VBLTX"])
t.test(all_returns[, "FMAGX"])
cor.test(all_returns[, "VBLTX"], all_returns[, "FMAGX"])
cor.test(all_returns[, "VBLTX"], all_returns[, "SBUX"])

#------------------------------------#

# The all_returns zoo object is preloaded in your workspace

# Test the correlation between VBLTX, FMAGX
cor.test(x = all_returns[,"VBLTX"], y = all_returns[,"FMAGX"])

#------------------------------------#

# The all_returns zoo object is preloaded in your workspace

# Test the normality of the returns of VBLTX
jarque.bera.test(all_returns[,"VBLTX"])

#------------------------------------#

# Function for bootstrapping sample mean: 
mean_boot <- function(x, idx) {
  ans <- mean(x[idx])
  ans 
} 

# Construct VBLTX_mean_boot:
VBLTX_mean_boot <- boot(return_matrix[,"VBLTX"], R = 999, statistic = mean_boot)

# Print the class of VBLTX_mean_boot
class(VBLTX_mean_boot)

# Print VBLTX_mean_boot
VBLTX_mean_boot

# Plot bootstrap distribution and qq-plot against normal
plot(VBLTX_mean_boot)

#------------------------------------#

