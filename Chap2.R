# X ~ N(0.05, (0.10)^2)
mu_x <- 0.05
sigma_x <- 0.10


# Pr(X > 0.10)
head(1 - pnorm(0.10,  mean = mu_x, sd = sigma_x))

# Pr(X < -0.10)

head(pnorm(-0.10,  mean = mu_x, sd = sigma_x))

# Pr(-0.05 < X < 0.15)

head(pnorm(0.15, mean = mu_x, sd = sigma_x) - pnorm(-0.05,  mean = mu_x, sd = sigma_x))

#------------------------------------#


# The mean (mu_x) and the standard deviation (sigma_x) are still in your workspace

# 1%, 5%, 95% and 99% quantile
head(qnorm(c(.01, .05, .95, .99), mean = mu_x, sd = sigma_x))

#------------------------------------#

# Normally distributed monthly returns
x_vals <- seq(-0.25, 0.35, length.out = 100)
MSFT <- dnorm(x_vals, mean = 0.05, sd = 0.1)
SBUX <- dnorm(x_vals, mean = 0.025, sd = 0.05)

#------------------------------------#

# MSFT and x_vals are still in your workspace

# Normal curve for MSFT

plot(x_vals, MSFT, type = "l", col = "blue", ylab = "Normal curves", ylim = c(0,8))


#------------------------------------#
# MSFT, SBUX and x_vals are still in your workspace

# Normal curve for MSFT
plot(x_vals, MSFT, type = "l", col = "blue", ylab = "Normal curves", 
     ylim = c(0, 8))

# Add a normal curve for SBUX
lines(x_vals, SBUX, type = "l", col = "red", ylab = "Normal curves", 
     ylim = c(0, 8))

# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), 
       col = c("blue", "red"), lty = 1)

#------------------------------------#
# R ~ N(0.04, (0.09)^2) 
mu_R <- 0.04
sigma_R <- 0.09

# Initial wealth W0 equals $100,000
W0 <- 100000

# The 1% value-at-risk
head(W0 * (mu_R + qnorm(0.01) * sigma_R))

# The 5% value-at-risk
head(W0 * (mu_R + qnorm(0.05) * sigma_R))

#------------------------------------#

# r ~ N(0.04, (0.09)^2) 
mu_r <- 0.04
sigma_r <- 0.09

# Initial wealth W0 equals $100,000
W0 <- 100000

# The 1% value-at-risk
W0 * (exp(qnorm(0.01, mean = mu_r, sd = sigma_r)) - 1)

# The 5% value-at-risk
W0 * (exp(qnorm(0.05, mean = mu_r, sd = sigma_r)) - 1)

#------------------------------------#

# Vectors of prices
PA <- c(38.23, 41.29)
PC <- c(41.11, 41.74)

# Simple monthly returns
RA <- (PA[2] - PA[1]) / PA[1]
RC <- (PC[2] - PC[1]) / PC[1]
#------------------------------------#

# The simple returns on Amazon (RA) and Costco (RC) are still in your workspace

# Continuously compounded returns
rA <- log(1 + RA)
rC <- log(1 + RC)
#------------------------------------#

# The prices for Amazon (PA) are still in your workspace

# Cash dividend per share
DA <- 0.10

# Simple total return
RA_total <- ((PA[2] - PA[1]) + DA) / PA[1]

# Dividend yield
DY <- DA / PA[1]

#------------------------------------#

# The simple monthly return on Amazon (RA) is still in your workspace

# Simple annual return
RA_annual <- (1 + RA) ** 12 - 1

# Continuously compounded annual return
rA_annual <- log(1 + RA_annual)


#------------------------------------#

# The simple returns on Amazon (RA) and Costco (RC) are still in your workspace

# Portfolio shares
xA <- 8000 / 10000
xC <- 2000 / 10000

