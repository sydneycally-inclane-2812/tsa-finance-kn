install.packages("rmgarch", repos = "http://cran.us.r-project.org")
library(rmgarch)
library(tidyverse)

library(parallel)
library(quantmod)
install.packages(quantmod)

returns <- read.csv("returns.csv")
head(returns)

### Directly from book ###

# define GARCH(1,1) model
univariate_spec <- mgarchspec(
    mean.model = list(armaOrder = c(0,0)),
    variance.model = list(garchOrder = c(1,1),
                          model = "sGARCH"),
    distribution.model = "norm"
)

n <- dim(returns)[2]
dcc_spec <- dccspec(
    uspec = multispec(replicate(n, univariate_spec)),
    dccOrder = c(1, 1),
    distribution = "mvnorm"
)

dcc.fit <- dccfit(dcc_spec, data = na.omit(returns))
dcc_fit

### End of book code ###



# Create a GARCH(1,1) model
garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0)),
                      distribution.model = "norm")
n <- 4
dcc_spec <- dccspec(uspec = multispec(replicate(n, garch11)),
                    dccOrder = c(1, 1),
                    distribution = "mvnorm")

dcc_fit <- dccfit(dcc_spec, data = data_in, fit.control = list(eval.se = TRUE), fit = parallel_fit, cluster = cl)
dcc_fit