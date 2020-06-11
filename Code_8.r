# Clear All
rm(list = ls())

# bibliotecas
library(urca)
library(dplyr)
library(ggplot2)
library(cowplot)

# ---- Code 8.1 ----
# load data
data(UKpppuip)

names(UKpppuip)
# attach(UKpppuip)

# Select endogenous variables and exogenous variables
dat1 <- UKpppuip %>% select(p1, p2, e12, i1, i2)
dat2 <- UKpppuip %>% select(doilp0, doilp1)

# print the arguments of the function ca.jo
print(args('ca.jo'))

# make a trace test for cointegration
H1.trace <- ca.jo(
  dat1,
  type = 'trace',
  K = 2,
  season = 4,
  dumvar = dat2
)

# print the summary of the test
print(summary(H1.trace))

# make a eigenvalue test for cointegration
H1.eigen <-
  ca.jo(
    dat1,
    type = 'eigen',
    K = 2,
    season = 4,
    dumvar = dat2
  )

# print the summary of the test
print(summary(H1.eigen))


# ---- Code 8.2 ----

# Select the beta estimated
beta <- H1.trace@V

# normalize the betas
beta[,2] <- beta[,2]/beta[4 ,2]
beta[,3] <- beta[,3]/beta[4 ,3]

# Calulate the alpha corresponding to the normalization
alpha <- H1.trace@PI%*%solve(t(beta))

beta1 <- cbind(beta[, 1:3], H1.trace@V[,4:5])

# sprintf("%.2f", beta1)

ci.1 <- ts((H1.trace@x%*%beta1)[-c(1, 2),], start=c(1972, 3), end=c (1987, 2), frequency=4)

ci.2 <- ts(H1.trace@RK%*%beta1, start=c(1972,3), end=c(1987,2), frequency=4)

# ---- Fig. 8.2. Graphical display of the first two cointegration relations  ----
tbl_ci.1 = as_tibble(ci.1)
tbl_ci.1$Time = as.numeric(time(ci.1))

tbl_ci.2 = as_tibble(ci.2)
tbl_ci.2$Time = as.numeric(time(ci.2))

p1 <- ggplot(tbl_ci.1) +
  geom_line(aes(y=p1.l2, x=Time)) +
  labs(x=NULL, y=NULL)

p2 <- ggplot(tbl_ci.2) +
  geom_line(aes(y=p1.l2, x=Time)) +
  labs(x=NULL, y=NULL)

p3 <- ggplot(tbl_ci.1) +
  geom_line(aes(y=p2.l2, x=Time)) +
  labs(x=NULL, y=NULL)

p4 <- ggplot(tbl_ci.2) +
  geom_line(aes(y=p2.l2, x=Time)) +
  labs(x=NULL, y=NULL)

plot_grid(p1, p2, p3, p4)
# ---- ssss ----


 