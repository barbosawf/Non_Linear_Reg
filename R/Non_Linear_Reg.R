#https://www.r-bloggers.com/2020/12/accounting-for-the-experimental-design-in-linear-nonlinear-regression-analyses-2/
#https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.505.4518&rep=rep1&type=pdf
#https://tjmurphy.github.io/jabstb/nestedregress.html
#https://rstudio.github.io/r-manuals/r-intro/Statistical-models-in-R.html
# Instalando e carregando os pacotes --------------------------------------

library(tidyverse)
#devtools::install_github("onofriandreapg/aomisc")
library(aomisc)

# Leituda dos dados -------------------------------------------------------

dados <- readxl::read_xlsx("Feeding_spinosad.xlsx",col_names = TRUE) %>%
  mutate(
    colony = factor(colony)
  )



modNlin <- nls(Feeding ~ A*exp(-k*Treatment), 
               start=list(A=100, k=0.05), 
               data = dados)
summary(modNlin)

par(mfrow=c(1,2))
plotnls(modNlin, which = 1)
plotnls(modNlin, which = 2)

plotnls(modNlin, type = "means",
        xlab = "Concentration", ylab = "Consumption")


modNlin2 <- nls(Feeding ~ NLS.expoDecay(Treatment, a, k),
                data = dados)


modAov <- lm(Feeding ~ factor(Treatment), data=dados)
anova(modNlin, modAov)

MSE <- summary(modNlin)$sigma ^ 2
MST <- var(dados$Feeding)
1 - MSE/MST

R2nls(modNlin)$PseudoR2

bc <- boxcox(modNlin)
bc$lambda
boxcox(modNlin, plotit = T)


modNlin3 <- boxcox(modNlin, lambda = 0.5)
summary(modNlin3)
