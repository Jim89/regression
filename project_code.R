
# You work for Motor Trend, a magazine about the automobile industry.
# Looking at a data set of a collection of cars, they are interested in
# exploring the relationship between a set of variables and miles per gallon.
# They are particularly interested in the following two questions:

# "Is an automatic or manual transmission better for MPG"
# "Quantify the MPG difference between automatic and manual transmissions"
################################################################################
# Step 0 - Set up environment - packages and working directory
################################################################################
library("dplyr", quietly = T)
library("ggplot2", quietly = T)
library("magrittr", quietly = T)
library("GGally", quietly = T)

dir <- "C:/Users/Jleach1/Documents/R/regression_models"
setwd(dir)
rm(dir)
################################################################################
# Step 1 - Load data
################################################################################
data(mtcars)
mtcars <- mtcars %>%
          mutate(trans = as.factor(ifelse(am==0,"Automatic","Manual")))

################################################################################
# Step 2 - initial explorations
################################################################################
summ_simple <- mtcars %>%
                group_by(trans) %>%
                summarise(mean_mpg = mean(mpg),
                         sd = sd(mpg),
                          n = n(),
                          se = sd/sqrt(n),
                          ss_n = (sd^2)/n)
summ_simple

violin <-
mtcars %>%
  ggplot(aes(x = trans, y = mpg, fill = trans))+
  geom_violin()+
  geom_point()+
  theme_minimal()+
  scale_fill_manual(values = c("steelblue","firebrick"))+
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Transmission Type")+
  ylab("Miles per Gallon")

pairs <- ggpairs(mtcars)

fit_am <- lm(mpg ~ am, data = mtcars)
summary(fit_am)
fit_amResid <- fit_am$resid
plot(fit_amResid, mtcars$mpg)

fit_all <- lm(mpg ~ ., data = mtcars)
summary(fit_all)

fit_refined <- lm(mpg ~ wt+am, data = mtcars)
summary(fit_refined)
fit_refinedResid <- fit_refined$resid
plot(fit_refinedResid, mtcars$mpg)




