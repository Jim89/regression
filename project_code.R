
# You work for Motor Trend, a magazine about the automobile industry.
# Looking at a data set of a collection of cars, they are interested in
# exploring the relationship between a set of variables and miles per gallon.
# They are particularly interested in the following two questions:

# "Is an automatic or manual transmission better for MPG"
# "Quantify the MPG difference between automatic and manual transmissions"

# order to do things in:
  # set up working environment and read in data
  # v. simple quantification of data - summary and simple violin plot?
  # exploratory - do a pairs plot
  # fit a few models - select one and provide justifcation
  # do some diagnostics on the selected models - residual plot, other diagnostic
  # answer the question - provide expected change in MPG with confidence interval!

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
# mtcars <- mtcars %>%
#           mutate(trans = as.factor(ifelse(am==0,"Automatic","Manual")))

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

pairs <- ggpairs(mtcars, axisLabels = "internal", colour = "steelblue")

fit_am <- lm(mpg ~ am, data = mtcars)

fit_amResid <- fit_am$resid


fit_all <- lm(mpg ~ ., data = mtcars)


fit_refined <- lm(mpg ~ wt+am, data = mtcars)
