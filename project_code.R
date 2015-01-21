
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
library("MASS", quietly = T)

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
str(mtcars)

summ_simple <- mtcars %>%
                group_by(am) %>%
                summarise(mean_mpg = mean(mpg),
                         sd = sd(mpg),
                          n = n(),
                          se = sd/sqrt(n),
                          ss_n = (sd^2)/n)
summ_simple

violin <-
mtcars %>%
  ggplot(aes(x = as.factor(ifelse(am==0,"Automatic","Manual")),
             y = mpg,
             fill = as.factor(ifelse(am==0,"Automatic","Manual"))))+
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

pairs <- ggpairs(mtcars, axisLabels = "internal")

################################################################################
# Step 3 - clean up for model fit
################################################################################
mtcars$cyl <- factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)


fit_all <- lm(mpg ~ . ,data = mtcars)
summary(fit_all)

step <- stepAIC(fit_all, direction = "both")
step$anova



fitRef <- lm(mpg ~ am + wt + qsec - 1, data = mtcars)
summary(fitRef)$coef

fitRef2 <- update(fitRef, . ~ . +am*qsec)
summary(fitRef2)$coef

fitRef3 <- update(fitRef, . ~ . +am*wt)
summary(fitRef3)$coef

fitRef4 <- update(fitRef, . ~ .+ wt*am + am*qsec + wt*qsec*am)
summary(fitRef4)$coef

aov2 <- anova(fitRef, fitRef2), fitRef3, fitRef4)

aov3 <- anova(fitRef, fitRef3)
aov4 <- anova(fitRef, fitRef2)




fitRefInteract <- lm(mpg ~ am*(wt + qsec), data = mtcars)
summary(fitRefInteract)$coef



fitRefInteractScaled <- lm(scale(mpg) ~ scale(am)*(scale(wt) + scale(qsec)), data = mtcars)
summary(fitRefInteractScaled)$coef



mtcars %>%
  ggplot(aes(x = wt, y = mpg, colour = factor(am)))+
  geom_point()+
  facet_grid(.~am)


aov <- anova(fitRef, fitRefInteract)
aov



confint(fitRefInteract)

fit <- lm(mpg ~ am, data = mtcars)
confint(fit)

fitRefRes <- fortify(fitRef)


# resid vs. fitted
residFit <- ggplot(fitRefRes, aes(.fitted, .resid)) +
  geom_point(colour = "steelblue")  +
  geom_smooth(method = "loess", se=FALSE, colour = "firebrick") +
  geom_hline(linetype=2, size=.2) +
  scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") +
  ggtitle("Residuals vs Fitted")+
  theme_minimal()

# normal Q-Q
a <- quantile(fitRefRes$.stdresid, c(0.25, 0.75))
b <- qnorm(c(0.25, 0.75))
slope <- diff(a)/diff(b)
int <- a[1] - slope * b[1]
qq <- ggplot(fitRefRes, aes(sample=.resid)) +
  stat_qq(colour = "steelblue") +
  geom_abline(slope=slope, intercept=int, colour = "firebrick") +
  scale_x_continuous("Theoretical Quantiles") +
  scale_y_continuous("Standardized Residuals") +
  theme_minimal()+
  ggtitle("Normal Q-Q")


# Scale -location (stanardised resid vs. fitted)
standFit <- ggplot(fitRefRes, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point(colour = "steelblue") +
  geom_smooth(se=FALSE, method = "loess", colour = "firebrick") +
  scale_x_continuous("Fitted Values") +
  scale_y_continuous("Root of Standardized Residuals") +
  theme_minimal()+
  ggtitle("Scale-Location")

# residuals vs. leverage
residLev <- ggplot(fitRefRes, aes(.hat, .stdresid)) +
  geom_point(colour = "steelblue") +
  geom_smooth(se=FALSE, colour = "firebrick", method = "loess") +
  geom_hline(linetype=2, size=.2) +
  scale_x_continuous("Leverage") +
  scale_y_continuous("Standardized Residuals")+
  ggtitle("Residuals vs. Leverage")+
  theme_minimal()
