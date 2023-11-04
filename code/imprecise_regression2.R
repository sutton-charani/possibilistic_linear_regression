# Variables and screen cleaning
graphics.off(); cat("\014"); rm(list=ls()) ;options(warn=-1)

library(ggplot2); library(rpart); library(rpart.plot); library(randomForest)

# Data definition
df <- iris[c("Sepal.Length", "Sepal.Width")] # "Petal.Length", "Sepal.Length"
names(df) <- c("y", "x")

# Scatterplot
ggplot(df, aes(x, y)) + 
  geom_point()

# Standard linear regression
reg_model <- lm(y~x, df)

# Standard linear regression with confidence intervals
ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
  geom_smooth(method='lm') + 
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2)))

# First approach: 4 sides compression
### Extreme points detection
yA <- min(df$y)
xA <- df$x[which.min(df$y)]
yC <- max(df$y)
xC <- df$x[which.max(df$y)]
xB <- max(df$x)
yB <- df$y[which.max(df$x)]
xD <- min(df$x)
yD <- df$y[which.min(df$x)]
extreme_pts <- data.frame(x=c(xA, xB, xC, xD), y=c(yA, yB, yC, yD))

### Plot: scatter + precise slope + confidence band + plausibility band
p <- ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2], 
              col='blue', size=2) +
  geom_smooth(method='lm') +
  geom_point(data=extreme_pts, aes(x, y), col='red') +
  geom_abline(intercept=yA-xA*(yA-yB)/(xA-xB), slope=(yA-yB)/(xA-xB), col='red') +
  geom_abline(intercept=yD-xD*(yD-yC)/(xD-xC), slope=(yD-yC)/(xD-xC), col='red') + 
  geom_ribbon(aes(ymin=(yA-yB)/(xA-xB)*x+yA-xA*(yA-yB)/(xA-xB), 
                  ymax=(yD-yC)/(xD-xC)*x + yD-xD*(yD-yC)/(xD-xC)), 
              fill='yellow', alpha=0.5) +
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
  theme_bw()

p
