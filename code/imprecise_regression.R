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
ggplot(df, aes(x, y)) + 
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
  
# Second approach: 2 sides slope compression 
slope <- reg_model$coefficients[['x']]
intercept <- reg_model$coefficients[['(Intercept)']]
err_top <- max(df$y - predict(reg_model, df))
err_bottom <- max(predict(reg_model, df) - df$y)

ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
  geom_smooth(method='lm', level=0.95) + 
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
  geom_abline(intercept=intercept - err_bottom, slope=slope, col='red') +
  geom_abline(intercept=intercept + err_top, slope=slope, col='red') +
  geom_ribbon(aes(ymin=slope*x + intercept - err_bottom, ymax=slope*x + intercept + err_top), 
              fill='yellow', alpha=0.25) +
  theme_bw()

# CI approach
confint(reg_model)

intercept_min <- confint(reg_model)['(Intercept)', 1]
intercept_max <- confint(reg_model)['(Intercept)', 2]
slope_min <- confint(reg_model)['x', 1]
slope_max <- confint(reg_model)['x', 2]
delta_ci <- 0.5

ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
  geom_smooth(method='lm', level=0.95) + 
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
  geom_abline(intercept=intercept_min, slope=slope_min, col='red') +
  geom_abline(intercept=intercept_max, slope=slope_max, col='red') +
  geom_ribbon(aes(ymin=intercept_min + x*slope_min, ymax=intercept_max + x*slope_max),
              fill='yellow', alpha=0.25) +
  theme_bw()

# Approach: 2 sides slope compression + CI
ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
  geom_smooth(method='lm', level=0.95) + 
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
  geom_abline(intercept=intercept - err_bottom, slope=slope, col='red') +
  geom_abline(intercept=intercept + delta_ci*err_top, slope=slope, col='purple') +
  geom_abline(intercept=intercept - delta_ci*err_bottom, slope=slope, col='purple') +
  geom_abline(intercept=intercept + err_top, slope=slope, col='red') +
  geom_ribbon(aes(ymin=slope*x + intercept - err_bottom, ymax=slope*x + intercept + err_top), 
              fill='yellow', alpha=0.25) +
  geom_ribbon(aes(ymin=slope*x + intercept - delta_ci*err_bottom, 
                  ymax=slope*x + intercept + delta_ci*err_top), 
              fill='cadetblue1', alpha=0.25) +
  theme_bw()

# MAIN FUNCTION
imprecise_regression <- function(x, y, confidence=0.95, do_plot=F){
  dataframe <- data.frame(x, y)
  reg_model <- lm(y~x, dataframe)
  
  slope <- reg_model$coefficients[['x']]
  intercept <- reg_model$coefficients[['(Intercept)']]
  err_top <- max(y - predict(reg_model, dataframe))
  err_bottom <- max(predict(reg_model, dataframe) - y)
  
  deltas <- seq(from=0, to=1, by=0.01)
  misclassified_proportion <- c()
  for (delta in deltas){
    n_misclassified <- nrow(dataframe[y > x * slope + intercept + delta*err_top | 
                                        y < x * slope + intercept - delta*err_bottom, ])
    misclassified_proportion <- c(misclassified_proportion, n_misclassified/nrow(dataframe))
  }
  names(misclassified_proportion) <- deltas
  misclassified_proportion <- misclassified_proportion[order(misclassified_proportion)]
  alpha <- 1 - confidence # = risk level -> confidence level = 1 - alpha
  delta_ci <- as.numeric(names(tail(misclassified_proportion[misclassified_proportion < alpha], 1)))
  
  if (do_plot){
    p <- ggplot(dataframe, aes(x, y)) + 
      geom_point() +
      geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
      geom_smooth(method='lm', level=0.95) + 
      ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
      geom_abline(intercept=intercept - err_bottom, slope=slope, col='red') +
      geom_abline(intercept=intercept + delta_ci*err_top, slope=slope, col='purple') +
      geom_abline(intercept=intercept - delta_ci*err_bottom, slope=slope, col='purple') +
      geom_abline(intercept=intercept + err_top, slope=slope, col='red') +
      geom_ribbon(aes(ymin=slope*x + intercept - err_bottom, ymax=slope*x + intercept + err_top), 
                  fill='yellow', alpha=0.25) +
      geom_ribbon(aes(ymin=slope*x + intercept - delta_ci*err_bottom, 
                      ymax=slope*x + intercept + delta_ci*err_top), 
                  fill='cadetblue1', alpha=0.25) +
      theme_bw()
  } else {
    p <- NULL
  }
  
  result <- list(slope=slope, intercept=intercept, 
                 intercept_min=intercept-err_bottom*delta_ci, 
                 intercept_max=intercept+err_top*delta_ci, 
                 plot=p)
  return(result)
}

n_run <- 10
confidence <- 0.65
models <- c("global_ci", "local_ci", "global_freq_ci", "tree", "forest")
av_coverage_global_ci <- c(); av_coverage_local_ci <- c()
av_coverage_global_freq_ci <- c(); av_coverage_tree <- c()
av_coverage_forest <- c()
av_ci_width_global_ci <- c(); av_ci_width_local_ci <- c()
av_ci_width_global_freq_ci <- c(); av_ci_width_tree <- c()
av_ci_width_forest <- c()
for(irun in 1 : n_run){
  cat(irun, " ")
  coverage_global_ci <- c(); coverage_local_ci <- c()
  coverage_global_freq_ci <- c(); coverage_tree <- c()
  coverage_forest <- c()
  ci_width_global_ci <- c(); ci_width_local_ci <- c()
  ci_width_global_freq_ci <- c(); ci_width_tree <- c()
  ci_width_forest <- c()
  
  df <- df[sample(nrow(df)), ]
  df_train <- df[1:100, ]
  df_test <- df[101:150, ]
  
  reg_model <- lm(y~x, df_train)
  impr_reg_model <- imprecise_regression(df_train$x, df_train$y, confidence=confidence, do_plot=F)
  tree <- rpart(y~x, df_train)
  forest <- randomForest(y~x, df_train, ntree=10, keep.forest=T)
    
  slope <- reg_model$coefficients[['x']]
  intercept <- reg_model$coefficients[['(Intercept)']]
  
  # Standard CI
  intercept_min <- confint(reg_model)['(Intercept)', 1]
  intercept_max <- confint(reg_model)['(Intercept)', 2]
  slope_min <- confint(reg_model)['x', 1]
  slope_max <- confint(reg_model)['x', 2]
  
  # Empirical CI
  emp_intercept_min <- impr_reg_model$intercept_min
  emp_intercept_max <- impr_reg_model$intercept_max

  for (i in 1 : nrow(df_test)){
    x_i <- df_test[i, 'x']
    y_i <- df_test[i, 'y']
    
    standard_ci <- c(min(x_i * slope_min + intercept_min,
                         x_i * slope_max + intercept_min,
                         x_i * slope_min + intercept_max,
                         x_i * slope_max + intercept_max),
                     max(x_i * slope_min + intercept_min,
                         x_i * slope_max + intercept_min,
                         x_i * slope_min + intercept_max,
                         x_i * slope_max + intercept_max))
    empirical_ci <- c(min(x_i * slope + emp_intercept_min,
                          x_i * slope + emp_intercept_max),
                      max(x_i * slope + emp_intercept_min,
                          x_i * slope + emp_intercept_max))
    
    # Local CI
    local_ci <- predict(reg_model, df_test[i, ], interval="prediction", level=confidence)[2:3]
    
    # Forest CI
    forest_sd <- sqrt(mean(forest$mse)/(1-mean(forest$rsq)))
    forest_ci <- c(predict(forest, df_test[i, ]) - forest_sd, 
                   predict(forest, df_test[i, ]) + forest_sd)
    
    if (y_i >= standard_ci[1] & y_i <= standard_ci[2]){
      coverage_global_ci <- c(coverage_global_ci, 1)
    } else {
      coverage_global_ci <- c(coverage_global_ci, 0)
    }
    if (y_i >= local_ci[1] & y_i <= local_ci[2]){
      coverage_local_ci <- c(coverage_local_ci, 1)
    } else {
      coverage_local_ci <- c(coverage_local_ci, 0)
    }
    if (y_i >= empirical_ci[1] & y_i <= empirical_ci[2]){
      coverage_global_freq_ci <- c(coverage_global_freq_ci, 1)
    } else {
      coverage_global_freq_ci <- c(coverage_global_freq_ci, 0)
    }
    if (y_i >= forest_ci[1] & y_i <= forest_ci[2]){
      coverage_forest <- c(coverage_forest, 1)
    } else {
      coverage_forest <- c(coverage_forest, 0)
    }
    ci_width_global_ci <- c(ci_width_global_ci, standard_ci[2] - standard_ci[1])
    ci_width_local_ci <- c(ci_width_local_ci, local_ci[2] - local_ci[1])
    ci_width_global_freq_ci <- c(ci_width_global_freq_ci, empirical_ci[2] - empirical_ci[1])
    ci_width_forest <- c(ci_width_forest, forest_ci[2] - forest_ci[1])
  }
  
  av_coverage_global_ci <- c(av_coverage_global_ci, mean(coverage_global_ci))
  av_coverage_local_ci <- c(av_coverage_local_ci, mean(coverage_local_ci))
  av_coverage_global_freq_ci <- c(av_coverage_global_freq_ci, mean(coverage_global_freq_ci))
  av_coverage_forest <- c(av_coverage_forest, mean(coverage_forest))
  av_ci_width_global_ci <- c(av_ci_width_global_ci, mean(ci_width_global_ci))
  av_ci_width_local_ci <- c(av_ci_width_local_ci, mean(ci_width_local_ci))
  av_ci_width_global_freq_ci <- c(av_ci_width_global_freq_ci, mean(ci_width_global_freq_ci))
  av_ci_width_forest <- c(av_ci_width_forest, mean(ci_width_forest))
}
round(
  rbind(coverage=colMeans(data.frame(global=av_coverage_global_ci, local=av_coverage_local_ci,
                                     global_freq=av_coverage_global_freq_ci, forest=av_coverage_forest)),
        width=colMeans(data.frame(global=av_ci_width_global_ci, local=av_ci_width_local_ci,
                                  global_freq=av_ci_width_global_freq_ci, forest=av_ci_width_forest))), 3)
#par(mfrow=c(1,2))
#boxplot(data.frame(global=av_coverage_global_ci,
#                   local=av_coverage_local_ci,
#                   global_freq=av_coverage_global_freq_ci,
#                   forest=av_coverage_forest), ylab='coverage')
#boxplot(data.frame(global=av_ci_width_global_ci,
#                   local=av_ci_width_local_ci,
#                   global_freq=av_ci_width_global_freq_ci,
#                   forest=av_ci_width_forest), ylab='interval widths')

ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_abline(intercept=reg_model$coefficients[1], slope=reg_model$coefficients[2]) +
  geom_smooth(method='lm', level=0.95) + 
  ggtitle(paste0("R2 = ", round(summary(reg_model)$adj.r.squared, 2))) +
  geom_abline(intercept=intercept - err_bottom, slope=slope, col='red') +
  geom_abline(intercept=intercept + delta_ci*err_top, slope=slope, col='purple') +
  geom_abline(intercept=intercept - delta_ci*err_bottom, slope=slope, col='purple') +
  geom_abline(intercept=intercept + err_top, slope=slope, col='red') +
  geom_ribbon(aes(ymin=slope*x + intercept - err_bottom, ymax=slope*x + intercept + err_top), 
              fill='yellow', alpha=0.25) +
  geom_ribbon(aes(ymin=slope*x + intercept - delta_ci*err_bottom, 
                  ymax=slope*x + intercept + delta_ci*err_top), 
              fill='cadetblue1', alpha=0.25) +
  theme_bw()




