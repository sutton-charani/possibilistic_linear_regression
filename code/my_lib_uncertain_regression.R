library(ggplot2); library(wrMisc)

################################
empirical_conf_int <- function(x, y, confidence=0.95){
  dataframe <- data.frame(x=x, y=y)
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
  
  intercept_min <- intercept-err_bottom*delta_ci
  intercept_max <- intercept+err_top*delta_ci
  slope_min <- (intercept_max - intercept_min + slope*(min(x) - max(x))) / (min(x) - max(x))
  slope_max <- (intercept_min - intercept_max + slope*(min(x) - max(x))) / (min(x) - max(x))
  
  result <- list(intercept_min=intercept_min,intercept_max=intercept_max, 
                 slope_min=slope_min,slope_max=slope_max)
  return(result)
}
################################
imprecise_regression <- function(x, y, confidences=c(0.7, 0.8, 0.95), do_plot=F, size=1){
  intercept_interval <- data.frame()
  slope_interval <- data.frame()
  for (confidence in confidences){
    soft_regression_intervals <- empirical_conf_int(x, y, confidence=confidence)
    intercept_interval <- rbind(intercept_interval, unlist(soft_regression_intervals[c('intercept_min', 'intercept_max')]))
    slope_interval <- rbind(slope_interval, unlist(soft_regression_intervals[c('slope_min', 'slope_max')]))
  }
  
  intercept_interval <- rbind(intercept_interval, c(-Inf, +Inf))
  slope_interval <- rbind(slope_interval, c(-Inf, +Inf))
  mass_omega <- 1/(length(x) - 1) # Shafer uncertainty model
  confidences <- confidences/(sum(confidences) + mass_omega)
  names(intercept_interval) <- c("intercept_min", "intercept_max")
  names(slope_interval) <- c("slope_min", "slope_max")
  intercept_interval$mass <- c(confidences, mass_omega)
  slope_interval$mass <- c(confidences, mass_omega)
  
  if (do_plot){
    dataframe <- data.frame(x=x, y=y)
    reg_model <- lm(y~x, dataframe)
    slope <- reg_model$coefficients[['x']]
    intercept <- reg_model$coefficients[['(Intercept)']]
    err_top <- max(dataframe$y - predict(reg_model, dataframe))
    err_bottom <- max(predict(reg_model, dataframe) - dataframe$y)
    p <- ggplot(dataframe, aes(x, y)) + 
      geom_point() +
      geom_abline(intercept=intercept, slope=slope, col="red", size=size, xmin=0) +
      geom_ribbon(aes(ymin=slope*x + intercept_interval$intercept_min[1], ymax=slope*x + intercept_interval$intercept_max[1]), 
                  fill=colorAccording2(intercept_interval$mass, gradTy='gray.colorsW')[1], alpha=0.25, show.legend = F) +
      geom_ribbon(aes(ymin=slope*x + intercept_interval$intercept_min[2], ymax=slope*x + intercept_interval$intercept_max[2]),
                  fill=colorAccording2(intercept_interval$mass, gradTy='gray.colorsW')[2], alpha=0.25, show.legend = F) +
      geom_ribbon(aes(ymin=slope*x + intercept_interval$intercept_min[3], ymax=slope*x + intercept_interval$intercept_max[3]),
                  fill=colorAccording2(intercept_interval$mass, gradTy='gray.colorsW')[3], alpha=0.25, show.legend = F) +
      geom_ribbon(aes(ymin=slope*x + intercept_interval$intercept_min[4], ymax=slope*x + intercept_interval$intercept_max[4]),
                  fill=colorAccording2(intercept_interval$mass, gradTy='gray.colorsW')[4], alpha=0.25, show.legend = F) +
      ggtitle("Possibilistic regression") +
      theme_bw() + xlab("Petal.Length") + ylab("Sepal.Width") +
      theme(text = element_text(size = 30),
            plot.title = element_text(hjust = 0.5)
            )
    p
  } else {
    p <- NULL
  }
  
  result <- list(precise_slope=slope, precise_intercept=intercept, 
                 slope_possibility=slope_interval, intercept_possibility=intercept_interval, 
                 plot=p)
  return(result)
}
################################











###############