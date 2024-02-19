library(ggplot2); library(wrMisc)

################################
empirical_conf_int <- function(x, y, confidence=0.95, do_plot=F){
  dataframe <- data.frame(x=x, y=y)
  reg_model <- lm(y~x, dataframe)
  
  slope <- reg_model$coefficients[['x']]
  intercept <- reg_model$coefficients[['(Intercept)']]
  err_top <- max(y - predict(reg_model, dataframe))
  err_bottom <- max(predict(reg_model, dataframe) - y)
  
  dataframe$d <- abs(dataframe$y - slope*dataframe$x- intercept)
  dataframe <- dataframe[order(dataframe$d, decreasing=F), ]
  irow_max <- floor(confidence * nrow(dataframe))
  df_conf <- dataframe[1 : irow_max, ]
  reg_model_conf <- lm(y~x, df_conf)
  slope_conf <- reg_model_conf$coefficients[['x']]
  intercept_conf <- reg_model_conf$coefficients[['(Intercept)']]
  
  intercept_min <- intercept_conf - max(df_conf$d)
  intercept_max <- intercept_conf + max(df_conf$d)
  slope_min <- (slope_conf*(max(df_conf$x) - min(df_conf$x)) + intercept_min - intercept_max) / 
    (max(df_conf$x) - min(df_conf$x))
  slope_max <- (slope_conf*(min(df_conf$x) - max(df_conf$x)) + intercept_min - intercept_max) / 
    (min(df_conf$x) - max(df_conf$x))
  
  if (do_plot){
    p <- ggplot(dataframe, aes(x, y)) + 
      geom_point() +
      geom_abline(intercept=reg_model_conf$coefficients[1], slope=reg_model_conf$coefficients[2], color='red') +
      ggtitle(paste0("Evidential band \nfor a confidence of ", confidence)) +
      geom_segment(aes(x=min(dataframe$x), y=slope_conf*min(dataframe$x)+intercept_min,
                       xend=min(dataframe$x), yend=slope_conf*min(dataframe$x)+intercept_max),
                   linetype = "dashed", col='purple') +
      geom_segment(aes(x=max(dataframe$x), y=slope_conf*max(dataframe$x)+intercept_min,
                       xend=max(dataframe$x), yend=slope_conf*max(dataframe$x)+intercept_max),
                   linetype = "dashed", col='purple') +
      geom_segment(aes(x=min(dataframe$x), y=slope_conf*min(dataframe$x)+intercept_max,
                       xend=max(dataframe$x), yend=slope_conf*max(dataframe$x)+intercept_max),
                   linetype = "dashed", col='purple') +
      geom_segment(aes(x=min(dataframe$x), y=slope_min*min(dataframe$x)+slope_conf*min(dataframe$x)+intercept_max-slope_min*min(dataframe$x),
                       xend=max(dataframe$x), yend=slope_conf*max(dataframe$x)+intercept_min),
                   col='blue') +
      geom_segment(aes(x=min(dataframe$x), y=slope_conf*min(dataframe$x)+intercept_min,
                       xend=max(dataframe$x), yend=slope_max*max(dataframe$x)+slope_conf*max(dataframe$x)+intercept_max-slope_max*max(dataframe$x)),
                   col='blue') +
      geom_segment(aes(x=min(dataframe$x), y=slope_conf*min(dataframe$x)+intercept_min,
                       xend=max(dataframe$x), yend=slope_conf*max(dataframe$x)+intercept_min),
                   linetype = "dashed", col='purple') +
      geom_ribbon(aes(ymin=slope_conf*x + intercept_min,
                      ymax=slope_conf*x + intercept_max),
                  fill='cadetblue1', alpha=0.25) +
      theme_bw() +
      theme(text = element_text(size = 30), plot.title = element_text(hjust = 0.5))
  } else {
    p <- NULL
  }
  
  
  result <- list(intercept=intercept_conf, intercept_min=intercept_min,intercept_max=intercept_max, 
                 slope=slope_conf, slope_min=slope_min,slope_max=slope_max, plot=p)
  return(result)
}
################################
possibilistic_linear_regression <- function(x, y, confidences=c(0.5, 0.75, 0.95), do_plot=F, size=1){
  
  precise_reg_model <- lm(y~x, data.frame(x=x, y=y))
  precise_slope <- precise_reg_model$coefficients[['x']]
  precise_intercept <- precise_reg_model$coefficients[['(Intercept)']]
  
  intercept_interval <- data.frame()
  slope_interval <- data.frame()
  for (confidence in confidences){
    soft_regression_intervals <- empirical_conf_int(x, y, confidence=confidence, do_plot=F)
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
      geom_abline(intercept=intercept, slope=slope, col="red", linewidth=size) +
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
  
  result <- list(precise_slope=precise_slope, precise_intercept=precise_intercept, 
                 slope_possibility=slope_interval, intercept_possibility=intercept_interval, 
                 plot=p)
  return(result)
}
################################











###############