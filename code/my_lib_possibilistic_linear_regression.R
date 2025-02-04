library(ggplot2); library(wrMisc)
################################
geom_evid_band <- function(intercept_conf, intercept_min, intercept_max, slope_conf, slope_min, slope_max, dataframe, 
                           band_slope=T, band_diags=T, band_border_type="dashed", alpha=0.25){
  band_color <- "black"
  x <- dataframe$x
  if (band_slope & band_diags){
    list(geom_abline(intercept=intercept_conf, slope=slope_conf, color='red'),
         geom_segment(aes(x=min(x), 
                          y=slope_min*min(x)+slope_conf*min(x)+intercept_max-slope_min*min(x),
                          xend=max(x), 
                          yend=slope_conf*max(x)+intercept_min),
                      col='blue'),
         geom_segment(aes(x=min(x), 
                          y=slope_conf*min(x)+intercept_min,
                          xend=max(x), 
                          yend=slope_max*max(x)+slope_conf*max(x)+intercept_max-slope_max*max(x)),
                      col='blue'),
         geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                          xend=min(x), yend=slope_conf*min(x)+intercept_max),
                      linetype = band_border_type, col='purple'),
         geom_segment(aes(x=max(x), y=slope_conf*max(x)+intercept_min,
                          xend=max(x), yend=slope_conf*max(x)+intercept_max),
                      linetype = band_border_type, col='purple'),
         geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_max,
                          xend=max(x), yend=slope_conf*max(x)+intercept_max),
                      linetype = band_border_type, col='purple'),
         geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                          xend=max(x), yend=slope_conf*max(x)+intercept_min),
                      linetype = band_border_type, col='purple'),
         geom_ribbon(aes(ymin=slope_conf*x + intercept_min, ymax=slope_conf*x + intercept_max),
                     fill=band_color, alpha=alpha))
  } else {
    if (band_slope & !band_diags){
      list(geom_abline(intercept=intercept_conf, slope=slope_conf, color='red'),
           geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                            xend=min(x), yend=slope_conf*min(x)+intercept_max),
                        linetype = band_border_type, col='purple'),
           geom_segment(aes(x=max(x), y=slope_conf*max(x)+intercept_min,
                            xend=max(x), yend=slope_conf*max(x)+intercept_max),
                        linetype = band_border_type, col='purple'),
           geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_max,
                            xend=max(x), yend=slope_conf*max(x)+intercept_max),
                        linetype = band_border_type, col='purple'),
           geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                            xend=max(x), yend=slope_conf*max(x)+intercept_min),
                        linetype = band_border_type, col='purple'),
           geom_ribbon(aes(ymin=slope_conf*x + intercept_min, ymax=slope_conf*x + intercept_max),
                       fill=band_color, alpha=alpha))
    } else {
      if (!band_slope & band_diags){
        list(geom_segment(aes(x=min(x), 
                              y=slope_min*min(x)+slope_conf*min(x)+intercept_max-slope_min*min(x),
                              xend=max(x), 
                              yend=slope_conf*max(x)+intercept_min),
                          col='blue'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                              xend=max(x), yend=slope_max*max(x)+slope_conf*max(x)+intercept_max-slope_max*max(x)),
                          col='blue'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                              xend=min(x), yend=slope_conf*min(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=max(x), y=slope_conf*max(x)+intercept_min,
                              xend=max(x), yend=slope_conf*max(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_max,
                              xend=max(x), yend=slope_conf*max(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                              xend=max(x), yend=slope_conf*max(x)+intercept_min),
                          linetype = band_border_type, col='purple'),
             geom_ribbon(aes(ymin=slope_conf*x + intercept_min, ymax=slope_conf*x + intercept_max),
                         fill=band_color, alpha=alpha))
      } else { # case (!slope & !diags)
        list(geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                              xend=min(x), yend=slope_conf*min(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=max(x), y=slope_conf*max(x)+intercept_min,
                              xend=max(x), yend=slope_conf*max(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_max,
                              xend=max(x), yend=slope_conf*max(x)+intercept_max),
                          linetype = band_border_type, col='purple'),
             geom_segment(aes(x=min(x), y=slope_conf*min(x)+intercept_min,
                              xend=max(x), yend=slope_conf*max()+intercept_min),
                          linetype = band_border_type, col='purple'),
             geom_ribbon(aes(ymin=slope_conf*x + intercept_min, ymax=slope_conf*x + intercept_max),
                         fill=band_color, alpha=alpha))
      }
    }
  }
} 
################################
empirical_conf_int <- function(x, y, confidence=0.95, do_plot=F, band_slope=T, band_diags=T, band_border_type="dashed", size=25){
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
      ggtitle(paste0("Evidential band \nfor a belief degree of ", confidence)) +
      theme_bw() +
      theme(text = element_text(size = size), plot.title = element_text(hjust = 0.5)) +
      geom_evid_band(intercept_conf, intercept_min, intercept_max, slope_conf, slope_min, slope_max, dataframe, 
                     band_slope=band_slope, band_diags=band_diags, band_border_type=band_border_type)
  } else {
    p <- NULL
  }
  
  
  result <- list(intercept=intercept_conf, intercept_min=intercept_min,intercept_max=intercept_max, 
                 slope=slope_conf, slope_min=slope_min,slope_max=slope_max, plot=p)
  return(result)
}
################################
possibilistic_linear_regression <- function(x, y, initial_confidences=c(0.5, 0.75, 0.95), do_plot=F, size=25){
  
  precise_reg_model <- lm(y~x, data.frame(x=x, y=y))
  precise_slope <- precise_reg_model$coefficients[['x']]
  precise_intercept <- precise_reg_model$coefficients[['(Intercept)']]
  
  intercept_interval <- data.frame()
  slope_interval <- data.frame()
  slope_confs <- c()
  intercept_confs <- c()
  for (confidence in initial_confidences){
    soft_regression_intervals <- empirical_conf_int(x, y, confidence=confidence, do_plot=F)
    intercept_interval <- rbind(intercept_interval, unlist(soft_regression_intervals[c('intercept_min', 'intercept_max')]))
    slope_interval <- rbind(slope_interval, unlist(soft_regression_intervals[c('slope_min', 'slope_max')]))
    slope_confs <- c(slope_confs, soft_regression_intervals$slope)
    intercept_confs <- c(intercept_confs, soft_regression_intervals$intercept)
  }
  
  intercept_interval <- rbind(intercept_interval, c(-Inf, +Inf))
  slope_interval <- rbind(slope_interval, c(-Inf, +Inf))
  mass_omega <- 1/(length(x) - 1) # Shafer uncertainty model
  confidences <- initial_confidences/(sum(initial_confidences) + mass_omega)
  names(intercept_interval) <- c("intercept_min", "intercept_max")
  names(slope_interval) <- c("slope_min", "slope_max")
  intercept_interval$mass <- c(confidences, mass_omega)
  slope_interval$mass <- c(confidences, mass_omega)
  
  if (do_plot){
    dataframe <- data.frame(x=x, y=y)
    reg_model <- lm(y~x, dataframe)
    slope <- reg_model$coefficients[['x']]
    intercept <- reg_model$coefficients[['(Intercept)']]
    
    if (length(initial_confidences) < 6){
      plot_title <- paste("Possibilistic regression for\nbelief degrees =", 
                     paste(initial_confidences, collapse=", "))
    } else {
      plot_title <- paste("Possibilistic regression for\nbelief degrees =", 
                          initial_confidences[1], ",", initial_confidences[2],
                          ", ... ,", initial_confidences[length(initial_confidences) - 1], ", ",
                          initial_confidences[length(initial_confidences)])
    }
    p <- ggplot(dataframe, aes(x, y)) + 
      ggtitle(plot_title) +
      theme_bw() + xlab("Petal.Length") + ylab("Sepal.Width") +
      theme(text = element_text(size = size),
            plot.title = element_text(hjust = 0.5))
    for (i in seq(from=length(initial_confidences), to=1, by=-1)){
      command_line <- paste0("p <- p + geom_evid_band(intercept_confs[", i,
                             "], intercept_interval$intercept_min[", i,
                             "], intercept_interval$intercept_max[", i,
                             "], slope_confs[", i,
                             "], slope_interval$slope_min[", i,
                             "], slope_interval$slope_max[", i,
                             "], dataframe, band_slope=F, band_diags=F, band_border_type=NA, alpha=confidences[", i,
                             "])")
      eval(parse(text=command_line))
    }
    p <- p + geom_point() +
      geom_abline(intercept=intercept, slope=slope, col="red", linewidth=1)
    
  } else {
    p <- NULL
  }
  
  result <- list(precise_slope=precise_slope, precise_intercept=precise_intercept, 
                 slope_possibility=slope_interval, intercept_possibility=intercept_interval, 
                 slope_confs=slope_confs,
                 plot=p)
  return(result)
}
################################











