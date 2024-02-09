# Possibilistic linear regression

The idea of this approch = model linear regression outputs uncertainty with possibilist distributions

# Code example
<pre>
  <code>
library(devtools)
source_url("https://raw.githubusercontent.com/sutton-charani/possibilistic_linear_regression_regression/main/code/my_lib_possibilistic_linear_regression.R")

# Data definition
df <- iris[c("Petal.Length", "Petal.Width")] # "Petal.Length", "Sepal.Length"
names(df) <- c("y", "x")

# Soft regression
soft_lm <- possibilistic_linear_regression(x=df$x, y=df$y, do_plot=T)
soft_lm

soft_lm <- possibilistic_linear_regression(x=df$x, y=df$y, do_plot=T, 
                                           confidences=seq(from=0.5, to=0.99, length.out=4))
soft_lm
  </code>
</pre>
