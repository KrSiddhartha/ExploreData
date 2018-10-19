central_tendency <- function(var_to){
  require(EnvStats)
  data.frame(Mean = mean(var_to, na.rm = T),
             Median = median(var_to, na.rm = T),
             Max = max(var_to, na.rm = T),
             Min = min(var_to, na.rm = T))
}
dispersion <- function(var_to){
  Range = range(var_to, na.rm = T)
  Range[5] <- NA
  Quantile = quantile(var_to, na.rm = T)
  IQR = IQR(var_to, na.rm = T)
  IQR[5] <- NA
  Variance = var(var_to, na.rm = T)
  Variance[5] <- NA
  Standard_Deviation = sd(var_to, na.rm = T)
  Standard_Deviation[5] <- NA
  Skewness  = skewness(var_to, na.rm = T)
  Skewness[5] <- NA
  Kurtosis = kurtosis(var_to, na.rm = T)
  Kurtosis[5] <- NA
  disp_df <- as.data.frame(matrix(nrow = 7, ncol = 5))
  disp_df[1,] <- Range
  disp_df[2,] <- Quantile
  disp_df[3,] <- IQR
  disp_df[4,] <- Variance
  disp_df[5,] <- Standard_Deviation
  disp_df[6,] <- Skewness
  disp_df[7,] <- Kurtosis
  disp_df$Measure <- c("Range", "Quartile", "IQR", "Variance", "Standard Deviation", "Skewness", "Kurtosis")
  disp_df <- subset.data.frame(disp_df, select = c(Measure, V1:V5))
  disp_df
}
univariate_plot <- function(df, var){
  require(plotly)
  var_loc <- which(names(df) == var)
  p1 <- plot_ly(data = df, y = ~df[,var_loc], type = "box", boxpoints = "all", jitter = 0.3,
                pointpos = -1.8, name = " ")
  p2 <- ggplotly(ggplot(df, aes(df[,var_loc])) + 
                   geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "tan4") + 
                   geom_density(fill = "#ff4d4d", alpha = 0.3))
  
  p <- subplot(p1, p2) %>% layout(title = paste('Data Distribution for ', var, sep = ""))
  p
}
