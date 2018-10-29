explore_univar <- function(df, var){
  df2var2 <- df[[var]]
  univar_measures <- function(var_to){
    var_to <- hp$GrLivArea
    require(EnvStats)
    Mean <- mean(var_to, na.rm = T)
    Mean[5] <- NA
    Median <- median(var_to, na.rm = T)
    Median[5] <- NA
    Range <- range(var_to, na.rm = T)
    Range[5] <- NA
    Quantile <- quantile(var_to, na.rm = T)
    IQR <- IQR(var_to, na.rm = T)
    IQR[5] <- NA
    Variance <- var(var_to, na.rm = T)
    Variance[5] <- NA
    Standard_Deviation <- sd(var_to, na.rm = T)
    Standard_Deviation[5] <- NA
    Skewness  <- skewness(var_to, na.rm = T)
    Skewness[5] <- NA
    Kurtosis <- kurtosis(var_to, na.rm = T)
    Kurtosis[5] <- NA
    disp_df <- as.data.frame(matrix(nrow = 9, ncol = 5))
    disp_df[1,] <- Mean
    disp_df[2,] <- Median
    disp_df[3,] <- Range
    disp_df[4,] <- Quantile
    disp_df[5,] <- IQR
    disp_df[6,] <- Variance
    disp_df[7,] <- Standard_Deviation
    disp_df[8,] <- Skewness
    disp_df[9,] <- Kurtosis
    disp_df$Measure <- c("Mean", "Median", "Range", "Quartile", "IQR", "Variance", "Standard Deviation", "Skewness", "Kurtosis")
    disp_df <- subset.data.frame(disp_df, select = c(Measure, V1:V5))
    disp_df$V1 <- round(disp_df$V1, 2)
    disp_df
  }
  univar_plot <- function(df, var){
    require(plotly)
    var_loc <- which(names(df) == var)
    suppressMessages(p1 <- plot_ly(data = df, y = ~df[,var_loc], type = "box", boxpoints = "all", jitter = 0.3,
                                   pointpos = -1.8, name = " "))
    
    suppressMessages(p2 <- ggplotly(ggplot(df, aes(df[,var_loc])) + 
                                      geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "tan4") + 
                                      geom_density(fill = "#ff4d4d", alpha = 0.3)))
    
    suppressMessages(p <- subplot(p1, p2) %>% layout(title = paste('Data Distribution for ', var, sep = "")))
    suppressMessages(p)
  }
  univar_measures_out <- univar_measures(df2var2)
  univar_plot_out <- univar_plot(df, var)
  explore_univar_out <- list(univar_Table = univar_measures_out, univar_plot = univar_plot_out)
  return(explore_univar_out)
}
