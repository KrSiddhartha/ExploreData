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
