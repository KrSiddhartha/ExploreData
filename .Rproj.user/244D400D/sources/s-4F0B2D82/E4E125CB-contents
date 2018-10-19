missing_unique_plot <- function(missing, unique, variables){
  p1 <- plot_ly(x = ~missing, y = ~reorder(variables, missing),
                name = 'Missing Values',
                type = 'bar', orientation = 'h',
                marker = list(color = 'rgba(50, 171, 96, 0.6)', line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
    layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
           xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE))

  p2 <- plot_ly(x = ~unique, y = ~reorder(variables, unique),
                name = 'Distinct Values',
                type = 'bar', orientation = 'h',
                marker = list(color = 'rgb(128, 0, 128)', line = list(color = 'rgba(102, 102, 102, 0.8', width = 1))) %>%
    layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
           xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE))

  p <- subplot(p1, p2) %>%
    layout(title = 'Data Quality as per Missing Values and Distinct Values',
           legend = list(x = 0.029, y = 1.038,
                         font = list(size = 10)),
           margin = list(l = 100, r = 20, t = 70, b = 70),
           paper_bgcolor = 'rgb(248, 248, 255)',
           plot_bgcolor = 'rgb(248, 248, 255)') %>%
    add_annotations(xref = 'paper', yref = 'paper',
                    x = -0.14, y = -0.15,
                    text = paste('Visual Representation for Data Quality'),
                    font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                    showarrow = FALSE)
  p
}
