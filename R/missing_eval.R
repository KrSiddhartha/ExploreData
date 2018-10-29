missing_eval <- function(df){
  missing_vars <- as.data.frame(apply(df, 2, function(z) sum(is.na(z))))
  missing_vars$Variables <- rownames(missing_vars)
  missing_vars$Col_Pos <- seq(1,ncol(df), 1)
  names(missing_vars) <- c("Missing", "Variables", "Col_Pos")
  missing_vars <- subset.data.frame(missing_vars, select = c(Col_Pos, Variables, Missing))
  missing_vars$Prop_Missing <- round((missing_vars$Missing/nrow(df))*100,2)
  missing_vars$Unique_Values <- apply(df, 2, function(x) length(unique(x[is.na(x) == F])))
  missing_vars <- missing_vars[with(missing_vars, order(Missing, decreasing = T)), ]
  rownames(missing_vars) <- NULL

# Plot --------------------------------------------------------------------
  require(ggplot2)
  require(plotly)
  require(reshape2)
  missing_adj <- missing_vars
  missing_adj$Col_Pos <- NULL 
  missing_adj$Prop_Missing <- NULL
  suppressMessages(missing_adj <- melt(missing_adj))
  missing_adj_miss <- missing_adj[missing_adj$variable == "Missing",]
  missing_adj_unq <- missing_adj[missing_adj$variable == "Unique_Values",]
  missing_adj <- rbind(missing_adj_miss, missing_adj_unq)
  missing_adj$value[missing_adj$variable=="Unique_Values"] <- -missing_adj$value[missing_adj$variable=="Unique_Values"]
  plt <- ggplotly(ggplot(missing_adj, aes(x=reorder(Variables, value), y=value, fill=variable)) + 
                    geom_bar(stat="identity", position="identity") + 
                    xlab("Variables") +
                    ylab("Observations") +
                    scale_y_continuous(breaks=seq(min(missing_adj$value),max(missing_adj$value),by=200),
                                       labels=paste(abs(seq(min(missing_adj$value),max(missing_adj$value),by=200)), sep = "")) +
                    coord_flip() +
                    scale_fill_discrete(name = ""))
  
  missing_out <- list(Missing_Unique_Table = missing_vars, Plot = plt)
  
  return(missing_out)
}

