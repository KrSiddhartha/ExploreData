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
  return(missing_vars)
}
