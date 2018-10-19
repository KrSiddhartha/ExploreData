cor2Pval <- function(cor_val,n){
  t_val <- cor_val/sqrt((1-(cor_val^2))/(n-2))
  p_val <- 2*pt(t_val, n-2, lower.tail = F)
  # p_val <- round(p_val, 10)
  return(p_val)
}
