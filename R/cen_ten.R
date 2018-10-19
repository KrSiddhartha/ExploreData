central_tendency <- function(var_to){
  require(EnvStats)
  data.frame(Mean = mean(var_to, na.rm = T),
             Median = median(var_to, na.rm = T),
             Max = max(var_to, na.rm = T),
             Min = min(var_to, na.rm = T))
}
