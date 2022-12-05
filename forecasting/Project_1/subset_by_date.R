subset_by_date <- function(df = NULL, startTime = NULL, endTime = NULL) {
  
  df_sub <- df[df$DATE >= startTime & df$DATE <= endTime,]
  
  return(df_sub) 
}