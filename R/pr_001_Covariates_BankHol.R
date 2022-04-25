#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Get public holidays in Switzerland
#:::::::::::::::::::::::::::::


years <- 2015:2022
pathURL <- paste0("https://date.nager.at/api/v2/publicholidays/", years, "/CH")

gatBankHol <- function(X){
  bankHolidaysCH <- fromJSON(X)
  
  bankHolidaysCH$counties[sapply(bankHolidaysCH$counties, is.null)] <- "CH"
  
  bankHolidaysCH %>% dplyr::select(date, counties) -> bankHolidaysCH
  
  N <- nrow(bankHolidaysCH)
  max.cantons <- max(sapply(bankHolidaysCH$counties, length))
  
  mat <- as.data.frame(matrix(NA, ncol = max.cantons, nrow = N))
  
  for(i in 1:N){
    
    m <- lengths(bankHolidaysCH$counties[i])
    mat[i, 1:m] <- bankHolidaysCH$counties[[i]]
    
  }
  
  mat$date <- bankHolidaysCH$date
  
  # long format
  data_long <- gather(mat, name.col, canton, V1:V25, factor_key=TRUE)
  data_long$name.col <- NULL
  data_long <- data_long[complete.cases(data_long$canton),]
  data_long$hol <- 1
  
  return(data_long)
}

lapply(pathURL, gatBankHol) -> holCH
holCH <- do.call(rbind, holCH)
saveRDS(holCH, file = "data/holCH")







