
transform_metadata_to_df <- function(df){
  result <- map(df[[1]], as_tibble) %>% # Transform every row to a tibble
    list_rbind() %>% 
    unnest(cols=c(latestData, location)) %>% # Un-nest the two nested column
    mutate(
      latestData = as_datetime(latestData, tz="UTC"), # Converts to datetime
      lat = location$latLon$lat, # Extract the nested elem
      lon = location$latLon$lon, # Extract the nested elem
      location = NULL #Remove the location elem 
    )
  return(result)
}

to_iso8601 <- function(dateTime, offset){
  # Add the offset to the dateTime
  adjusted_datetime <- dateTime + days(offset)
  
  # Converts to the correct format and adds Z to the end of the string
  return(paste0(iso8601(adjusted_datetime), "Z"))
}

transform_volumes <- function(data){
  data <- toJSON(data) # Transform to a JSON object
  data_df <- as.data.frame(fromJSON(data))$node %>% #Transform the JSON object to a dataframe
    mutate(volume = total$volumeNumber$volume, #Extract the volume from the nested elem
           to = ymd_hms(to), # Convert to datetime
           from = ymd_hms(from), # Convert to datetime
           volume = as.numeric(volume)) %>% # Convert to numerical
    select(c("from", "to", "volume")) # Select the columns I need
  return(data_df) # Return the dataframe
}
