####################
# function to prepare the main input datafile and do predictions if it is needed
# authors: Andres camilo mendez, Harold achicanoy
###################


prepare_input_data <- function(data_path = choose.files( caption = "Select a valid .csv file"), 
                               col_number = NULL, 
                               mask = mask,
                               env_rasts){
  
  msk <- raster(mask)
  data <- read.csv(data_path, header = T)
  
  
  #in case of col_number empty request a value from the user
  if(is.null(col_number)){
    warning("please enter the column number of response variable", immediate. = TRUE, noBreaks. = T)
    col_number <- readline(prompt="Enter column number of response variable: ") 
  }
  #check if the number enter by de user has the correct format(only a number)
  if(!grepl("^[0-9]+$", col_number)){
    warning("Only numbers are allowed", immediate. = TRUE, noBreaks. = T)
    col_number <- readline(prompt="Enter column number of response variable: ")
  }
  
  if("status" %in% names(data)){
    status <- data %>% dplyr::select(., matches("status", ignore.case = T))  
  }else{status <- NULL}
   
  #select only the response variable and lat / long and create an ID column
  data <- data %>% dplyr::select(., as.integer(col_number), 
                             matches("declat|^[L|l]atitude|lat", ignore.case = T), 
                             matches("declon|^[L|l]ongitude|lon", ignore.case = T) )
  if(ncol(data) > 3){
    stop("Data base has more than 1 lat/long variable")
  }
  
  if(ncol(data) <= 2){
    stop("Data base doesn't have one of lat/long variable")
  }
  #change names
  names(data) <- c("Y", "Latitude", "Longitude")
  
  cat("Cleaning zero lat/lon \n")
  
  data <- data %>% 
    dplyr::mutate(ID = 1:nrow(.)) %>%
    dplyr::filter(., Latitude != 0 | Longitude != 0) %>% 
    dplyr::filter(., !is.na(Latitude) | !is.na(Longitude)) %>%
    dplyr::filter(., !is.na(Y))
  
  cat("Removing coordinates on Oceans/Seas \n")
  data <- data[which(!is.na(raster::extract(x = msk, y = data[,c("Longitude", "Latitude")]))),]
  
  cat("Extracting values from rasters \n")
  #climate extraction
 
  current_clim_layer <- env_rasts
  
  cat("Removing duplicated coordinates \n")
  rep <- which(duplicated( raster::extract(msk, data[ , c("Longitude", "Latitude")], cellnumbers = TRUE)  ))
  if(length(rep) != 0){
    data  <- data[-rep, ]
  }
  
  
  clim_table <-  SpatialPoints(data[, c("Longitude", "Latitude")], crs(msk)) %>% 
    raster::extract(stack(current_clim_layer), .)

  full_data <- data %>% 
        data.frame(., clim_table) 
  
  full_data <- full_data[complete.cases(full_data[, c(-1, -2, -3)]), ]
  full_data <- droplevels(full_data)

    #add status column if it exists
    
    if(!is.null(status)){
      full_data$status <- status[full_data$ID, ]
    }
    names(full_data)[1] <- "ensemble"
    full_data$ID <- NULL
    
    return(full_data)
  
}#end fucntion
