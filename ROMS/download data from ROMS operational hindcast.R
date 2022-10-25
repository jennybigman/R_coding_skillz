	# how to download data from operational hindcast 

	library(here)
	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(data.table)
	library(tidync)
	
	# this example is for bottom temp and for 1970-1974
	
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC/B10K-K20_CORECFS/Level2/"
  opendap <- "1970-1974/B10K-K20_CORECFS_1970-1974_average_temp_bottom5m.nc"
  
  nc <- nc_open(paste(url_base, opendap, sep = ""))

  lats <- ncvar_get(nc,"lat_rho") # wait until all chunks together
  lons <- ncvar_get(nc,"lon_rho") # wait until all chunks together
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

	# create object for time axis
	t_axis   <- ncvar_get(nc,"ocean_time") # wait until all chunks together
	time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") # wait until all chunks together

	# download temp array -- for some reason, only works without last two time steps
	temp_array <- ncvar_get(nc, "temp")

	t_axis   <- ncvar_get(nc,"ocean_time") # wait until all chunks together
	#time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 

  # name the dimensions
	dim(temp_array)

	dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = t_axis[1:nrow(t_axis)])

	# turn array into dataframe 
	temp_df <- reshape2::melt(temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
	temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	temp_df$latitude <- lats[cbind(temp_df$Xi, temp_df$Eta)]

	# save output as .csv
	
	fwrite(temp_df, "./ROMS/bottom_temp_1970-1974.csv")
	
	