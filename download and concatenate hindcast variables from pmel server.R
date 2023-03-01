	# download and transform level 2 hindcast output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)


	#### hindcast temps ####
	
	hindcast_download_func <- function(yr){

	# set up filepath
  fname <- file.path(tdsbase, 
                     hcsim_updated, 
                     "Level2",
                     paste0(yr, "-", yr + 4), 
                     paste0(hcsim_updated, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get lats, longs, ocean_time
  lats <- ncvar_get(ncin,"lat_rho")
  lons <- ncvar_get(ncin,"lon_rho")
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  temp_array <- ncvar_get(ncin, "temp") # can change this to whatever variable you need
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	temp_df <- reshape2::melt(temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
	temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	temp_df$latitude <- lats[cbind(temp_df$Xi, temp_df$Eta)]
	
	return(temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	temp_hind_dat_list <- lapply(yr, hindcast_download_func)
	
	temp_hind_dat <- bind_rows(temp_hind_dat_list)
	
	temp_hind_dat$month <- month(temp_hind_dat$DateTime) # month of year
	temp_hind_dat$week <- week(temp_hind_dat$DateTime) # week of year
	temp_hind_dat$year <- year(temp_hind_dat$DateTime)
	
	

	write_csv(temp_hind_dat, file = here("./data/hindcast_temp.csv"))

	#### test plots ####
	
	## yearly ###

	years_hind <- c(2016, 2017, 2018, 2019, 2020, 2021)
	
	temp_df_trim <- temp_hind_dat %>%
		filter(year %in% years_hind)

	temp_df_sum <- temp_df_trim %>%
		group_by(year, latitude, longitude) %>%
		summarise(mean_temp = mean(temp))
	
	temp_df_sum_sf <- temp_df_sum	%>%
			 mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	# function to plot #
	temp_yr_plot_func_hind <- function(x){
		
		    new_dat <- temp_df_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c( "-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "temp") +
					theme_bw() +
    	  	ggtitle(paste0(x)) + 
  		theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  	
    	  plot
    	  
	}
	
	hind_plot_list <- lapply(years_hind, temp_yr_plot_func_hind)
	
	name_func_year <- function(x){
  	 paste0(x, "_temp")
  }
   
	names_year <- sapply(years_hind, name_func_year)
  
	name_func_file <- function(x){
  	paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/", x)
  }
   
  plot_names <- sapply(names_year, name_func_file)
			
   ggsave_func <- function(x,y,w = 10,h = 10){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = w, height = h, units = "in")
   }
   
	plot_list <- mapply(ggsave_func, x = hind_plot_list, y = names_year)


	