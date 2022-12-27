library(ncdf4)
library(lubridate)
library(abind)

#-------------------
# Setup
#-------------------

yrblock <- seq(1970, 2015, by=5) # Calibration block ####### change this to 1970, 2020!!!!!

# Variable to read 

vname1 <- "temp_bottom5m" # Variable name in filename
vname2 <- "temp"          # Variable name in file

# Simulation names

#hcsim <- "B10K-H16_CORECFS"                  # hindcast ##### change to K20 when it is back on

hcsim_updated <- "B10K-K20P19_CORECFS"

# The naming conventions for files are slightly different on mox compared to on the 
# PMEL server (PMEL server adds an extra folder level, with 5-year blocks).  In this script,
# we're reading hindcast data from the PMEL server (because it's faster than over the mox
# ssh connection) and forecast data from mox b/c it hasn't been added to the server yet.

tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server

	#--------------------
	# Read hindcast data
	#--------------------
	
	for (yr in yrblock) {

  fname <- file.path(tdsbase, 
                     hcsim_updated, 
                     "Level2",
                     paste0(yr, "-", yr+4), 
                     paste0(hcsim_updated, "_", yr, "-", yr+4, "_average_", vname1, ".nc"))
  
  # Read data
  
  ncin <- nc_open(fname)
  
  # Read data
  
  ttmp <- ncvar_get(ncin,"ocean_time")
  ttmp <- lubridate::ymd("1900-01-01") + lubridate::seconds(ttmp)
  vtmp <- ncvar_get(ncin, vname2)
  
  nc_close(ncin)
  
  # Concatenate across 5-year blocks
  
  if (yr == yrblock[1]){
    thc <- ttmp
    vhc <- vtmp
  } else {
    thc <- c(thc, ttmp)
    vhc <- abind(vhc, vtmp)
  }
	}

dim(vhc)