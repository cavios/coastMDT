##' Write a grid[lon,lat] to a netcdf file 
##'
##' This function \code{grid2file} saves a grid[lon,lat] to a netcdf file.
##' @param grid An object as returned by the function 'iterativeAveSmoother', which includes a list containing  a matrix g[lon,lat], a vector lon (longitudes) and a vector lat (latitudes).
##' @param varname A string containing the name of the variable, the default is 'MDT'  
##' @param filename A string containing the file name, the default is 'grid.nc' 
##' @importFrom ncdf4 nc_create ncvar_def ncvar_put nc_close ncdim_def
##' @details ...
##' @export
##' @examples
##'\dontrun{
##' grid2file(boxTG_ALT,filename='MDT_filtered.nc')
##' }
grid2file<-function(grid,varname="MDT",filename="grid.nc"){
   # Get x and y vectors (dimensions)
   Longvector <- grid$lon
   Latvector <- grid$lat
   # Define data
   
   # Define the dimensions
   dimX = ncdim_def("Long", "degrees", Longvector)
   dimY = ncdim_def("Lat", "degrees", Latvector)
   
   # Define missing value
   mv = NA
   
   # Define the data
   
   var2d <-ncvar_def( varname, "units", list(dimX,dimY), mv, prec="double")
   
   # Create the NetCDF file
   
   nc <- nc_create(filename, var2d)
   
   # Write data to the NetCDF file
   ncvar_put(nc, var2d, grid$g)
   
   # Close your new file to finish writing
   nc_close(nc)
   }

