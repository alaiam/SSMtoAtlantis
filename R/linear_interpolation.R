#' linear_interpolation
#'
#' @param year_start numeric Start year
#' @param year_end numeric End year
#' @param scenario character Scenario name
#' @param variable character Variable name
#'
#' @returns Array of interpolated values
#' @export
#'
#' @examples
#' year_start = 2015
#' year_end = 2025
#' scenario = "status_quo"
#' variable = "temperature"
#' linear_interpolation(year_start, year_end, scenario, variable)

linear_interpolation <- function(year_start, year_end, scenario, variable){
  if (year_start>= year_end) stop("The start year needs to be ealier than the end year")
  if (year_end == year_start+1) stop("The years are consecutive")
  list.var <- c("salinity","temperature", "uvw", "NO3", "NH4",
                "SZ", "LZ", "MZ", "SP", "LP", "oxygen", "LPON", "RPON", "RDON")
  if (!variable %in% list.var) {
    stop("The variable is not in SSM, please try: ",
         paste(list.var, collapse = ", "))
  }

  N_year = year_end - year_start + 1
  mid_year = ifelse(N_year/2==round(N_year/2), year_start + N_year/2,
                    year_start + N_year/2-0.5)

  i = variable
    nc.start.name = paste0("pugetsound_SSM_Atlantis_",i,"_",scenario,"_",year_start,".nc")
    nc.end.name   = paste0("pugetsound_SSM_Atlantis_",i,"_",scenario,"_",year_end,".nc")
    nc.start <- ncdf4::nc_open(here::here("Atlantis_inputs", scenario, year_start,nc.start.name))
    nc.end   <- ncdf4::nc_open(here::here("Atlantis_inputs", scenario, year_end,  nc.end.name))
    # nc.end = nc.start
    if (i == 'uvw'){
      # Repeat current of start year until half the period, then repeat current of end period
      var.name <- names(nc.start$var)
      var <- ncvar_get(nc.start, varid = var.name[1])
      var_end <- ncvar_get(nc.end, varid = var.name[1])
      results <- array(0, dim = c(dim(var)[-4], 730*N_year))
      for (year in year_start:(year_end)){
        if (year <= mid_year){
          results[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var
        }else{
          results[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_end
        }
      }
    }else{
      var.name <- names(nc.start$var)
      var <- ncvar_get(nc.start, varid = var.name[1])
      var_end <- ncvar_get(nc.end, varid = var.name[1])
      results <- array(0, dim = c(dim(var)[-3], 730*N_year))
      for (year in year_start:(year_end)){
          results[,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var + (var_end - var)/(N_year-1)*(year-year_start)
      }
    }
    ncdf4::nc_close(nc.start)
    ncdf4::nc_close(nc.end)

    return(results)
}
