#' linear_interpolation
#'
#' It returns an array with the variable interpolated between the year_start and the year_end.
#' Year end in not included in the returned array. The returned array is of dimension
#' [1:layer, 1:polygon, (1:730)*(year_end-year_start)].
#'
#' @param year_start numeric Start year
#' @param year_end numeric End year
#' @param year_name_decade_start character Start year name in Atlantis input. year_start and year_name_decade_start may differ when the second refer to a decade, the first is on average the mid-year of the decade.
#' @param year_name_decade_end Character End year name in Atlantis input. year_end and year_name_decade_end may differ when the second refer to a decade, the first is on average the mid-year of the decade.
#' @param scenario character Scenario name
#' @param variable character Variable name
#'
#' @returns Array of interpolated values
#' @export
#'
#' @examples
#' year_start = 2015
#' year_end = 2025
#' year_name_decade_start = 2010
#' year_name_decade_end = 2020
#' scenario = "status_quo"
#' variable = "temperature"
#' linear_interpolation(year_start, year_end, scenario, variable)

linear_interpolation <- function(year_start, year_end, scenario, variable,
                                 year_name_decade_start = "", year_name_decade_end = ""){
  if (year_start>= year_end) stop("The start year needs to be ealier than the end year")
  if (year_end == year_start+1) stop("The years are consecutive")

  if(year_name_decade_start == ""){year_name_decade_start = year_start}
  if(year_name_decade_end == ""){year_name_decade_end = year_end}

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
    nc.start.name = paste0("pugetsound_SSM_Atlantis_",i,"_",scenario,"_",year_name_decade_start,".nc")
    nc.end.name   = paste0("pugetsound_SSM_Atlantis_",i,"_",scenario,"_",year_name_decade_end,".nc")
    nc.start <- ncdf4::nc_open(here::here("Atlantis_inputs", scenario, year_name_decade_start,nc.start.name))
    nc.end   <- ncdf4::nc_open(here::here("Atlantis_inputs", scenario, year_name_decade_end,  nc.end.name))

    if (i == 'uvw'){
      # Repeat current of start year until half the period, then repeat current of end period
      results <- list()
      # Exchange
      var.name <- names(nc.start$var)
      var_exchange <- ncvar_get(nc.start, varid = var.name[1])
      var_dest_b <- ncvar_get(nc.start, varid = var.name[2])
      var_dest_k <- ncvar_get(nc.start, varid = var.name[3])

      var_end_exchange <- ncvar_get(nc.end, varid = var.name[1])
      var_end_dest_b <- ncvar_get(nc.end, varid = var.name[2])
      var_end_dest_k <- ncvar_get(nc.end, varid = var.name[3])


      results$exchange <- array(0, dim = c(dim(var_end_exchange)[-4], 730*(N_year-1)))
      results$dest_b <- array(0, dim = c(dim(var_end_dest_b)[-4], 730*(N_year-1)))
      results$dest_k <- array(0, dim = c(dim(var_end_dest_k)[-4], 730*(N_year-1)))

      for (year in year_start:(year_end-1)){
        if (year < mid_year){
          results$exchange[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_exchange
          results$dest_b[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_dest_b
          results$dest_k[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_dest_k
        }else{
          results$exchange[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_end_exchange
          results$dest_b[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_end_dest_b
          results$dest_k[,,,(1+730*(year-year_start)):(730*(year-year_start+1))] <- var_end_dest_k
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
      results[,,-(1:730)]  # the last year is not included in the returned array
    }
    ncdf4::nc_close(nc.start)
    ncdf4::nc_close(nc.end)

    return(results)
}
