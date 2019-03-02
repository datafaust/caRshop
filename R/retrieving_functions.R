#' Return VIN information from the NHTSA database
#'
#' vin_diesel breaks into NHTSA's database to quickly and most furiously return VIN information. More information on the NHTSA API is available at https://vpic.nhtsa.dot.gov/api/#
#' @import httr
#' @import jsonlite
#' @param vin vin number in quotes
#' @param sec denotes Sys.sleep() between requests if there are multiple, Defaults to 5 seconds
#' @param tidyup simplifies json response into a dataframe, Defaults to TRUE
#' @keywords vin
#' @export
#' @examples
#' vin_diesel("1G2HX54K724118697", sec = 1, tidyup = T)


vin_diesel = function(vin,sec = 5,tidyup = TRUE) {

    if (tidyup == TRUE) {
      vinme = paste0('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/',vin,'?format=json')

      print(vinme)
      myvin = httr::GET(vinme)
      myvin = httr::content(myvin, as = "text")
      super_vin = jsonlite::fromJSON(myvin)
      Sys.sleep(sec)
      return(as.data.frame(super_vin))
    }

    else {
      vinme = paste0('https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/',vin,'?format=json')
      print(vinme)
      myvin = httr::GET(vinme)
      myvin = httr::content(myvin, as = "text")
      super_vin = jsonlite::fromJSON(myvin)
      Sys.sleep(sec)
      return(super_vin)
    }
}



#' Return EXTENDED VIN information from the NHTSA database
#'
#' vin_dieselex breaks into NHTSA's database to quickly and most furiously return VIN information. More information on the NHTSA API is available at https://vpic.nhtsa.dot.gov/api/#
#' @import httr
#' @import jsonlite
#' @param vin vin number in quotes
#' @param sec denotes Sys.sleep() between request, Defaults to 5 seconds
#' @param tidyup simplifies json response into a dataframe, Defaults to TRUE
#' @keywords vin
#' @export
#' @examples
#' vin_dieselex("1G2HX54K724118697", sec = 1, tidyup = TRUE)

#extended vin decoder
vin_dieselex = function (vin,sec = 5, tidyup = TRUE) {

    if (tidyup ==TRUE) {
      vinex = paste0("https://vpic.nhtsa.dot.gov/api/vehicles/ decodevinvalues/",vin,"?format=json")
      print(vinex)
      vinex_ans = httr::GET(vinex)
      vinex_ans = httr::content(vinex_ans, as = "text")
      vinex_ans = jsonlite::fromJSON(vinex_ans)
      Sys.sleep(sec)
      return(as.data.frame(vinex_ans))
    }
    else {
      vinex = paste0("https://vpic.nhtsa.dot.gov/api/vehicles/ decodevinvalues/",vin,"?format=json")
      vinex_ans = httr::GET(vinex)
      vinex_ans = httr::content(vinex_ans, as = "text")
      vinex_ans = jsonlite::fromJSON(vinex_ans)
      Sys.sleep(sec)
      return(vinex_ans)
    }

}


#' Return vehcile type by id from the NHTSA database
#'
#' vin_dieseltype returns vehicle types associated with a makeid. More information on the NHTSA API is available at https://vpic.nhtsa.dot.gov/api/#
#' @import httr
#' @import jsonlite
#' @param vin makeid in quotes
#' @param tidyup simplifies json response into a dataframe, Defaults to TRUE
#' @keywords vin
#' @export
#' @examples
#' vin_dieseltype("18697", tidyup = TRUE)

vin_dieseltype = function (makeid, tidyup = TRUE) {


    if (tidyup ==TRUE) {
      vinex = paste0("https://vpiclist.cdan.dot.gov/vpiclistapi/vehicles/GetVehicleTypesForMakeId/450?format=json")
      print(vinex)
      vinex_ans = httr::GET(vinex)
      vinex_ans = httr::content(vinex_ans, as = "text")
      vinex_ans = jsonlite::fromJSON(vinex_ans)
      return(as.data.frame(vinex_ans))
    }
    else {
      vinex = paste0("https://vpiclist.cdan.dot.gov/vpiclistapi//vehicles/GetVehicleTypesForMakeId/450?format=json")
      vinex_ans = httr::GET(vinex)
      vinex_ans = httr::content(vinex_ans, as = "text")
      vinex_ans = jsonlite::fromJSON(vinex_ans)
    }

}



#' A wrapper for Thomas J. Leeper's MPG package merging data from fueleconomy.gov & the myMPGwebsite
#'
#' Retrieve vehicle fuel economy by vehicle ID & Retrieve average fuel economy from My MPG by vehicle ID in dataframe format merged
#' @import httr
#' @import jsonlite
#' @param vin VINid number last 5 digits of a VIN
#' @keywords vin
#' @export
#' @examples
#' fuel_build(18697)
fuel_build = function(vinid) {

    return(
      cbind(
        as.data.frame(
          t(
            unlist(
              mpg::feVehicle(vinid)
            )
          )
        ),
        as.data.frame(
          t(
            unlist(
              mpg::mympgAvg(vinid)
            )
          )
        )
      )



    )

}


#' vin_thief: return a list of Year, Make, Model based on VIN
#'
#' String returned is useful for feeding into other api's that look up on model and year (FuelEconomy.gov) or rOpenGov/mpg
#' @import httr
#' @import jsonlite
#' @param vin VIN number in quotes
#' @param sec denotes Sys.sleep() between requests if there are multiple, Defaults to 5 seconds
#' @param vector.transform Denotes a transformation to a character vector for unique id usage. Defaults to FALSE.
#' @param sep  symbol denoting separator, can be used to return as a character vector separated by symbols e.g ("model-make-year"). Defaults to "-".
#' @keywords vin
#' @export
#' @examples
#' vin_thief("1G2HX54K724118697", vector.transform = TRUE, sep = ",")

vin_thief = function(vin,sec = 5,vector.transform = T, sep = "-") {

    if (vector.transform == TRUE) {
      vinme = paste0('https://vpiclist.cdan.dot.gov/vpiclistapi/vehicles/DecodeVinValues/',vin,'?format=json')
      print(vinme)
      myvin = httr::GET(vinme)
      myvin = content(myvin, as = "text")
      Sys.sleep(sec)
      super_vin = jsonlite::fromJSON(myvin)
      model_year = super_vin$Results$ModelYear
      make = super_vin$Results$Make
      model = super_vin$Results$Model
      unique_id = paste(model_year, make, model, sep = sep)
      return(tolower(unique_id))
    }

    else {
      vinme = paste0('https://vpiclist.cdan.dot.gov/vpiclistapi/vehicles/DecodeVinValues/',vin,'?format=json')
      print(vinme)
      myvin = httr::GET(vinme)
      myvin = content(myvin, as = "text")
      Sys.sleep(sec)
      super_vin = jsonlite::fromJSON(myvin)
      model_year = super_vin$Results$ModelYear
      make = super_vin$Results$Make
      model = super_vin$Results$Model
      return(
        list(tolower(model_year, make, model))
      )
    }


}


#' vin_batcher: outputs a vector of vins for batch upload to NHTSA
#'
#' Input a list of VINS to return a vector semicolon separated
#' @param vinlist a vector of VIN numbers (list or dataframe column)
#' @keywords vin
#' @export
#' @examples
#' vin_batcher(c("1G2HX54K724118697","1G2HX54K724118696")) #save as csv and copy paste values into: https://vpic.nhtsa.dot.gov/api/

vin_batcher = function(vinlist) {
  vinlist = paste(vinlist, collapse=";")
  return(vinlist)
}




#' vin_row: Retrieves rows for vin mpg lookup from fueleconomy.com
#'
#' Input make model and year to retrieve matching list with row ids
#' @param year vehicle year
#' @param make vehicle make
#' @param model vehicle model
#' @keywords vin
#' @export
#' @examples
#' vin_row("2015", "honda", "civic")


vin_row = function(year, make, model) {
  suber = paste('options?year=',year,'&make=',make,'&model=',model,
                sep='')
  print(suber)
  response = feQuery(baseurl='http://www.fueleconomy.gov/ws/rest/vehicle/menu/', paste(suber)
  )
  out = xmlToDataFrame(response)
}



