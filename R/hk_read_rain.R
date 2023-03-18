#' HK weather - Read the RAIN files
#'
#' @param file_list a list of csv file directories
#'
#' @return
#' @export
#'
#' @examples hk_read_rain(c("C:/Users/carol/Desktop/1.csv", "C:/Users/carol/Desktop/2.csv", "C:/Users/carol/Desktop/3.csv"))
hk_read_rain = function(file_list){
  df = weather2::read_file_csv(file_list)
  colnames(df) = c("time_s", "time_e", "lat", "lon", "value")
  df = dplyr::mutate(.data = df,
                     time_s = as.POSIXct(as.character(time_s), tz = "HongKong", format = c("%Y%m%d%H%M")),
                     time_e = as.POSIXct(as.character(time_e), tz = "HongKong", format = c("%Y%m%d%H%M")),
                     time = as.POSIXct((as.numeric(time_s) + as.numeric(time_e))/2,
                                       tz = "HongKong", origin = "1970-01-01"),
                     station = "Hong Kong",
                     type = "rain",
                     value = as.numeric(value)) %>%
    dplyr::relocate(station, time, type, value, time_s, time_e, lat, lon)

  return(df)
}
