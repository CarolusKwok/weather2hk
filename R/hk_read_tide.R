#' HK weather - Read the tide files
#'
#' @param file_list a list of csv file directories
#'
#' @return
#' @export
#'
#' @examples hk_read_tide(c("C:/Users/carol/Desktop/1.csv", "C:/Users/carol/Desktop/2.csv", "C:/Users/carol/Desktop/3.csv"))
hk_read_tide = function(file_list){
  df = weather2::read_file_csv(file_list)
  colnames(df) = c("station", "date", "time", "value")
  df = dplyr::mutate(.data = df,
                     type = "tide",
                     time = paste(date, time),
                     time = as.POSIXct(time, tz = "HongKong"),
                     value = as.numeric(value)) %>%
    dplyr::select(-date) %>%
    dplyr::arrange(station, time) %>%
    dplyr::relocate(station, time, type, value)
  return(df)
}
