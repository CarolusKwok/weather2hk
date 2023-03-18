#' HK weather - Read the GTMP files
#'
#' @param file_list a list of csv file directories
#'
#' @return
#' @export
#'
#' @examples hk_read_gtmp(c("C:/Users/carol/Desktop/1.csv", "C:/Users/carol/Desktop/2.csv", "C:/Users/carol/Desktop/3.csv"))
hk_read_gtmp = function(file_list){
  df = weather2::read_file_csv(file_list)
  colnames(df) = c("time", "station", "value")
  df = dplyr::mutate(df,
                     type = "gtmp",
                     time = ISOdatetime(substr(time,  1,  4),
                                        substr(time,  5,  6),
                                        substr(time,  7,  8),
                                        substr(time,  9, 10),
                                        substr(time, 11, 12),
                                        0, tz = "HongKong"),
                     value = as.numeric(value)) %>%
    dplyr::arrange(station, time) %>%
    dplyr::relocate(station, time, type, value)
  return(df)
}
