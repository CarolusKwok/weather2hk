#' HK weather - Read the SART files
#'
#' @param file_list a list of csv file directories
#'
#' @return
#' @export
#'
#' @examples hk_read_sart(c("C:/Users/carol/Desktop/1.csv", "C:/Users/carol/Desktop/2.csv", "C:/Users/carol/Desktop/3.csv"))
hk_read_sart = function(file_list){
  df = weather2::read_file_csv(file_list)

  colnames(df) = c("time", "station", "sart_global", "sart_direct", "sart_diffuse")
  df = dplyr::mutate(.data = df,
                     time = ISOdatetime(substr(time,  1,  4),
                                        substr(time,  5,  6),
                                        substr(time,  7,  8),
                                        substr(time,  9, 10),
                                        substr(time, 11, 12),
                                        0, tz = "HongKong"),
                     temp = stringr::str_locate(sart_global, '\\.0')[,1],
                     sart_global = as.numeric(stringr::str_sub(sart_global, 1, temp-1)),
                     temp = stringr::str_locate(sart_direct, '\\.0')[,1],
                     sart_direct = as.numeric(stringr::str_sub(sart_direct, 1, temp-1)),
                     temp = stringr::str_locate(sart_diffuse, '\\.0')[,1],
                     sart_diffuse = as.numeric(stringr::str_sub(sart_diffuse, 1, temp-1))) %>%
    dplyr::select(-temp) %>%
    tidyr::pivot_longer(names_to = "type", values_to = "value",
                        cols = c("sart_global", "sart_direct", "sart_diffuse")) %>%
    dplyr::relocate(station, time, type, value)

  return(df)
}
