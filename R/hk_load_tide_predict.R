#' HK weather - Download predicted tidal height data from https://www.hko.gov.hk/en/tide/ttext.htm
#'
#' @param year Year to download
#' @param station Station to download. Check the hk_dict_tide for more information
#'
#' @return
#' @export
#'
#' @examples hk_load_tide_predict("CLK", c(2016, 2021))
hk_load_tide_predict = function(station, year){
  #Preset function ####
  lhtide_method1 = function(URL, station, year){
    Lines1 = readLines(URL)%>%
      tibble::as_tibble() %>%
      dplyr::slice(match("    Date      Time Height(m)  Time Height(m)  Time Height(m)  Time Height(m)", value):
                     (match("    <a href=\"http://www.w3.org/WAI/WCAG2AA-Conformance\" target=\"_blank\">", value)-8)) %>%
      dplyr::filter(!(value == "    Date      Time Height(m)  Time Height(m)  Time Height(m)  Time Height(m)" |
                        value == "____________________________________________________________________________")) %>%
      dplyr::mutate(value = trimws(value)) %>%
      tidyr::drop_na() %>%
      dplyr::bind_rows(tibble::tibble(value = "MM DD t1 h1 t2 h2 t3 h3 t4 h4"),.)
    Data1 = suppressWarnings(readr::read_table(I(Lines1$value))) %>%
      dplyr::mutate(time1 = ISOdatetime(year, MM, DD, substr(t1, 1, 2), substr(t1, 3, 4), 00, tz = "HongKong"),
                    time2 = ISOdatetime(year, MM, DD, substr(t2, 1, 2), substr(t2, 3, 4), 00, tz = "HongKong"),
                    time3 = ISOdatetime(year, MM, DD, substr(t3, 1, 2), substr(t3, 3, 4), 00, tz = "HongKong"),
                    time4 = ISOdatetime(year, MM, DD, substr(t4, 1, 2), substr(t4, 3, 4), 00, tz = "HongKong")) %>%
      dplyr::select(-MM, -DD, -t1, -t2, -t3, -t4)

    Data2 = dplyr::bind_rows(dplyr::select(Data1, time1, h1) %>% dplyr::rename(time = time1, tide = h1),
                             dplyr::select(Data1, time2, h2) %>% dplyr::rename(time = time2, tide = h2),
                             dplyr::select(Data1, time3, h3) %>% dplyr::rename(time = time3, tide = h3),
                             dplyr::select(Data1, time4, h4) %>% dplyr::rename(time = time4, tide = h4)) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(station = station) %>%
      tidyr::drop_na()
    return(Data2)
  }
  lhtide_method2 = function(URL, station, year){
    Lines1 = tibble::as_tibble(readLines(URL)) %>%
      dplyr::filter(stringr::str_detect(value, "<TD>")) %>%
      dplyr::mutate(value = stringr::str_remove_all(string = value, pattern =  "</TR>|<TR>|&nbsp;"),
                    value = stringr::str_replace_all(string = value, pattern = "<TD>|</TD>", replacement =" "),
                    value = trimws(value)) %>%
      dplyr::slice(2:(dplyr::n())) %>%
      dplyr::bind_rows(tibble::tibble(value = "MM DD t1 h1 t2 h2 t3 h3 t4 h4"), .)
    Data1 = suppressWarnings(readr::read_table(I(Lines1$value))) %>%
      dplyr::mutate(year = year,
                    time1 = ISOdatetime(year, MM, DD, substr(t1, 1, 2), substr(t1, 3, 4), 00, tz = "HongKong"),
                    time2 = ISOdatetime(year, MM, DD, substr(t2, 1, 2), substr(t2, 3, 4), 00, tz = "HongKong"),
                    time3 = ISOdatetime(year, MM, DD, substr(t3, 1, 2), substr(t3, 3, 4), 00, tz = "HongKong"),
                    time4 = ISOdatetime(year, MM, DD, substr(t4, 1, 2), substr(t4, 3, 4), 00, tz = "HongKong")) %>%
      dplyr::select(-year, -MM, -DD, -t1, -t2, -t3, -t4)

    Data2 = dplyr::bind_rows(dplyr::select(Data1, time1, h1) %>% dplyr::rename(time = time1, tide = h1),
                             dplyr::select(Data1, time2, h2) %>% dplyr::rename(time = time2, tide = h2),
                             dplyr::select(Data1, time3, h3) %>% dplyr::rename(time = time3, tide = h3),
                             dplyr::select(Data1, time4, h4) %>% dplyr::rename(time = time4, tide = h4)) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(station = station) %>%
      tidyr::drop_na()
    return(Data2)
  }

  hrtide_method1 = function(URL, station, year){
    Lines = readLines(URL) %>%
      tibble::as_tibble() %>%
      dplyr::slice((match("<TR><TH COLSPAN='2'>Date</TH><TH>&nbsp;</TH><TH COLSPAN='24'>Hour</TH></TR>", value)+4):
                     (match("    <a href=\"http://www.w3.org/WAI/WCAG2AA-Conformance\" target=\"_blank\">", value)-7)) %>%
      dplyr::mutate(value = stringr::str_remove_all(string = value, pattern = "<TR>|</TR>"),
                    value = stringr::str_replace_all(string = value, pattern = "<TD>|</TD>", replacement = " "),
                    value = trimws(value)) %>%
      dplyr::filter(substr(value, 1, 1) %in% c(0, 1)) %>%
      dplyr::bind_rows(tibble::tibble(value = "MM DD 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"),
                       .)

    Data = readr::read_table(file = I(Lines$value)) %>%
      tidyr::pivot_longer(cols = -c("MM", "DD"), names_to = "HH", values_to = "tide") %>%
      dplyr::mutate(time = ISOdatetime(year, MM, DD, HH, 00, 00),
                    station = station) %>%
      dplyr::select(-MM, -DD, -HH) %>%
      tidyr::drop_na()
    return(Data)
  }
  hrtide_method2 = function(URL, station, year){
    Lines1 = readLines(URL) %>%
      tibble::as_tibble() %>%
      dplyr::filter(grepl("<TD>", value)) %>%
      dplyr::mutate(value = stringr::str_remove_all(string = value, pattern =  "<TR>|</TR>|&nbsp;"),
                    value = stringr::str_replace_all(string = value, pattern = "<TD>|</TD>|<TH>|</TH>", replacement =" "),
                    value = trimws(value)) %>%
      dplyr::bind_rows(tibble::tibble(value = "MM DD 01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24 "), .)
    Data1 = suppressWarnings(readr::read_table(file = I(Lines1$value))) %>%
      dplyr::select(-X27) %>%
      tidyr::pivot_longer(cols = -c("MM", "DD"), names_to = "hh", values_to = "tide") %>%
      dplyr::mutate(time = ISOdatetime(year, MM, DD, hh, 00, 00, tz = "HongKong"),
                    station = station) %>%
      dplyr::select(time, station, tide) %>%
      dplyr::arrange(time) %>%
      dplyr::distinct()
    return(Data1)
  }

  #Check ####
  list_station = weather2::hk_dict_tide()$name
  for(i in station){
    if(weather2::w2_check_list_item(i, "station", list_station)){return(invisible())}
  }
  if(weather2::w2_check_type_integer(year, value_name = "year")){return(invisible())}


  #Start Running ####
  data = tidyr::crossing(station, year) %>%
    dplyr::mutate(lhtide = paste0("https://www.hko.gov.hk/tide/e",
                                  station, "text", sprintf("%04d", year), ".html"),
                  hrtide = paste0("https://www.hko.gov.hk/tide/",
                                  "CLK", "textPH", "2022", ".htm")) %>%
    tidyr::pivot_longer(cols = c("hrtide", "lhtide"), names_to = "name", values_to = "URL")

  data_tide = tibble::tibble(time = as.POSIXct(NA_real_, tz = "HongKong"),
                             station = NA_character_,
                             tide = NA_real_,
                             .rows = 0)

  for(i in 1:nrow(data)){
    #2020 - 2025: Use method 2
    #2015 - 2019: Use method 1
    sel_year = data$year[i]
    sel_type = data$name[i]
    sel_URL = data$URL[i]
    sel_station = data$station[i]

    if(2015 <= sel_year & sel_year <= 2019){
      if(sel_type == "hrtide"){data_temp = hrtide_method1(URL = sel_URL,
                                                          station = sel_station,
                                                          year = sel_year)}
      if(sel_type == "lhtide"){data_temp = lhtide_method1(URL = sel_URL,
                                                          station = sel_station,
                                                          year = sel_year)}
    }
    if(2020 <= sel_year & sel_year <= 2025){
      if(sel_type == "hrtide"){data_temp = hrtide_method2(URL = sel_URL,
                                                          station = sel_station,
                                                          year = sel_year)}
      if(sel_type == "lhtide"){data_temp = lhtide_method2(URL = sel_URL,
                                                          station = sel_station,
                                                          year = sel_year)}
    }
    data_temp = dplyr::relocate(data_temp, time, station, tide)
    data_tide = dplyr::bind_rows(data_tide, data_temp)
  }
  data_tide = dplyr::arrange(data_tide, station, time)
  return(data_tide)
}
