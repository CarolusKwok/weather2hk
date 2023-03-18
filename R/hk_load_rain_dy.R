#' HK weather - Downloading daily rainfall distribution image from https://www.hko.gov.hk/en/index.html
#'
#' @param lan Language. Accepts "en", "tc", "sc"
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_rain_dy()
hk_load_rain_dy = function(lan = "en", list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Additional variables
  nlan = ifelse(lan == "en", "e",
         ifelse(lan == "tc", "c",
         ifelse(lan == "sc", "c", NA)))

  #Find the latest leap year
  for(i in lubridate::year(Sys.time()):0){
    if(lubridate::leap_year(i)){
      leap = i
      break
    }
  }

  #Generate time and format it to be HKT
  time = weather2::tool_date(end = Sys.time() - lubridate::days(1), by = "1 day", duration = "1 year") %>%
    lubridate::with_tz(tzone = "HongKong") %>%
    append(ISOdatetime(leap, 02, 29, 08, 00, 00, tz = "HongKong"))
  #Format
  URL = tibble::tibble(time = time) %>%
      dplyr::mutate(year = lubridate::year(time),
                    month = lubridate::month(time),
                    day = lubridate::day(time),
                    Date = paste0(sprintf("%04d", lubridate::year(time)),
                                  sprintf("%02d", lubridate::month(time)),
                                  sprintf("%02d", lubridate::day(time))),
                    Info = Date,
                    URL = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs",
                                 substr(Date, start = 5, stop = 8), "0000", nlan, ".png"),
                    DIR = paste0(getwd(),
                                 "/", "HK_Data",
                                 "/", "RAIN",
                                 "/", "RAIN_dy", lan,
                                 "/", substr(Date, 1, 4),
                                 "/", substr(Date, 1, 6),
                                 "/", "HK_RAIN_dy", lan, "_", Date, ".png")) %>%
      dplyr::select(Info, URL, DIR) %>%
      dplyr::distinct()
    #Start to download
    weather2::w2_load_file(data = URL, attempt = attempt, title = "Daily Rainfall Image (HKO)",
                           list_fail = list_fail, worker = worker, check = F)
}
