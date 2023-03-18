#' HK weather - Downloading daily rainfall distribution images from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param lan Language. Accepts "en", "tc", "sc".
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_rain_hr()
hk_load_rain_hr = function(time = weather2::tool_datetime(end = Sys.time(), by = "15 min", duration = "7 day"),
                           lan = "en", list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_lan(lan, "lan")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Additional variables
  dit = 15
  nlan = ifelse(lan == "en", "e",
         ifelse(lan == "tc", "c",
         ifelse(lan == "sc", "c", NA)))
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong")) %>%
    dplyr::select(time, Ltime) %>%
    dplyr::mutate(LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap",
                               LDate, LHour, nlan, ".png"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "RAIN",
                               "/", "RAIN_hr", lan,
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_RAIN_hr", lan, "_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
    #Start to download
    weather2::w2_load_file(data = URL, attempt = attempt, title = paste0("Hourly Rain distribution_", lan, " (HKO)"),
                           list_fail = list_fail, worker = worker, check = F)
}
