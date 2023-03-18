#' MO weather - Download tidal images from https://www.smg.gov.mo/zh
#'
#' @param time Date/time to download
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param subattempt Attempts to be made per attempt to download.
#' @param worker Numbers of sessions to be open
#' @param list_fail List failed-to-download items
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#'
#' @return
#' @export
#'
#' @examples mo_load_tide()
mo_load_tide = function(time = weather2::tool_datetime(end = Sys.time(), duration = "91 hour", by = "1 min") %>% weather2::tool_datetime_select(by = "min", value = 0),
                        dir = getwd(), attempt = 900, subattempt = 5, worker = 1, list_fail = T, threshold = 0.4){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}

  #Additional variables
  dit = 15

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "Asia/Macau")

  #Format
  URL = tidyr::expand_grid(Time = time,
                           seq = 1:attempt) %>%
    dplyr::left_join(y = tibble::tibble(Time = time,
                                        Set = 1:length(time)),
                     by = "Time") %>%
    dplyr::mutate(Year = lubridate::year(Time),
                  Month= lubridate::month(Time),
                  Day = lubridate::day(Time),
                  Hour = lubridate::hour(Time),
                  Min = lubridate::minute(Time),
                  com = Min %% dit,
                  Ltime = ISOdatetime(Year, Month, Day, Hour, (Min - com), 00, tz = "Asia/Macau"),

                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Ztime = as.numeric(difftime(lubridate::with_tz(Ltime,tzone = "UTC"),
                                              ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC"),
                                              units = "sec")),
                  URL = paste0("https://cms.smg.gov.mo/uploads/backup/WL/WL_",
                               (Ztime+seq),
                               ".png"),
                  Info = paste0(LDate, "-", LHour),
                  DIR = paste0(dir,
                               "/", "MO_Data",
                               "/", "TIDE",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "MO_TIDE_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Set, URL, DIR, Info)

  #Start
  weather2::w2_load_fileset(data = URL,
                            title = "MO Tidal Height",
                            attempt = subattempt,
                            list_fail = list_fail,
                            worker = worker,
                            threshold = threshold,
                            check = F)
}
