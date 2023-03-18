#' HK weather - Downloading weather photo from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param worker Numbers of sessions to be open
#' @param station Stations selected for the task, read tool_hk_wxph() tibble for more information
#'
#' @return
#' @export
#'
#' @examples hk_load_wxph()
hk_load_wxph = function(time = weather2::tool_datetime(end = Sys.time(), by = "10 min", duration = "3 day"),
                        station = "all", list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Check if station = "all", and Force station to be lower
  if("all" %in% station | "ALL" %in% station){station = weather2::tool_hk_wxph()$code}
  station = unique(toupper(station))

  #Additional variables
  dit = 5

  URL = tidyr::crossing(time = time, station = station) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),

                  Info = paste0(LDate, "-", LHour, "-", station),
                  URL = paste0("http://www.weather.gov.hk/wxinfo/aws/hko_mica/",
                               tolower(station),
                               "/img", toupper(station), "_", substr(LDate, 3, 8), "_", LHour, ".jpg"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "WXPH",
                               "/", "WXPH(", station, ")",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_WXPH(", station, ")_", LDate, "_", LHour, ".jpg")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()

  title = paste0("Weather Photo_", stringr::str_flatten(station, ","), " (HKO)")
  weather2::w2_load_file(data = URL, attempt = attempt, title = title,
                         list_fail = list_fail, worker = worker, check = F)
}
