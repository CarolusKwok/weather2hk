#' HK weather - Downloading weather chart from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#'
#' @return
#' @export
#'
#' @examples hk_load_wcht()
hk_load_wcht = function(time = weather2::tool_datetime(end = Sys.time(), by = "1 hour", duration = "7 day"),
                        list_fail = T, dir = getwd(), attempt = 5, worker = 1, threshold = 0.6){
  #Check
  #Additional variables
  dit = 6
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(Ztime = lubridate::with_tz(time, tzone = "UTC"),
                  Zyear = lubridate::year(Ztime),
                  Zmonth = lubridate::month(Ztime),
                  Zday = lubridate::day(Ztime),
                  Zhour = lubridate::hour(Ztime),
                  com = Zhour %% dit,
                  ZLtime = ISOdatetime(Zyear, Zmonth, Zday, Zhour - com, 00, 00, tz = "UTC"),
                  Ltime = lubridate::with_tz(ZLtime, tzone = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/currwx/wxchart/",
                               LDate, substr(LHour, 1, 2), ".gif"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "WCHT",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", "HK_WCHT", "_", LDate, "_", LHour, ".gif")) %>%
    dplyr::distinct() %>%
    dplyr::select(Info, URL, DIR)
  weather2::w2_load_file(data = URL, title = "Weather Chart (HKO)",
                         attempt = attempt, worker = worker, list_fail = list_fail, threshold = threshold, check = F)
}
