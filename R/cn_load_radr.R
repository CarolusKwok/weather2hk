#' CN weather - Downloading RADAR data from http://www.weather.com.cn/
#'
#' @param time Date/time to download
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#'
#' @return Photos from http://www.weather.com.cn/
#' @export
#'
#' @examples cn_load_radr()
cn_load_radr = function(time = weather2::tool_datetime(end = Sys.time(), duration = "7 days", by = "1 mins") %>%
                          weather2::tool_datetime_select(by = "min", value = 0),
                        list_fail = T, dir = getwd(), attempt = 2L, worker = 1L){
  #Check
  if(weather2hk::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)){return(invisible())}


  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(time = lubridate::with_tz(time, tzone = "UTC"),
                  time_scan = time -
                          lubridate::minutes(lubridate::minute(time) %% 6) -
                          lubridate::seconds(lubridate::second(time)),
                  time_print  = lubridate::with_tz(time_scan, tzone = "PRC"),
                  Set = 1:dplyr::n()) %>%
    dplyr::select(time_scan, time_print, Set) %>%
    tidyr::expand_grid(tibble::tibble(dit = 0:2160)) %>%
    dplyr::mutate(time_update= time_scan + lubridate::seconds(dit),
                  URL = stringr::str_c("https://pi.weather.com.cn/i/product/pic/l/z_rada_c_babj_",
                                        sprintf("%04d", lubridate::year(time_update)),
                                        sprintf("%02d", lubridate::month(time_update)),
                                        sprintf("%02d", lubridate::day(time_update)),
                                        sprintf("%02d", lubridate::hour(time_update)),
                                        sprintf("%02d", lubridate::minute(time_update)),
                                        sprintf("%02d", lubridate::second(time_update)),
                                        "_p_dor_achn_cref_",
                                        sprintf("%04d", lubridate::year(time_scan)),
                                        sprintf("%02d", lubridate::month(time_scan)),
                                        sprintf("%02d", lubridate::day(time_scan)),
                                        "_",
                                        sprintf("%02d", lubridate::hour(time_scan)),
                                        sprintf("%02d", lubridate::minute(time_scan)),
                                        "00", ".png"),
                  Info = stringr::str_c(sprintf("%04d", lubridate::year(time_print)),
                                        sprintf("%02d", lubridate::month(time_print)),
                                        sprintf("%02d", lubridate::day(time_print)),
                                        "_",
                                        sprintf("%02d", lubridate::hour(time_print)),
                                        sprintf("%02d", lubridate::minute(time_print))),
                  DIR = paste0(dir, "/", "CN_Data", "/",
                               "RADR", "/",
                               substr(Info, 1, 4), "/",
                               substr(Info, 1, 6), "/",
                               substr(Info, 1, 8), "/",
                               "CN_RADR_", Info, ".png")) %>%
    dplyr::select(Set, DIR, URL, Info)

  #Start to download
  weather2::sys_load_fileset(data = URL, attempt = attempt, title = "China RADAR Data",
                             worker = worker, check = F, threshold = 0, list_fail = list_fail)
}
