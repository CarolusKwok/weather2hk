#' HK weather - Downloading lighting & radar images from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param type Type of lighting data to be loaded. Accepts "cc" and "cg"
#' @param range Radar range in km. Accepts 64 and 256.
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_ltng()
hk_load_ltng = function(time = weather2::tool_datetime(end = Sys.time(), by = "6 min", duration = "96 hour"),
                        type = c("cc", "cg"), range = c(64, 256), list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(sum(type == "cc" | type == "cg") != length(type)){
    cli::cli_text('Error: {.var type} can only be "cc", "cg" or both.')
    cli::cli_bullets(c("x" = 'You supplied {.var {type}}.'))
    return(invisible())
  }
  if(sum(range == 64 | range == 256) != length(range)){
    cli::cli_text('Error: {.var range} can only be "64", "256" or both.')
    cli::cli_bullets(c("x" = 'You supplied {.var {type}}.'))
    return(invisible())
  }
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Find all combinations of type and range
  URL = tidyr::crossing(type = type, range = range, time = time) %>%
    dplyr::mutate(dit = ifelse(range == 64, 6, 12),
                  push = ifelse(range == 64, 0, 6),
                  time = lubridate::with_tz(time, tzone = "HongKong"),
                  year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com + push, 00, tz = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_",
                               range, toupper(type),"_", LDate, LHour,".png"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "LTNG",
                               "/", "LTNG", sprintf("%03d", range), type,
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_LTNG", sprintf("%03d", range), type, "_", LDate, "_", LHour, ".png")) %>%
    dplyr::arrange(Ltime) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
  #Start to download
  title = paste0("Lighting & Radar_",
                 stringr::str_flatten(sprintf("%03d", range)), " ",
                 stringr::str_flatten(type),
                 "(HKO)")
  weather2::w2_load_file(data = URL, attempt = attempt,
                         title = title, list_fail = list_fail, worker = worker, check = F)
}
