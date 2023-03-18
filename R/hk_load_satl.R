#' HK weather - Downloading satellite images from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param magn Satellite image magnification. Accepts 2/ 4/ 8, or any combinations
#' @param type Type of satellite image. Accepts "tc" (True color), "ir" (Infrared), "dc" (Deep convection), or any combinations.
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_satl()
hk_load_satl = function(time = weather2::tool_datetime(end = Sys.time(), by = "10 min", duration = "3 day"),
                        magn = c(2, 4, 8), type = c("tc", "ir", "dc"), list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(sum(magn == 2 | magn == 4 | magn == 8) != length(magn)){
    cli::cli_text('Error: {.var magn} can only be 2, 4, 8, or any combination.')
    cli::cli_bullets(c("x" = 'You supplied {.var {magn}}.'))
    return(invisible())
  }
  if(sum(type == "tc" | type == "ir" | type == "dc") != length(type)){
    cli::cli_text('Error: {.var type} can only be "tc", "ir", "dc, or any combination.')
    cli::cli_bullets(c("x" = 'You supplied {.var {type}}.'))
    return(invisible())
  }
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Additional variables
  dit = 10

  URL = tidyr::crossing(time = time, magn = magn, type = type) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong"),
                  Ltime_UTC = lubridate::with_tz(Ltime, tzone = "UTC"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  LDate_UTC = paste0(sprintf("%04d", lubridate::year(Ltime_UTC)),
                                     sprintf("%02d", lubridate::month(Ltime_UTC)),
                                     sprintf("%02d", lubridate::day(Ltime_UTC))),
                  LHour_UTC = paste0(sprintf("%02d", lubridate::hour(Ltime_UTC)),
                                     sprintf("%02d", lubridate::minute(Ltime_UTC)),
                                     "00"),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/intersat/satellite/image/images/h8L_",
                               type,"_x",magn,"M_",LDate_UTC, LHour_UTC,".png"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "SATL",
                               "/", "SATL", sprintf("%02d", magn), type,
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_SATL", sprintf("%02d", magn), type, "_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Info, URL, DIR)
  #Start to download
  title = paste0("Satellite Image_", stringr::str_flatten(sprintf("%02d", magn)), " ", stringr::str_flatten(type), " (HKO)")
  weather2::w2_load_file(data = URL, attempt = attempt, title = title,
                         list_fail = list_fail, worker = worker, check = F)
}
