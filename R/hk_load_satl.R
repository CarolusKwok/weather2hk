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
                        magn = c(2, 4, 8), type = c("tc", "ir", "dc"), list_fail = T, dir = getwd(), attempt = 5L, worker = 1L){
  #Check
  if(weather2hk::sys_ckf_HKLoad(time, list_fail = list_fail, attempt = attempt, worker = worker)){return(invisible())}
  if(weather2::sys_ckl_ItemIn(list = magn, list_name = "magn", expected = c(2,4,8), mode = "in")){return(invisible())}
  if(weather2::sys_ckl_ItemIn(list = type, list_name = "type", expected = c("tc", "ir", "dc"), mode = "in")){return(invisible())}

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
  weather2::sys_load_file(data = URL, attempt = attempt, title = title,
                          list_fail = list_fail, worker = worker, check = F)
}
