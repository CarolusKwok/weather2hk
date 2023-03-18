#' HK weather - Downloading wind distribution images from https://www.hko.gov.hk/en/index.html
#'
#' @param lan Language. Accepts "en", "tc", "sc".
#' @param type Type of satellite image. Accepts "tc" (True color), "ir" (Infrared), "dc" (Deep convection), or any combinations.
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_wind()
hk_load_wind = function(lan = "en", type = c("wind", "gust"), list_fail = T, dir = getwd(), attempt = 5, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_lan(lan, "lan")){return(invisible())}
  if(sum(type == "wind" | type == "gust") != length(type)){
    cli::cli_text('Error: {.var type} can only be "wind", "gust", or any combination.')
    cli::cli_bullets(c("x" = 'You supplied {.var {type}}.'))
    return(invisible())
  }
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Additional variables
  comb = unique(type)
  lan = ifelse(lan == "sc", "tc", lan)
  nlan = ifelse(lan == "en", "e",
         ifelse(lan == "tc", "c",
         ifelse(lan == "sc", "c", NA)))

  for(i in 1:length(comb)){
    #Create time and force time to be HKT
    type = comb[i]
    if(type == "wind"){
      time = weather2::tool_datetime(end = Sys.time(), by = "1 hour", duration = "1 day")
      time = time[2:(length(time))]
    }
    if(type == "gust"){
      time = weather2::tool_datetime(end = Sys.time(), by = "10 min", duration = "1 day")
      time = time[1:(length(time)-1)]
    }
    time = lubridate::with_tz(time = time, tzone = "HongKong")

    dit = ifelse(type == "wind", 60,
          ifelse(type == "gust", 10, NA))
    url_s = ifelse(type == "wind", "https://www.hko.gov.hk/wxinfo/ts/wind",
            ifelse(type == "gust", "https://www.hko.gov.hk/wxinfo/ts/windgust/gust", NA))

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
                    URL = paste0(url_s, nlan, "hk_", LHour, ".png"),
                    DIR = paste0(dir,
                                 "/", "HK_Data",
                                 "/", "WIND",
                                 "/", "WIND(", type, ")", lan,
                                 "/", substr(LDate, 1, 4),
                                 "/", substr(LDate, 1, 6),
                                 "/", LDate,
                                 "/", "HK_WIND(", type, ")", lan, "_", LDate, "_", LHour, ".png")) %>%
      dplyr::select(Info, URL, DIR) %>%
      dplyr::distinct()


    #https://www.hko.gov.hk/wxinfo/ts/windehk_1600.png
    #https://www.hko.gov.hk/wxinfo/ts/windgust/gustehk_2230.png

    #Start to download
    #return(URL)
    weather2::w2_load_file(data = URL, attempt = attempt, title = paste0("Wind distribution_", type, " (HKO)"),
                           list_fail = list_fail, worker = worker, check = F)
  }
}
