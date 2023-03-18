#' Dictionary of HK tidal stations
#'
#' @return
#' @export
#'
#' @examples hk_dict_tide()
hk_dict_tide = function(){
  data = tibble::tibble(CLK = c("Chek Lap Kok (E)", 113.9452778, 22.3205556),
                        CCH = c(     "Cheung Chau", 114.0230556, 22.2141667),
                        CMW = c(      "Chi Ma Wan", 113.9999115, 22.2395810),
                        KLW = c(      "Ko Lau Wan", 114.3608333, 22.4586111),
                        KCT = c(      "Kwai Chung", 114.1227778, 22.3236111),
                        LOP = c(      "Lok On Pai", 114.0016415, 22.3614868),
                        MWC = c(          "Ma Wan", 114.0713889, 22.3638889),
                        QUB = c(      "Quarry Bay", 114.2133333, 22.2911111),
                        SPW = c(        "Shek Pik", 113.8944444, 22.2202778),
                        TMW = c(     "Tai Miu Wan", 114.2886111, 22.2697222),
                        TAO = c(           "Tai O", 113.8655556, 22.2550000),
                        TPK = c(      "Tai Po Kau", 114.1838889, 22.4425000),
                        TBT = c(   "Tsim Bei Tsui", 114.0141667, 22.4872222),
                        WAG = c(   "Waglan Island", 114.3027778, 22.1830556))
  data2 = t(data) %>%
    as.data.frame() %>%
    dplyr::mutate(name = rownames(.)) %>%
    dplyr::rename(full_name = V1,
                  Lon = V2,
                  Lat = V3) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Lon = as.numeric(Lon),
                  Lat = as.numeric(Lat)) %>%
    dplyr::relocate(name, full_name, Lon, Lat)
  return(data2)
}
