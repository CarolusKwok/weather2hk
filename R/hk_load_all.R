#' HK weather - Downloading all information from HKO and SMG
#'
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_all(worker = 1) or hk_load_all(worker = 20)
hk_load_all = function(worker = 1){
  weather2hk::hk_load_ltng(worker = worker)
  weather2hk::hk_load_satl(worker = worker)
  weather2hk::hk_load_rain_dy(worker = worker)
  weather2hk::hk_load_rain_hr(worker = worker)

  weather2hk::hk_load_gtmp_csv(worker = worker)
  weather2hk::hk_load_mslp_csv(worker = worker)
  weather2hk::hk_load_rain_csv(worker = worker)
  weather2hk::hk_load_rhum_csv(worker = worker)
  weather2hk::hk_load_sart_csv(worker = worker)
  weather2hk::hk_load_temp_csv(worker = worker)
  weather2hk::hk_load_tide_csv(worker = worker)
  weather2hk::hk_load_wind_csv(worker = worker)

  weather2hk::mo_load_tide(worker = worker)
}
