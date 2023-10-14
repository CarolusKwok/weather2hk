#' Check if items within the HKLoad or MOLoad are valid
#'
#' @param time time within the function
#' @param list_fail list_fail within the function
#' @param attempt attempt within the function
#' @param worker worker within the function
#'
#' @return
#' @export
#'
#' @examples sys_ckf_HKLoad(time, list_fail, attempt, worker)
sys_ckf_HKLoad = function(time, list_fail, attempt, worker){
  if(weather2::sys_ckc_POSIXct(value = time, value_name = "time")){return(T)}
  if(weather2::sys_ckc_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys_ckc_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys_ckc_integer(value = worker, value_name = "worker")){return(T)}
  return(F)
}




#' Check if language is supported
#'
#' @param lan lan within the function
#'
#' @return
#' @export
#'
#' @examples sys_ckf_HKLoadLan(lan)
sys_ckf_HKLoadLan = function(lan){
  if(weather2::sys_ckl_length(list = lan, list_name = "lan", expected = 1L, mode = "==")){return(T)}
  if(weather2::sys_ckl_ItemIn(list = lan, list_name = "lan", expected = c("tc", "sc", "en"), mode = "in")){return(T)}
  return(F)
}
