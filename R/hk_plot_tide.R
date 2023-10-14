#' Plots HK predicted tide, based on weeks
#'
#' A common function used in SKLMP
#'
#' @param station Station to download. Check the `hk_dict_tide` for more information
#' @param year Year to download, in `integer`
#' @param weeks Week to display, in `integer`
#' @param range Range of the appropriate tidal level. Default (`NULL`) assumes that any tidal level will be appropriate.
#' @param draw_rect Draw a shading rectangle between 18:00 to 06:00 next day to represent night time? Default as `TRUE`
#'
#' @return
#' @export
#'
#' @examples hk_plot_tide("CLK", 2020L, weeks = 10L:20L, range = c(0, 1), draw_rect = T)
hk_plot_tide = function(station, year, weeks, range = NULL, draw_rect = T){
  #Check ####
  stations = weather2hk::hk_dict_tide()$name
  if(weather2::sys_ckl_ItemIn(station, "station", stations)){return(invisible())}
  if(weather2::sys_ckl_length(station, "station", expected = 1L)){return(invisible())}
  if(weather2::sys_ckl_ItemIn(year, "year", expected = 2015L:2025L)){return(invisible())}
  if(weather2::sys_ckc_integer(weeks, "weeks")){return(invisible())}
  if(weather2::sys_ckc_numeric(value = range, value_name = "range")){return(invisible())}
  if(weather2::sys_ckl_length(range, "range", expected = 2L)){return(invisible())}

  #Start Drawing ####
  date_start = ISOdatetime(year-1, 12, 30, 22, 00, 00, tz = "")
  date_end   = ISOdatetime(year+1, 01, 01, 02, 00, 00, tz = "")

  data0= weather2hk::hk_load_tide_predict(station = station, year = year, mode = c("hl", "hr"))
  ylim = c(min(data0$tide), max(data0$tide))
  if(is.null(range)){
    range = ylim
  }
  data1= tibble::tibble(time = weather2::tool_datetime(start = date_start,
                                                       end   = date_end,
                                                       by    = "1 mins")) %>%
    dplyr::filter(!(time %in% data0$time))
  plot = dplyr::bind_rows(dplyr::distinct(data0), data1) %>%
    dplyr::filter(lubridate::week(time) %in% weeks,
                  lubridate::year(time) %in% year) %>%
    weather2::calc_smooth_lm(based = time, value = tide) %>%
    dplyr::mutate(fill = (min(range) <= slm_tide) & (slm_tide <= max(range)),
                  week = lubridate::week(time))  %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = slm_tide)) +
    ggplot2::geom_line(ggplot2::aes(color = fill, group = 1), show.legend = F)+
    ggplot2::scale_x_datetime(date_labels = "%m%d",
                              breaks = "1 day",
                              minor_breaks = weather2::tool_datetime(start = date_start,
                                                                       end = date_end,
                                                                        by = "2 hour"),
                              oob = scales::squish_infinite)+
    ggplot2::scale_y_continuous(breaks = seq(-10, 10, 0.5),
                                minor_breaks = seq(-10, 10, 0.1),
                                limits = ylim,
                                oob = scales::squish_infinite) +
    ggplot2::scale_color_manual(breaks = c(T, F),
                                values = c("#00AA00", "red"))+
    ggplot2::coord_cartesian(expand = F)+
    ggplot2::facet_wrap(facets = ~ paste("Week:", week), scales = "free")+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Time (labelled @ 00 hour)",
                  y = "Tide (mCD)")

  if(draw_rect){
    Rest1 = tibble::tibble(Date = weather2::tool_date(start = date_start, end = date_end, by = "1 day")) %>%
      dplyr::bind_rows(.,.,.,.) %>%
      dplyr::arrange(Date) %>%
      dplyr::slice(3:(nrow(.)-2)) %>%
      dplyr::mutate(num = 1:dplyr::n(),
                    grp = ceiling(num/4),
                    height = rep(c(-1, 10, 10, -1), as.numeric(date_end - date_start)),
                    hour = rep(c(18, 18, 6, 6), as.numeric(date_end - date_start)),
                    time = lubridate::with_tz(Date, tzone = "Asia/Hong_Kong"),
                    time = time - lubridate::hours(8) + lubridate::hours(hour),
                    week = lubridate::week(time))
    Rest2 = dplyr::group_by(Rest1, grp) %>%
      dplyr::summarise(Date = max(Date)) %>%
      dplyr::mutate(week = lubridate::week(Date)) %>%
      dplyr::select(-Date)
    Rest1 = dplyr::select(Rest1, -week) %>%
      dplyr::left_join(x = ., y = Rest2, by = "grp")
    Rest2 = dplyr::group_by(Rest1, week) %>%
      dplyr::filter(grp == min(grp)) %>%
      dplyr::mutate(week = week - 1)
    Rest1 = dplyr::bind_rows(Rest1, Rest2)

    plot = plot +
      ggplot2::geom_polygon(data = dplyr::filter(Rest1, week %in% weeks),
                            ggplot2::aes(x = time, y = height, group = grp), alpha = 0.2)
  }


  if(min(weeks) == max(weeks)){
    plot = plot +
      ggplot2::labs(title = paste0("Tides @ ", station, " for Weeks ", min(weeks), " in ", year))
  } else {
    plot = plot +
      ggplot2::labs(title = paste0("Tides @ ", station, " for Weeks ", min(weeks), " to ", max(weeks), " in ", year))
  }
  return(plot)
}
