library(magrittr)

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt",
                              readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                              na = "-99")

storm_observation <- ext_tracks %>%
    dplyr::mutate(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-")) %>%
    dplyr::mutate(longitude = -longitude) %>%
    tidyr::unite(date, year, month, day, hour) %>%
    dplyr::mutate(date = lubridate::ymd_h(date)) %>%
    dplyr::select(storm_id, date, latitude, longitude, dplyr::starts_with("radius")) %>%
    tidyr::gather(wind_key, radius, -storm_id, -date, -latitude, -longitude) %>%
    tidyr::extract(wind_key, c("wind_speed", "wind_direction"), regex = "radius_([0-9]+)_([a-z]{2})", convert = TRUE) %>%
    tidyr::spread(wind_direction, radius)


storm_observation %>%
    dplyr::filter(storm_id == 'Katrina-2005' & date == lubridate::ymd_hm("2005-08-29 12:00")) %>%
    head %>%
    print
