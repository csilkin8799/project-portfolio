library(magrittr)

contracts <- nflreadr::load_contracts()%>%
  dplyr::filter(is_active == TRUE)

cb_contracts <- contracts%>%
  dplyr::filter(position == "CB")%>%
  dplyr::arrange(-apy_cap_pct)

cb_contracts <- cb_contracts%>%
  dplyr::mutate(clean_names = nflreadr::clean_player_names(player))%>%
  dplyr::select(player, clean_names, dplyr::everything())

non_rookie_cb_contracts <- cb_contracts%>%
  dplyr::filter(year_signed != draft_year)

nfl_roster_data <- nflreadr::load_rosters()

non_rookie_cb_contracts <- non_rookie_cb_contracts%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::select(player, clean_names, full_name, -date_of_birth, dplyr::everything())

non_rookie_cb_contracts <- non_rookie_cb_contracts%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)

non_rookie_rb_contracts <- contracts%>%
  dplyr::filter(position == "RB")%>%
  dplyr::filter(year_signed != draft_year)%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)%>%
  dplyr::arrange(-apy_cap_pct)

non_rookie_safety_contracts <- contracts%>%
  dplyr::filter(position == "S")%>%
  dplyr::filter(year_signed != draft_year)%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)%>%
  dplyr::arrange(-apy_cap_pct)

non_rookie_guard_contracts <- contracts%>%
  dplyr::filter(position %in% c("LG", "RG"))%>%
  dplyr::filter(year_signed != draft_year)%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)%>%
  dplyr::arrange(-apy_cap_pct)

non_rookie_idl_contracts <- contracts%>%
  dplyr::filter(position == "IDL")%>%
  dplyr::filter(year_signed != draft_year)%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)%>%
  dplyr::arrange(-apy_cap_pct)

non_rookie_qb_contracts <- contracts%>%
  dplyr::filter(position == "QB")%>%
  dplyr::filter(year_signed != draft_year)%>%
  dplyr::inner_join(nfl_roster_data%>%dplyr::select(full_name, gsis_id, birth_date), by = dplyr::join_by(gsis_id))%>%
  dplyr::mutate(age = lubridate::interval(birth_date, lubridate::today()) / lubridate::years(1))%>%
  dplyr::mutate(
    sep1 = paste0(year_signed, "-09-01"),
    age_at_signing = lubridate::interval(birth_date, sep1) / lubridate::years(1)
  )%>%
  dplyr::select(-sep1)%>%
  dplyr::arrange(-apy_cap_pct)
