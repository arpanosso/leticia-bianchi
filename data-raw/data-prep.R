# Data input
data_set <- readr::read_csv("data-raw/banco_de_dados_novo.csv", na = 'NA') |>
  janitor::clean_names()
  tidyr::drop_na()

# discriptive statistic
skimr::skim((data_set))

# glimpse
dplyr::glimpse(data_set)

# data prep
data_set <- data_set |>
  tidyr::drop_na() |>
  dplyr::mutate(
    mestre = stringr::str_detect(station, "(m\\/e)"),
    day = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,1]),
    month = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,2]),
    year = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,3]),
    date = lubridate::make_date(year,month,day)
  )

# save my data
readr::write_rds(data_set,"data/my_data_set_novo.rds")

