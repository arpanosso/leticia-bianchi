# Data input
data_set <- readr::read_csv2("data-raw/Banco de dados.csv", na = 'NA') |>
  tidyr::drop_na()

# discriptive statistic
skimr::skim((data_set))

# glimpse
dplyr::glimpse(data_set)

# data prep
data_set <- data_set |>
  tidyr::drop_na() |>
  dplyr::mutate(
    day = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,1]),
    month = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,2]),
    year = as.numeric(stringr::str_split(date,"/",simplify = TRUE)[,3]),
    date = lubridate::make_date(year,month,day)
  )

# save my data
readr::write_rds(data_set,"data/my_data_set.rds")
