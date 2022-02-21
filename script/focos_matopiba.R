#leitura do shapefile das microrregioes
microrregioes <-
  sf::st_read(here::here("dados", "shp", "microrregioes_matopiba.shp"))

#leitura do csv dos focos de queimadas
#leitura de arquivo csv de cada ano e vinculando em um unico objeto
tbl <-
  list.files(path = "dados/csv/",
             pattern = "*.csv",
             full.names = T) |>
  purrr::map_df( ~ readr::read_csv(., col_types = readr::cols(.default = "c")))

#filtrando por estado
tbl <- tbl |>
  dplyr::filter(estado == 'TOCANTINS' |
                  estado == 'MARANHAO' |
                  estado == 'BAHIA' | estado == 'PIAUI')

#convertendo em arquivo simple feature
tbl <-
  sf::st_as_sf(tbl,
               coords = c("longitude", "latitude"),
               crs = 4326)

#convertendo para mesmo crs (SIRGAS 2000 / Brazil Polyconic)
micromtp <- sf::st_transform(microrregioes, crs = 5880)
tbl <- sf::st_transform(tbl, crs = 5880)

#criando objeto com os focos que interceptam as microrregiões do matopiba
focos <- sf::st_intersection(tbl, micromtp)

#convertendo coleção em objeto
focos <- sf::st_collection_extract(focos, "POINT")

#coluna com ano e mes
focos$ano_mes <- stringr::str_sub(focos$datahor, end = 7)

# Seleção de colunas e cria novo dataframe
focosdt <- focos |>
  dplyr::select(nm_micr, cd_micr, ano_mes)
focosdt <- as.data.frame(focosdt)

# Dataframe com contagem de focos por mes/ano e por microrregião
focoscount <- focosdt |>
  dplyr::group_by(nm_micr) |>
  dplyr::count(ano_mes)

# salvar em formato csv
readr::write_csv(focoscount,
                 here::here("dados", "csv", "focos_ano_matopiba.csv"))
