#leitura da lista das micrroregiões do Matopiba
list_micro <-
  readr::read_csv("dados/micro.CSV", locale = readr::locale(encoding = "latin1"))

#criação de lista com os códigos das microrregiões
codmicro <- list_micro$CD_MICRO

#acesso a feições das microrregiões através do pacote geobr e laço de repetição
micromtp <- NULL
temp <- NULL
for (i in 1:length(codmicro)) {
  temp <-
    geobr::read_micro_region(code_micro = codmicro[i], year = 2020)
  micromtp <- rbind(micromtp, temp)
}

#salvar shapefile das microrregiões do Matopiba
sf::st_write(micromtp,
             here::here("dados", "shp", "microrregioes_matopiba.shp"))
