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
                  estado == 'MARANHAO' | estado == 'BAHIA' | estado == 'PIAUI')

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

#salvar shp
sf::st_write(focos, here::here("dados", "shp", "focos_matopiba.shp"))

#conversão para data
focos<-focos|>
  dplyr::rename(ano_mes_dia = ano_mes)
focos$ano_mes_dia<-as.Date(focos$datahora, "%Y/%m/%d")
focos$ano<-lubridate::year(focos$ano_mes_dia)

#gráfico
library(ggplot2)
library(ggspatial)
x<-as.character(c(2011:2021))

g[[1]]<-ggplot()+
  geom_sf(data=focos[focos$ano==x[1],], aes(color = nm_micro), size = 0.1)+
  geom_sf(data=micromtp, fill = NA)+
  ggtitle(x[1])+
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 28),
        legend.position="")

g <- list()
for(i in 2:10){
  g[[i]]<-ggplot()+
    geom_sf(data=focos[focos$ano==x[i],], aes(color = nm_micro), size = 0.1)+
    geom_sf(data=micromtp, fill = NA)+
    ggtitle(x[i])+
    theme(text = element_text(size = 20),
          plot.title = element_text(size = 28),
          legend.position="")
}
g[[11]]<-ggplot()+
  geom_sf(data=focos[focos$ano==x[11],], aes(color = nm_micro), size = 0.1)+
  geom_sf(data=micromtp, fill = NA)+
  ggtitle(x[11])+
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 28),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24))+
  labs(color = "Microrregiões")

grid1<-gridExtra::grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]],
                               g[[5]], g[[6]], g[[7]], g[[8]],
                               g[[9]], g[[10]], g[[11]],
                              nrow = 3)
grid2<-gridExtra::grid.arrange(g[[1]], g[[2]], g[[3]],
                               g[[4]], g[[5]], g[[6]],
                               g[[7]], g[[8]], g[[9]],
                               g[[10]], g[[11]],
                               nrow = 4)
grid3<-gridExtra::grid.arrange(g[[9]], g[[10]], g[[11]],
                               nrow = 1)

#criando um gif
library(ggplot2)
library(scales)
library(gganimate)

p <- ggplot() +
  geom_sf(data=focos, aes(color = nm_micro), size = 0.1)+
  geom_sf(data=micromtp, fill = NA)+
  theme(legend.position="none")+
  labs(title = "ano:{frame_time}")+
  transition_time(ano) +
  ease_aes('linear')

anim_save("teste.gif")


#limpeza de dados

# focosm<-as.data.frame(focos)
# focosm$ano_mes<-as.Date(focos$datahora, "%Y/%m")
# focosm$mes<-lubridate::month(focos$ano_mes)
# focosm$ano<-lubridate::year(focos$ano_mes)

focosm<- focos|>
  mutate(ano_mes = as.Data(datahora,"%Y/%m" ))|>
  mutate(mes = lubridate::month(ano_mes))|>
  mutate(ano = lubridate::year(ano_mes))|>
  select(name_micro, ano_mes,mes,ano)


teste<-focos|>
  as.data.frame()|>
  dplyr::group_by(name_micro)|>
  dplyr::select(name_micro,year)|>
  dplyr::count(year, sort = TRUE) |> #conta as linhas de cada espécie ordenando de maior para menor
  tidyr::pivot_wider(names_from = "year",
                     values_from = "n",
                     names_prefix = "a")|>
  dplyr::select(name_micro,a2011,a2012,a2013,a2014,a2015,a2016,a2017,a2018,a2019,a2020,a2021)|>
  dplyr::mutate(Total = sum(a2011:a2021))|>
  dplyr::rename(Microrregiões = name_micro)


DT::datatable(teste, options = list(scrollX=TRUE))|>
  DT::formatCurrency(c("a2011","a2012", "a2013", "a2014","a2015", "a2016","a2017","a2018","a2019","a2020","a2021" , "Total"),
                     currency = "",
                     interval = 3,
                     mark = ".",
                     digits = 0)



library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(focos, aes(x = month, y = name_micro, fill = name_micro)) +
  geom_density_ridges(stat="binline",bins = 10) +
  theme_ridges() +
  theme(legend.position = "none")
