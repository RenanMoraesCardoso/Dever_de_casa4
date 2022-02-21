if(!require("pacman"))install.packages("pacman")
pacman::p_load(terra,spData)
install.packages("spDataLarge")
install.packages("geobr")
install.packages("devtools")
install.packages("ggplot2")
library("terra")
library("raster")
library("spDataLarge")
library("geobr")
library("sf")
library("devtools")
library("geobr")
library("dplyr")
library("tidyverse")
library("ggplot2")
utils::remove.packages('geobr')
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

raster_filepath="C:/Users/renan/OneDrive/Documentos/Economia/Mestrado/Estatistica/brasil_coverage_2020.tif"
my_rast=rast(raster_filepath)

plot(my_rast)

rj = read_municipality(code_muni="RJ", year = 2020)
rj_vect = vect(rj)
plot(rj_vect)
srtm_crop = crop(my_rast, rj_vect)
plot(srtm_crop)

srtm_masked = mask(srtm_crop, rj_vect)
plot(srtm_masked)

srtm_extracted = extract(srtm_masked, rj_vect)

pixel_municipios = srtm_extracted %>%
  group_by(ID) %>% summarise(numero_pixel = n())
pixel_florestais = srtm_extracted %>%
  group_by(ID) %>% 
  filter(brasil_coverage_2020 %in% c(3,4,5,49)) %>% 
  summarise(n_forest = n())

rj =  rj %>%
  mutate(ID = 1:92) %>% 
  left_join(pixel_municipios, by = "ID")

rj =  rj %>%
  mutate(ID = 1:92) %>% 
  left_join(pixel_florestais, by = "ID")


rj = rj %>%
  mutate(razao_florestal = (n_forest/numero_pixel))

ggplot(rj) +
  geom_sf(aes(fill = razao_florestal)) +
  labs(title = "Cobertura vegetal por município do RJ")

Tabela = rj %>%
  select(name_muni, name_state, razao_florestal)
write.csv(Tabela, file = 'Tabela.csv')
