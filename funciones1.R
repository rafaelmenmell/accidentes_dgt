library(ggplot2)
library(ggridges)

LeeMicrodatos <- function(year){
  file <- sprintf("MICRODATOS_ACC_VICT_%s.zip",year)
  lf <- unzip(file,list = TRUE)
  unzip(file)
  accidentes <- read.csv(sprintf("TABLA_ACCVICT_%s.csv",year),header = TRUE,sep=";")
  vehiculos <- read.csv(sprintf("TABLA_VEHIC_%s.csv",year),header = TRUE,sep=";")
  victimas <- read.csv(sprintf("TABLA_PERS_%s.csv",year),header = TRUE,sep=";")
  microdatos <- list(accidentes=accidentes,vehiculos=vehiculos,victimas=victimas)
  unlink(lf$Name)
  return(microdatos)
}

LeeMicrodatosTodos <- function(){
  if(!file.exists("microdatos_accidentes.rds")){
    year <- 2011:2015
    microdatos <- year %>% purrr::map(LeeMicrodatos)
    names(microdatos) <- year
    saveRDS(microdatos,"microdatos_accidentes.rds")
  } else {
    microdatos <- readRDS("microdatos_accidentes.rds")
  }
  return(microdatos)
}

#este grafico es un mapa de calor por mes, dia y hora del número de accidentes
Grafico1 <- function(datos){
  accidentes.g <- accidentes %>% dplyr::group_by(MES,DIASEMANA,HORA) %>% dplyr::summarise(total=n())
  g1 <- ggplot(accidentes.g) + aes(x=HORA) + aes(y=DIASEMANA) + aes(fill=total) + geom_tile() + facet_grid(MES~.) + scale_fill_viridis_c(,option = "C") + theme_minimal()
  return(g1)
}

#lo mismo con ridges que están de moda
Grafico2 <- function(datos){
  accidentes.g <- accidentes %>% dplyr::group_by(DIASEMANA,HORA) %>% dplyr::summarise(total=n())
  g1 <- ggplot(accidentes.g) + geom_density_ridges(aes(x=HORA,y=as.factor(DIASEMANA),height=total),stat = "identity") + theme_minimal() + ylab("DIA DE LA SEMANA")
  return(g1)
}


#mapa de calor general
microdatos <- LeeMicrodatosTodos()
accidentes <- lapply(microdatos, function(x) x$accidentes) %>% bind_rows()
g1 <- Grafico1(datos = accidentes)
g1 <- g1 + labs(title = "Número total de accidentes con víctimas",subtitle = "2011-2015",caption = "Fuente: DGT")
