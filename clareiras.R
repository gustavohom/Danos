library(raster)
library(sf)
library(dplyr)

# carregando arquivo ------------------------------------------------------

dossel_a <- raster::raster('Canopy_pre.tif')
dossel_p <- raster::raster('Canopy_pos.tif')


# filtrando alturas inferiores a 10 m -------------------------------------

# reclassificando o raster 

## todos os valores > minimo e <= maximo  reclassificar como dano

altura_minima <- 10

tbl_reclass <- c(-999, altura_minima, 1,
                            altura_minima, 999, 0)

reclass <- matrix(tbl_reclass, ncol = 3, byrow = TRUE)

dossel_a_reclass <- raster::reclassify(dossel_a, reclass)
dossel_p_reclass <- raster::reclassify(dossel_p, reclass)
dossel_a_reclass[dossel_a_reclass == 0] <- NA
dossel_p_reclass[dossel_p_reclass == 0] <- NA


# agrupando feicoes -------------------------------------------------------

rst_clump_a <- raster::clump(dossel_a_reclass, directions= 4) # 4 ou 8
rst_clump_p <- raster::clump(dossel_p_reclass, directions= 4)


# poligonizando -----------------------------------------------------------

clareira_a <- raster::rasterToPolygons(x = rst_clump_a,
                                            na.rm = TRUE, dissolve = TRUE)

clareira_p <- raster::rasterToPolygons(x = rst_clump_p,
                                           na.rm = TRUE,dissolve = TRUE)


clareira_a <- sf::st_as_sf(clareira_a, merge = TRUE)
clareira_p <- sf::st_as_sf(clareira_p, merge = TRUE)


# filtrando areas menores que 10 m2 ---------------------------------------


clareira_a$area <- as.numeric(sf::st_area(clareira_a))
clareira_p$area <- as.numeric(sf::st_area(clareira_p))

clareira_a_flt <- dplyr::filter(clareira_a, area >= 10)
clareira_p_flt <- dplyr::filter(clareira_p, area >= 10)

clareira_a_flt$soma_area <- sum(clareira_a_flt$area)/10000
clareira_p_flt$soma_area <- sum(clareira_p_flt$area)/10000


# tabela de area ----------------------------------------------------------


area_clareiras <- data.frame(
                    clareiras = c('antes', 'apos'),
                    area = c(clareira_a_flt$soma_area[1], clareira_p_flt$soma_area[1]),
                    diferenca = clareira_p_flt$soma_area[1] - clareira_a_flt$soma_area[1]
                  )

# salvando arquivos -------------------------------------------------------

sf::write_sf(clareira_a_flt,'C:\\produto\\clareiras_pre.shp')
sf::write_sf(clareira_p_flt,'C:\\produto\\clareiras_pos.shp')
