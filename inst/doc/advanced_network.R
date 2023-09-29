## ----setup, include = FALSE---------------------------------------------------
library(hydroloom)
library(dplyr)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.width=6, 
  fig.height=6, 
  fig.align="center",
  eval=local
)

oldoption <- options(scipen = 9999)


## ----sort, echo=FALSE, eval=TRUE, fig.cap="Smaller 'topo_sort' values are guaranteed to be downstream of larger values along connected paths.", fig.dim=c(3, 3)----
x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
x <- add_toids(x)
x <- sort_network(x)
x$topo_sort <- seq(nrow(x), 1)
oldpar <- par(mar = c(0, 0, 0, 0))
plot(sf::st_geometry(x), col = NA)
plot(x["topo_sort"], add = TRUE, lwd = 2)
par(oldpar)

## ----lp, echo=FALSE, eval=TRUE, fig.cap="Levelpath values are constant along mainstem paths and are derived from the topo_sort of their outlet flowline.", fig.dim=c(3, 3)----
x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))
x <- add_toids(x)

lp <- data.frame(lp = sort(unique(x$levelpath)))
lp$lpid <- seq_len(nrow(lp))
x <- dplyr::left_join(x, lp, by = c("levelpath" = "lp"))

oldpar <- par(mar = c(0, 0, 0, 0))
plot(sf::st_geometry(x), col = NA)
plot(x["lpid"], add = TRUE, lwd = 2)
par(oldpar)

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
# Import data
x <- hy(sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")))

# Strip the data back to the required base attributes
fpath <- hydroloom::add_toids(
  dplyr::select(x, id, fromnode, tonode, divergence, feature_type,
                da_sqkm, length_km, GNIS_ID)
)

# Print
head(fpath <- select(sf::st_cast(fpath, "LINESTRING"), 
                     -tonode, -fromnode, -divergence, -feature_type))

## -----------------------------------------------------------------------------
head(fpath <- sort_network(fpath, split = TRUE))

## ---- echo = TRUE, fig.dim=c(3, 3)--------------------------------------------
fpath['topo_sort'] <- seq(nrow(fpath), 1)
plot(fpath['topo_sort'], key.pos = NULL)

## -----------------------------------------------------------------------------
# Rename and compute weight
fpath$arbolatesum <- accumulate_downstream(
  dplyr::select(fpath, 
                id, toid, length_km), "length_km")

plot(sf::st_geometry(fpath), lwd = fpath$arbolatesum / 10)

## ----levelpath, fig.show="hold", out.width="45%"------------------------------
# Get levelpaths
fpath <- add_levelpaths(fpath, name_attribute = "GNIS_ID",weight_attribute = "arbolatesum", 
  status = FALSE, override_factor = 5)

# Print
head(fpath)

plot(fpath["topo_sort"], key.pos = NULL, reset = FALSE)
plot(fpath["levelpath"], key.pos = NULL)

## -----------------------------------------------------------------------------
# Invert plotting order
fpath <- dplyr::arrange(fpath, topo_sort) 

# Level Paths with more then 2 flowlines
lp <- dplyr::group_by(fpath, levelpath) %>%
dplyr::filter(n() > 2) 

# Unique Level Path ID
lp <-  unique(lp$levelpath)

# Terminal flowline 
terminal_fpath <- dplyr::filter(fpath, id %in% terminal_id)

gif_file <- "levelpath.gif"

gifski::save_gif({
  for(i in 1:length(lp)) {
    lp_plot <- dplyr::filter(fpath, levelpath == lp[i])

    outlet_plot <- dplyr::filter(lp_plot, id %in% terminal_id)

    plot(sf::st_geometry(fpath), lwd = 0.5, col = "grey")
    plot(sf::st_geometry(terminal_fpath), lwd = 3, col = "red", add = TRUE)
    plot(sf::st_geometry(dplyr::filter(fpath, levelpath %in% lp[1:i])), add = TRUE)
    plot(sf::st_geometry(lp_plot), col = "blue", add = TRUE)
    plot(sf::st_geometry(outlet_plot), col = "red", lwd = 1.5, add = TRUE)
  }
}, gif_file, delay = 0.5)

knitr::include_graphics(gif_file)

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

if(Sys.getenv("BUILD_VIGNETTES") != "TRUE") {
  unlink(work_dir, recursive = TRUE)
}

