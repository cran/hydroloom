## ----setup, include = FALSE---------------------------------------------------
library(hydroloom)
eval <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE") &
  requireNamespace("nhdplusTools", quietly = TRUE)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.width=4,
  fig.height=4,
  fig.align = "center",
  eval=eval
)

library(sf)

oldoption <- options(scipen = 9999)

## -----------------------------------------------------------------------------
library(hydroloom)
library(sf)

hy_net <- sf::read_sf(system.file("extdata/new_hope.gpkg",
                                  package = "hydroloom"))

nrow(hy_net)

class(hy_net)

names(hy_net)

class(hy(hy_net, clean = TRUE))

names(hy(hy_net, clean = TRUE))

# map utilities
map_prep <- \(x, tol = 100) sf::st_geometry(x) |> # no attributes
  sf::st_transform(3857) |> # basemap projection
  sf::st_simplify(dTolerance = tol) # sleaner rendering

pc <- list(flowline = list(col = NA)) # to hide flowlines in basemap

oldpar <- par(mar = c(0, 0, 0, 0)) # par is reset in cleanup
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)

plot(map_prep(hy_net), col = "blue", add = TRUE)


## -----------------------------------------------------------------------------
# work in hydroloom attribute names for demo sake
hy_net <- hy(hy_net)

# the smallest topo_sort is the most downstream
outlet <- hy_net[hy_net$topo_sort == min(hy_net$topo_sort), ]

# features with the levelpath of the outlet are the mainpath, 
# or mainstem of the network
main_path <- hy_net[hy_net$levelpath == outlet$levelpath, ]

# the largest topo sort along the main path is its headwater flowline
headwater <- main_path[main_path$topo_sort == max(main_path$topo_sort), ]

# basemap
par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)

# plot the elements prepped above
plot(map_prep(hy_net), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(outlet), col = "magenta", add = TRUE, lwd = 4)
plot(map_prep(headwater), col = "magenta", add = TRUE, lwd = 4)
plot(map_prep(main_path), col = "darkblue", add = TRUE, lwd = 1.5)


## -----------------------------------------------------------------------------

# this is just the ids
path <- navigate_hydro_network(hy_net, 
                               start = outlet$id, 
                               mode = "UM")

# filter the source data to get the id's representation 
path <- hy_net[hy_net$id %in% path, ]

# pathlength_km is the distance from the furthest downstream network outlet
# it is used within navigate_hydro_network to filter to a given distance.
pathlength <- max(path$pathlength_km) - min(path$pathlength_km)

half_path <- navigate_hydro_network(hy_net, 
                               start = outlet$id, 
                               mode = "UM", 
                               distance = pathlength / 2)

half_path <- hy_net[hy_net$id %in% half_path, ]

par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)
plot(map_prep(hy_net), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(half_path), col = "magenta", add = TRUE, lwd = 3)
plot(map_prep(path), col = "darkblue", add = TRUE, lwd = 2)


## -----------------------------------------------------------------------------

start <- half_path[half_path$topo_sort == max(half_path$topo_sort), ]

up <- navigate_hydro_network(hy_net, 
                             start = start$id, 
                             mode = "UT")
up <- hy_net[hy_net$id %in% up, ]

down <- navigate_hydro_network(hy_net,
                               start = start$id,
                               mode = "DD")
down <- hy_net[hy_net$id %in% down, ]

par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)
plot(map_prep(hy_net), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(start), col = "magenta", add = TRUE, lwd = 4)
plot(map_prep(up), col = "darkblue", add = TRUE, lwd = 2)
plot(map_prep(down), col = "blue", add = TRUE, lwd = 2)

## -----------------------------------------------------------------------------
hydroloom_name_definitions[names(hydroloom_name_definitions) == "upmain"]
hydroloom_name_definitions[names(hydroloom_name_definitions) == "downmain"]

## ----dend_fig, echo=FALSE-----------------------------------------------------

x <- c(2, 2, 3, 2, 2)
y <- c(5, 4, 3, 2, 1)
a <- c(1.4, 3.5)
b <- c(.9, 5.1)
main_col = "darkblue"
div_col = "purple"
make_edges <- function() {
  arrows(x[1], y[1] - .1, x[2], y[2] + .1, length = .1, col = main_col)
  arrows(x[2] + .1, y[2] - .1, x[3] - .1, y[3] + .1, length = .1, col = div_col) # right
  arrows(x[2] + .0, y[2] - .1, x[4] - .0, y[4] + .1, length = .1, col = main_col)
  arrows(x[3] - .1, y[3] - .1, x[4] + .1, y[4] + .1, length = .1, col = div_col)
  arrows(x[4], y[4] - .1, x[5], y[5] + .1, length = .1, col = main_col)
  text(c(2.1, 2.5, 2.5, 1.9, 2.1), c(4.5, 3.8, 2.3, 3, 1.5), c("1", "2", "3", "4", "5"))
}

oldpar <- par(mar = c(0, 0, 0, 0))
plot(a, b, col = NA)

make_edges()
par(oldpar)

## -----------------------------------------------------------------------------

# select only id, name, feature_type. 
# Note that the geometry is "sticky" and is included in base_net
base_net <- dplyr::select(hy_net, id, GNIS_NAME, feature_type)

# create a geometric network -- this includes divergences
base_net <- dplyr::left_join(make_attribute_topology(base_net, min_distance = 10),
                             dplyr::select(base_net, id), by = "id") |>
  sf::st_sf()

names(base_net)
nrow(base_net)

# now switch from a flownetwork topology to a node topology.
base_net <- hydroloom::make_node_topology(base_net, add_div = TRUE, add = TRUE)

names(base_net)
nrow(base_net)

# divergence determination needs a dominant feature type input
unique(base_net$feature_type)

base_net <- add_divergence(base_net, 
                           coastal_outlet_ids = outlet$id, 
                           inland_outlet_ids = c(), 
                           name_attr = "GNIS_NAME", 
                           type_attr = "feature_type", 
                           major_types = "StreamRiver")

names(base_net)
nrow(base_net)

# now we can add a dendritic toid attribute because we have "divergence"
base_net <- add_toids(base_net, return_dendritic = TRUE)

# note that no rows were added -- these are only downmain!
nrow(base_net)

# now add a length attribute as the accumulated flowline length.
base_net$length_km <- as.numeric(st_length(base_net) / 1000)
base_net$weight <- accumulate_downstream(base_net, "length_km")

base_net <- add_levelpaths(base_net, 
                           name_attribute = "GNIS_NAME",
                           weight_attribute = "weight")

names(base_net)

#remove dendritic toid used above
base_net <- dplyr::select(base_net, -toid)

flow_net <- to_flownetwork(base_net)

nrow(flow_net)
names(flow_net)

## -----------------------------------------------------------------------------

flow_net_nhdplus <- to_flownetwork(hy_net) |>
  dplyr::arrange(id, toid)

flow_net_hydroloom <- to_flownetwork(base_net) |>
  dplyr::arrange(id, toid)

different_downmain <- flow_net_nhdplus[flow_net_nhdplus$downmain != flow_net_hydroloom$downmain,]

different_downmain

different_upmain <- flow_net_nhdplus[flow_net_nhdplus$upmain != flow_net_hydroloom$upmain,]

different_upmain

different_upmain <- hy_net[hy_net$id %in% c(different_upmain$id, different_upmain$toid), ]

par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(different_upmain), plot_config = pc)
plot(map_prep(hy_net, 10), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(different_upmain, 10), col = "blue", add = TRUE, lwd = 2)

## -----------------------------------------------------------------------------

# this is just the ids
path <- navigate_network_dfs(flow_net, 
                             starts = outlet$id,
                             direction = "upmain")

# filter the source data to get the id's representation 
path <- hy_net[hy_net$id %in% unlist(path), ]

# distance not yet supported
half_path <- navigate_network_dfs(flow_net, 
                                  starts = 8893396, # chosen from a map
                                  direction = "downmain")

half_path <- hy_net[hy_net$id %in% unlist(half_path), ]

par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)
plot(map_prep(hy_net), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(half_path), col = "magenta", add = TRUE, lwd = 3)
plot(map_prep(path), col = "darkblue", add = TRUE, lwd = 2)


## -----------------------------------------------------------------------------
# chosen from map
start <- hy_net[hy_net$id == 8893396, ]

up <- navigate_network_dfs(flow_net, 
                           starts = start$id, 
                           direction = "up")
up <- hy_net[hy_net$id %in% unlist(up), ]

down <- navigate_network_dfs(flow_net,
                               starts = start$id,
                               direction = "down")
down <- hy_net[hy_net$id %in% unlist(down), ]

par(mar = c(0, 0, 0, 0))
nhdplusTools::plot_nhdplus(bbox = sf::st_bbox(hy_net), plot_config = pc)
plot(map_prep(hy_net), col = "dodgerblue2", add = TRUE, lwd = 0.5)
plot(map_prep(start), col = "magenta", add = TRUE, lwd = 4)
plot(map_prep(up), col = "darkblue", add = TRUE, lwd = 2)
plot(map_prep(down), col = "blue", add = TRUE, lwd = 2)

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)
par(oldpar)
if(!eval) {
  unlink(nhdplusTools::nhdplusTools_data_dir(), recursive = TRUE)
}

