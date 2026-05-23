## ----setup, include = FALSE---------------------------------------------------
library(hydroloom)
library(dplyr)

local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.align = "center",
  eval = local
)

oldoption <- options(scipen = 9999)

## ----synth_node, eval=TRUE----------------------------------------------------
library(hydroloom)

node_df <- data.frame(
  id         = c(1, 2, 3, 4, 5),
  fromnode   = c("N1", "N2", "N3", "N2", "N4"),
  tonode     = c("N2", "N3", "N4", "N4", "N5"),
  divergence = c(0,  2,  0,  1,  0)
)

x_node <- hy(node_df)
class(x_node)
nrow(x_node)

## ----synth_topo, eval=TRUE----------------------------------------------------
x_topo <- add_toids(x_node, return_dendritic = TRUE)
class(x_topo)
nrow(x_topo)
x_topo[, c("id", "toid")]

## ----synth_fn, eval=TRUE------------------------------------------------------
x_fn <- add_toids(x_node, return_dendritic = FALSE)
class(x_fn)
nrow(x_fn)
x_fn[, c("id", "toid")]

## -----------------------------------------------------------------------------
x <- sf::read_sf(system.file("extdata/new_hope.gpkg",
  package = "hydroloom"))

# First we select only an id, a name, and a feature type.
flow_net <- x |>
  select(COMID, GNIS_ID, FTYPE) |>
  sf::st_transform(5070)

# Now we convert the geometric network to an attribute topology
# and convert that to a node topology and join our attributes back
flow_net <- flow_net |>
  make_attribute_topology(min_distance = 5) |>
  hydroloom::make_node_topology(add_div = TRUE) |>
  left_join(sf::st_drop_geometry(flow_net), by = "COMID")

# We only have one outlet so it doesn't matter if it is coastal
# or inland but we have to provide it.
outlets <- filter(flow_net, !tonode %in% fromnode)

# We have these feature types. A larger dataset might include
# things like canals which would not be considered  "major"
unique(flow_net$FTYPE)

# compare dendritic vs non-dendritic toid row counts to see how many
# secondary paths exist in the data
flow_net_with_div <- add_divergence(flow_net,
  coastal_outlet_ids = c(),
  inland_outlet_ids = outlets$COMID,
  name_attr = "GNIS_ID",
  type_attr = "FTYPE",
  major_types = unique(flow_net$FTYPE))

dend <- add_toids(flow_net_with_div, return_dendritic = TRUE)
nondend <- add_toids(flow_net_with_div, return_dendritic = FALSE)

nrow(dend)
nrow(nondend)
nrow(nondend) - nrow(dend)

# now we run the add_divergence, add_toids, and add_streamorder
flow_net <- add_divergence(flow_net,
  coastal_outlet_ids = c(),
  inland_outlet_ids = outlets$COMID,
  name_attr = "GNIS_ID",
  type_attr = "FTYPE",
  major_types = unique(flow_net$FTYPE)) |>
  add_toids() |>
  add_streamorder() |>
  add_return_divergence()

# Make sure we reproduce what came from our source NHDPlus data.
sum(flow_net$divergence == 2)
sum(x$Divergence == 2)
all(flow_net$divergence == x$Divergence)
sum(flow_net$return_divergence == x$RtnDiv)

names(flow_net)

