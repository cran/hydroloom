## ----setup, include = FALSE---------------------------------------------------
local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.width = 4,
  fig.height = 4,
  fig.align = "center",
  eval = local
)

library(sf)

oldoption <- options(scipen = 9999)

## -----------------------------------------------------------------------------
library(hydroloom)

hy_net <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom")) |>
  dplyr::select(COMID, REACHCODE, FromNode, ToNode, Hydroseq, TerminalFl, Divergence)

hy(hy_net[1:3, ])

attr(hy(hy_net), "orig_names")

## ----echo=FALSE, eval=TRUE, fig.dim=c(6, 4)-----------------------------------
print.data.frame(data.frame(id = c(1, 2, 3),
  toid = c(3, 3, NA),
  fromnode = c("N1", "N2", "N3"),
  tonode = c("N3", "N3", "N4")),
row.names = FALSE)

## ----node, fig.show="hold", out.width="45%", echo=FALSE, eval=TRUE, fig.cap="In an edge-node topology, edges are directed to nodes which are then directed to other edges. An edge-to-edge topology does not include intervening nodes."----
x <- c(1, 5, 3, 3)
y <- c(5, 5, 3, 1)

oldpar <- par(mar = c(0, 0, 0, 0))
plot(x, y, col = NA)
arrows(x[1] + 0.1, y[1] - 0.1, x[3] - 0.1, y[3] + 0.1, 0.1)
arrows(x[2] - 0.1, y[2] - 0.1, x[3] + 0.1, y[3] + 0.1, 0.1)
arrows(x[3], y[3] - 0.1, x[4], y[4] + 0.1, 0.1)
text(c(2, 4, 3.15), c(4.2, 4.2, 2), c("1", "2", "3"))

par(mar = c(0, 0, 0, 0))
plot(x, y)
arrows(x[1] + 0.1, y[1] - 0.1, x[3] - 0.1, y[3] + 0.1, 0.1)
arrows(x[2] - 0.1, y[2] - 0.1, x[3] + 0.1, y[3] + 0.1, 0.1)
arrows(x[3], y[3] - 0.1, x[4], y[4] + 0.1, 0.1)
text(c(2, 4, 3.1), c(4.2, 4.2, 2), c("1", "2", "3"))
text(c(1, 5, 3, 3.25), c(4.8, 4.8, 3.4, 1), c("N1", "N2", "N3", "N4"))
par(oldpar)

## ----node1, fig.show="hold", fig.width=3, out.width="45%", echo=FALSE, eval=TRUE----
x <- c(2, 2, 3, 2, 2)
y <- c(5, 4, 3, 2, 1)

a <- c(1.4, 3.5)
b <- c(.9, 5.1)

main_col <- "darkblue"
div_col <- "purple"

oldpar <- par(mar = c(0, 0, 0, 0))
plot(a, b, col = NA)
points(x, y)
text(c(2.15, 1.85, 3.02, 1.85, 2.15), c(5, 4, 3.2, 2, 1), c("N1", "N2", "N3", "N4", "N5"))

make_edges <- function() {
  arrows(x[1], y[1] - .1, x[2], y[2] + .1, length = .1, col = main_col)
  arrows(x[2] + .1, y[2] - .1, x[3] - .1, y[3] + .1, length = .1, col = div_col) # right
  arrows(x[2] + .0, y[2] - .1, x[4] - .0, y[4] + .1, length = .1, col = main_col)
  arrows(x[3] - .1, y[3] - .1, x[4] + .1, y[4] + .1, length = .1, col = div_col)
  arrows(x[4], y[4] - .1, x[5], y[5] + .1, length = .1, col = main_col)
  text(c(2.1, 2.5, 2.5, 1.9, 2.1), c(4.5, 3.8, 2.3, 3, 1.5), c("1", "2", "3", "4", "5"))
}

make_edges()
par(oldpar)

## ----node2, fig.show="hold", fig.width=3, out.width="45%", echo=FALSE, eval=TRUE----
oldpar <- par(mar = c(0, 0, 0, 0))
plot(a, b, col = NA)

make_edges()
par(oldpar)

## ----class_demo, eval=TRUE----------------------------------------------------
library(hydroloom)

# bipartite graph: id + fromnode + tonode (unique id)
node_df <- data.frame(
  id       = c(1, 2, 3, 4, 5),
  fromnode = c("N1", "N2", "N3", "N2", "N4"),
  tonode   = c("N2", "N3", "N4", "N4", "N5")
)
class(hy(node_df))

# dendritic edge list: id + toid (unique id; secondary path dropped)
topo_df <- data.frame(
  id   = c(1, 2, 3, 4, 5),
  toid = c(2, 3, 5, 5, 0)
)
class(hy(topo_df))

# non-dendritic junction table: id + toid with id repeating
fn_df <- data.frame(
  id   = c(1, 1, 2, 3, 4, 5),
  toid = c(2, 4, 3, 5, 5, 0)
)
class(hy(fn_df))

## -----------------------------------------------------------------------------
y <- add_toids(hy_net, return_dendritic = TRUE)

ind_id <- make_index_ids(y)

names(ind_id)

dim(ind_id$to)

max(lengths(ind_id$lengths))

names(ind_id$to_list)

sapply(ind_id, class)

## -----------------------------------------------------------------------------
y <- add_toids(st_drop_geometry(hy_net), return_dendritic = FALSE)

ind_id <- make_index_ids(y)

names(ind_id)
dim(ind_id$to)

max(ind_id$lengths)

sum(ind_id$lengths == 2)
sum(ind_id$lengths == 3)

names(ind_id$to_list)

sapply(ind_id, class)

## -----------------------------------------------------------------------------
from_id <- make_index_ids(y, mode = "from")

names(from_id)

dim(from_id$froms)

# a confluence: two upstream connections
max(from_id$lengths)

sum(from_id$lengths == 2)

## -----------------------------------------------------------------------------
both_id <- make_index_ids(y, mode = "both")

names(both_id)

# each direction covers the same set of features
ncol(both_id$to$to) == ncol(both_id$from$froms)

## -----------------------------------------------------------------------------
# navigate_network_dfs creates the graph internally from a data.frame
navigate_network_dfs(y, starts = y$id[1], direction = "down")

# or accept pre-built index ids -- use "to" for downstream, "from" for upstream
to_index <- make_index_ids(y, mode = "to")
navigate_network_dfs(to_index, starts = y$id[1], direction = "down")

from_index <- make_index_ids(y, mode = "from")
navigate_network_dfs(from_index, starts = y$id[1], direction = "up")

