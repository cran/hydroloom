## ----setup, include = FALSE---------------------------------------------------
local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")

if(!local) {
  nhdplusTools::nhdplusTools_data_dir(tempdir())
}

oldoption <- options(scipen = 9999)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.width=4,
  fig.height=4,
  fig.align = "center",
  eval=local
)


## ---- message=FALSE-----------------------------------------------------------
library(hydroloom)
library(dplyr)

dir <- nhdplusTools::download_nhd(nhdplusTools::nhdplusTools_data_dir(), 
                                  "1908", TRUE)
fs <- list.files(dir, pattern = ".*1908.*.gdb", full.names = TRUE)

# Dropping Z and M and making sure everything is LINESTRING helps later.
fl <- sf::st_zm(sf::st_cast(sf::read_sf(fs, "NHDFlowline"), "LINESTRING"))

# remove any coastal (ftype 566) features
fl <- filter(fl, ftype != 566)

## ---- message=FALSE-----------------------------------------------------------
# this is a seed point near the outlet of a watershed to consider.
point <- sf::st_sfc(sf::st_point(c(-147.911472, 64.796633)), 
                              crs = 4269)

# now we get the line our point is along.
i <- index_points_to_lines(fl, point)

# Let's see what we have.
sub <- fl[fl$permanent_identifier == i$permanent_identifier, ]

mapview::mapview(list(sub, point))

## ---- message=FALSE-----------------------------------------------------------
# remove coastal and make terminals go to an empty id.
flow_table <- sf::read_sf(fs, "NHDFlow") |>
  filter(from_permanent_identifier %in% fl$permanent_identifier) |>
  mutate(to_permanent_identifier = 
           ifelse(!to_permanent_identifier %in% from_permanent_identifier, 
                  "", 
                  to_permanent_identifier))

# Remove loops found in navigate network dfs
remove <- hydroloom::check_hy_graph(flow_table)

# this is naive and these removals would normally be reviewed.
flow_table <- flow_table |>
  mutate(row = 1:n()) |>
  filter(!row %in% remove$row)

down <- navigate_network_dfs(flow_table, sub$permanent_identifier, direction = "down")
up <- navigate_network_dfs(flow_table, sub$permanent_identifier, direction = "up")

subdown <- fl[fl$permanent_identifier %in% unique(unlist(down)),]
subup <- fl[fl$permanent_identifier %in% unique(unlist(up)),]

map_image <- "flow-table-fig.jpeg"
map <- mapview::mapview(list(subup, subdown, point))
mapview::mapviewOptions(fgb = FALSE)
mapview::mapshot(map, file = map_image)
knitr::include_graphics(map_image)

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

if(!local) {
  unlink(nhdplusTools::nhdplusTools_data_dir(), recursive = TRUE)
}

