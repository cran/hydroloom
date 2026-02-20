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

## -----------------------------------------------------------------------------
net <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

# subset to a connected upstream basin
x <- subset_network(net, 8893236)

plot(sf::st_geometry(net))
plot(sf::st_geometry(x), col = "red", add = TRUE)

## -----------------------------------------------------------------------------
# add_toids creates a dendritic edge list (id/toid)
x$dend_totdasqkm <- accumulate_downstream(add_toids(x, return_dendritic = TRUE), "AreaSqKM")

# diversions reset to their own local area
plot(x["dend_totdasqkm"], lwd = x$dend_totdasqkm / 20,
  main = "Dendritic accumulation")

## -----------------------------------------------------------------------------
y <- x |>
  group_by(FromNode) |>
  # split evenly among downstream flowlines
  mutate(divergence_fraction = 1 / max(n())) |>
  ungroup()

y$frac_totdasqkm <- accumulate_downstream(y, "AreaSqKM")

# diversions now carry a fraction of upstream area
plot(y["frac_totdasqkm"], lwd = y$frac_totdasqkm / 20,
  main = "Fractional accumulation")

# zoom in to see what's going on at diversions
plot(sf::st_geometry(y[y$COMID %in% c("8893210", "8893222"), ]), col = NA)
plot(y["frac_totdasqkm"], lwd = y$frac_totdasqkm / 20,
  main = "Fractional accumulation", add = TRUE)

## -----------------------------------------------------------------------------
z <- x |>
  select(COMID, FromNode, ToNode, Divergence, AreaSqKM, TotDASqKM)

z$tot_totdasqkm <- accumulate_downstream(z, "AreaSqKM", total = TRUE)

plot(z["tot_totdasqkm"], lwd = z$tot_totdasqkm / 20,
  main = "Total upstream accumulation")

# zoom in to see what's going on at diversions
plot(sf::st_geometry(y[y$COMID %in% c("8893210", "8893222"), ]), col = NA)
plot(z["tot_totdasqkm"], lwd = z$tot_totdasqkm / 20,
  main = "Total upstream accumulation", add = TRUE)

# matches NHDPlusV2 total drainage area
any(abs(z$tot_totdasqkm - z$TotDASqKM) > 0.001)

## -----------------------------------------------------------------------------
suppressWarnings(
ppt7100 <- nhdplusTools::get_catchment_characteristics(c("CAT_PPT7100_ANN", "TOT_PPT7100_ANN"), ids = x$COMID)
)

ppt7100 <- tidyr::pivot_wider(ppt7100, names_from = "characteristic_id", values_from = "characteristic_value")

x <- left_join(x, ppt7100, by = c("COMID" = "comid"))

# mean annual precipitation is already on the sample data
# CAT_PPT7100_ANN is the local catchment mean (mm)
# TOT_PPT7100_ANN is the pre-computed total upstream mean (mm)
w <- x |>
  select(COMID, FromNode, ToNode, Divergence,
    AreaSqKM, CAT_PPT7100_ANN, TOT_PPT7100_ANN)

# local precipitation volume = depth * area
w$cat_ppt_vol <- w$CAT_PPT7100_ANN * w$AreaSqKM

# accumulate volume, then divide by total area for area-weighted mean
w$total_da_sqkm <- accumulate_downstream(w, "AreaSqKM", total = TRUE)
w$tot_ppt_vol <- accumulate_downstream(w, "cat_ppt_vol", total = TRUE)
w$tot_ppt <- w$tot_ppt_vol / w$total_da_sqkm

# note that all differences from the source estimate are within rounding error
range(w$TOT_PPT7100_ANN - w$tot_ppt, na.rm = TRUE)

## ----teardown, include=FALSE--------------------------------------------------
options(oldoption)

