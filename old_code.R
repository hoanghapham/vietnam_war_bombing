
# Setting -----------------------------------------------------------------
source("config/config_pj.R")

data_raw <- fread("data/processed/bombing.csv")

data_raw[, msndate := ymd(msndate)] 

# Class
varid = c("thor_data_viet_id", "id", "sourceid", "sourcerecord")
varclass <- lapply(data_raw, class) %>% unlist() %>% sort() 
varchar <- varclass[varclass == "character"] %>% names() %>% sort() %>% setdiff(varid)
varnum <- varclass[varclass %in% c("numeric", "integer")] %>% names() %>% sort() %>% setdiff(varid)


# Examining cols ------------------------------------------------

varchar_ref <- data.table(varchar)
i = 1

for (col in varchar) {
    varchar_ref[i, unique_val := uniqueN(data_raw[, col, with = F])]
    i = i + 1
}

varnum_ref <- data.table(varnum)
i = 1

for (col in varnum) {
    varnum_ref[i, unique_val := uniqueN(data_raw[, col, with = F])]
    i = i + 1
}


# Explore -----------------------------------------------------------------

# Action type and country
# Most used aircraft


# Look at
# Period of day: more than 2 values
# Vars to cross tab: mil service, tgt country, takeoffloc, 

data_raw[, .(op_count = uniqueN(operationsupported)), by = .(milservice)][order(-op_count)]
data_raw[, .(op_count = uniqueN(operationsupported)), by = .(tgtcountry)][order(-op_count)]
data_raw[grepl("strike|STRIKE", mfunc_desc), .(op_count = uniqueN(operationsupported)), by = .(tgtcountry)][order(-op_count)]

data_raw[, .(count = .N), .(mfunc_desc, mfunc_desc_class)][order(-count)] %>% View()

# How many operations have kinetic / non kinetic missions?

data_raw[, .(op_count = uniqueN(operationsupported)), by = .(mfunc_desc_class)]

# Obvious thing to do first: visualize points of targets

# vncenter = geocode("Da Nang") %>% as.numeric()
vncenter = c(108.20217, 16.05441)

vnmap <- ggmap(get_googlemap(center = vncenter, scale = 2, zoom = 6, maptype = "roadmap"), extent = "normal")

target_coord <- data_raw[, .(msnyear, mfunc_desc_class, 
                            lon = tgt_lon, 
                            lat = tgt_lat)] %>% unique()

target_col <- c(
    "KINETIC" = "firebrick2"
    , "NONKINETIC" = "dodgerblue"
)


plot_bomb_dens_year <- function(vnmap, target_coord, year)
{
    target_sample <- target_coord %>% 
        filter(
            mfunc_desc_class == "KINETIC"
            , lon >= 100,  lon <= 112
            , lat >= 10, lat <= 24
            , msnyear == year
        ) %>% 
        sample_n(1000)
    
    bomb_map = vnmap +
        geom_density_2d(data = target_sample, aes(x = lon, y = lat), size = 0.3) +
        stat_density_2d(data = target_sample, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") + 
        geom_vline(xintercept = seq(102, 114, 1), size = 0.1, alpha = 0.5) +
        geom_hline(yintercept = seq(10, 22, 1), size = 0.1, alpha = 0.5) + 
        scale_color_manual(values = target_col) + 
        scale_fill_gradient(low = "green2", high = "red2") + 
        scale_alpha(guide = F) + 
        scale_x_continuous(breaks = seq(102, 114, 1)) + 
        scale_y_continuous(breaks = seq(10, 22, 1)) + 
        ggtitle(sprintf("Bombing map %s", year)) + 
        labs(x = "Longitude", y = "Latitude") + 
        theme(
            panel.background = element_blank()
            , axis.ticks = element_blank()
            , axis.line = element_line(size = 0.3)
        )
    
    return(bomb_map)
}

target_sample <- target_coord  %>%  sample_n(1000)

vnmap + geom_point(data = target_sample, aes(x = lon, y = lat), col, size = 0.1)

# bombing moved from 16th latitude to 17th latitude in 1966. since then, it's the most heavily bombed area
# different data_raw pattern in 1973

plot_bomb_dens_year(vnmap, target_coord, 1965)
plot_bomb_dens_year(vnmap, target_coord, 1966)
plot_bomb_dens_year(vnmap, target_coord, 1967)
plot_bomb_dens_year(vnmap, target_coord, 1968)
plot_bomb_dens_year(vnmap, target_coord, 1969)
plot_bomb_dens_year(vnmap, target_coord, 1970)
plot_bomb_dens_year(vnmap, target_coord, 1971)
plot_bomb_dens_year(vnmap, target_coord, 1972)
plot_bomb_dens_year(vnmap, target_coord, 1973)

plot_bomb_route_year(vnmap, target_coord, 1965, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1966, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1967, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1968, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1969, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1970, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1971, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1972, nsample = 5000)
plot_bomb_route_year(vnmap, target_coord, 1973, nsample = 5000)


# Which campaigns saw the heaviest bombings?
# Heaviest in terms of number of weapons delivered, or total weight of weapons delivered

# Which months saw the most runs?
# What were the most used aircraft?

# Check dup rows
# date, take-off base, take-off time, aircraft type, unit, mission number, callsign, weapon load, and target
# struck

data_raw[, occ_lv2 := .N, by = .(msndate, takeofflocation, timeontarget, timeofftarget, 
                                 aircraft_root, unit, missionid, weapontype, weaponsloadedweight, numweaponsdelivered, 
                                 flthours, mfunc_desc, numofacft,
                                tgtlatdd_ddd_wgs84, tgtlonddd_ddd_wgs84, tgtcloudcover)]

table(data_raw$occ_lv2)

View(data_raw[occ_lv2 == 7][order(missionid, msndate, takeofflocation, aircraft_root, timeontarget, unit)] %>% head(21)) 
View(data_raw[occ_lv2 == 18][order(missionid, msndate, takeofflocation, aircraft_root, timeontarget, unit)])

# Check if target coord has any anomaly


summary(data_raw[, .(tgtlatdd_ddd_wgs84, tgtlonddd_ddd_wgs84)])

ggplot(data_raw) + 
    geom_point(aes(x = tgtlatdd_ddd_wgs84, tgtlonddd_ddd_wgs84))

tgt_coord_l <- data_raw[!is.na(tgt_lon) & !is.na(tgt_lat), .(tgt_lon, tgt_lat)] %>% 
    melt(measure.vars = c("tgt_lat", "tgt_lon"))

ggplot(tgt_coord_l) + 
    geom_density(aes(x = value, fill = variable))


# coord anomaly detection -------------------------------------------------
# https://datascience-enthusiast.com/R/anomaly_detection_R.html

library(MASS)

tgt_coord <- data_raw[!is.na(tgt_lon) & !is.na(tgt_lat), .(tgt_lon, tgt_lat)]
# 
# tgt_coord[, tgt_lon := tgt_lon - mean(tgt_lon)]
# tgt_coord[, tgt_lat := tgt_lat - mean(tgt_lat)]
# 
# 
# sigma2 = var(tgt_coord) %>% diag() %>% diag()
# 
# A = (2*pi) ^ (-ncol(tgt_coord)/2) * det(sigma2)^(-0.5)
# B = exp(-0.5 * rowSums( (as.matrix(tgt_coord) %*% ginv(sigma2) ) * tgt_coord))
# 
# 
# prob = A * B
# 
# tgt_coord[, probability := prob]
# 
# View(tgt_coord)
# ggplot(df_prob) + 
#     geom_histogram(aes(x = probability), binwidth = 0.0001) + 
#     scale_x_continuous(breaks = seq(0, 1, 0.002))



# ggplot(df_prob, aes(y = probability, x = 1)) + 
#     geom_boxplot(group = 1) + 
#     geom_jitter(data = df_prob[sample(1000)])
# 


lon = ggplot(tgt_coord[tgt_lon >= 90]) + 
    geom_histogram(aes(x = tgt_lon), binwidth = 1) 


lat = ggplot(tgt_coord) + 
    geom_histogram(aes(x = tgt_lat), binwidth = 1) 

lon_bins <- ggplot_build(lon)$data[[1]]
lat_bins <- ggplot_build(lat)$data[[1]]

View(lat_bins)
data_raw[(tgt_lon <= 96 | tgt_lon >= 113) 
         | (tgt_lat <= 6 | tgt_lat >= 24), `:=`(tgt_lon = NA, tgt_lat = NA)]



summary(data_raw[, setdiff(varnum, varid), with = F])

# flthours
p_flthours <- ggplot(data_raw) + 
    geom_histogram(aes(x = flthours), binwidth = 1)

flthours_bin <- ggplot_build(p_flthours)$data[[1]]

View(flthours_bin)

View(data_raw[order(-flthours), .(timeontarget, timeofftarget, flthours)])

# flight hours by mission?
# flight hours by aircraft?

# group sortie by mission, aircraft

aircraft_time <- data_raw[aircraft_original != "", .(
    sortie_count = .N
    , flthour_sum = sum(flthours)
    , flthour_mean = mean(flthours)
    , flthour_max = max(flthours)
), keyby = .(aircraft_original, missionid, msndate, flthours)]

View(mission_time[order(aircraft_original, -flthour_max), .()][aircraft_original != ""])

View(data_raw[missionid == "C261" & aircraft_original == "A1", .(msndate, missionid, aircraft_original, aircraft_root, flthours)])
View(data_raw[missionid == "1432" & aircraft_original == "F4", .(msndate, missionid, aircraft_original, aircraft_root, flthours)])



aircraft_time <- data_raw[, .(
    sortie_count = .N
    , mission_count = uniqueN(missionid)
    , flthour_sum = sum(flthours, na.rm = T)
    , flthour_mean = mean(flthours)
    , flthour_max = max(flthours)
), by = .(aircraft_original)]

View(aircraft_time)

aircraft_hrs <- unique(data_raw[, .(aircraft_original, msndate, timeontarget, timeofftarget, flthours)])
aircraft_hrs[, is_outlier := outlier(flthours, logical = T), by = .(aircraft_original)]
aircraft_hrs[flthours == 0, is_outlier := F]

View(aircraft_hrs[order(aircraft_original, msndate, timeontarget)][aircraft_original != ""])

ggplot(flthours) + 
    geom_histogram(aes(x = flthours), binwidth = 1) + 
    scale_x_continuous(breaks = seq(0, 10000, 1000)) + 
    scale_y_continuous(breaks = seq(0, 10, 1)) + 
    facet_wrap(~sourcerecord)

View(data_raw[, .(timeontarget, timeofftarget)] %>% unique())

bombing_hours = data_raw[, .N, by = .(atk_hour)] 

ggplot(bombing_hours[atk_hour > 0]) + 
    geom_bar(aes(x = atk_hour, y = N), stat = "identity")

# Strange pattern in the atk hours. 


# Look at missionid


missions = data_raw[, .(
    count = .N
), by = .(missionid)]

data_raw[missionid == "J8002", .(missionid, operationsupported, aircraft_original, 
                                 aircraft_root, msndate, atk_hour)][order(operationsupported, missionid, 
                                                                          msndate, aircraft_original, atk_hour)] %>% View()


# aggregate data over which dimensions?

ops = data_raw[, .(
    count = .N
), by = .(operationsupported)]

View(ops)


# Fix names
opnames <- data_raw[, .(count = .N), keyby = .(operationsupported)]

ops_fix <- fread("data/raw/ops_fix.csv")

for (i in 1:nrow(ops_fix)){
    opnames[, operationsupported := gsub(ops_fix$pattern[i], ops_fix$fix[i], operationsupported)]
}


# View(opnames[grepl("^[A-Z][A-Z] ", operationsupported) == F, 
#              sum(count), 
#              keyby = .(operationsupported)])


opnames[, operation_grp := tstrsplit(operationsupported, " |&|-")[1]]

View(opnames[, .(count = .N), by = .(operation_grp)][order(-count)])

# Change of mission types before bombing?
# Predict bombing by surge in non-kinetic missions?


target_types <- extract_uvals(data_raw$tgttype, split = "\\\\")
target_df <- count_uvals_occ(data_raw$tgttype, split = "\\\\")


# Top operations ----------------------------------------------------------
table(data_raw$periodofday)
data_raw[, .(periodofday, atk_hour)] %>% unique() %>% View()

data_raw[, .(count = .N), by = takeofflocation] %>% View()
