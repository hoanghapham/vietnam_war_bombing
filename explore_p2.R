source("config/config_pj.R")
thunder = fread("data/processed/bombing_rolling_thunder.csv")
vnmap = readRDS("data/raw/vnmap.rds")
north_map = readRDS("data/raw/north_map.rds")

replace_missing(thunder)
thunder[, msndate := ymd(msndate)]

thunder[, delivered_tonnage := weapontypeweight * numweaponsdelivered ]
thunder[, jettisoned_tonnage := weapontypeweight * numweaponsjettisoned]
thunder[, returned_tonnage := weapontypeweight * numweaponsreturned]


thunder
View(head(thunder))
# Which branch of the U.S. military flew the most daytime ground-attack missions in Cambodia? 
# Which U.S. Air Force unit dropped the greatest overall tonnage of bombs? 
# What was the average duration of carrier-based U.S. Navy missions? 
# Was napalm used more frequently in some countries than others? 

# Variance 
var(thunder$tgt_lat, na.rm = T)
var(thunder$tgt_lon, na.rm = T)


tgt_dispersion = thunder[, .(
    sd_lat = sd(tgt_lat, na.rm = T)
    , sd_lon = sd(tgt_lon, na.rm = T)
    
    , range_lat = range(tgt_lat, na.rm = T)[2] - range(tgt_lat, na.rm = T)[1]
    , range_lon = range(tgt_lon, na.rm = T)[2] - range(tgt_lon, na.rm = T)[1]
    
    , iqr_lat = IQR(tgt_lat, na.rm = T)
    , iqr_lon = IQR(tgt_lon, na.rm = T)
), by = .(msnym)]

library(plotly)

p = ggplot(tgt_dispersion[msnym >= 196701], aes(x = as.factor(msnym))) + 
    geom_line(aes(y = sd_lat, group = 1), stat = "identity", color = "red") + 
    geom_line(aes(y = sd_lon, group = 1), stat = "identity", color = "blue") 
    

# Use this
# range of lat and lon decreases over time, which means a more focused bombing effort.
ggplotly(p)

p2 = ggplot(tgt_dispersion[msnym >= 196701], aes(x = as.factor(msnym))) + 
    geom_line(aes(y = range_lat, group = 1), stat = "identity", color = "red") + 
    geom_line(aes(y = range_lon, group = 1), stat = "identity", color = "blue") 

ggplotly(p2)


p3 = ggplot(tgt_dispersion[msnym >= 196701], aes(x = as.factor(msnym))) + 
    geom_line(aes(y = iqr_lat, group = 1), stat = "identity", color = "red") + 
    geom_line(aes(y = iqr_lon, group = 1), stat = "identity", color = "blue") 

ggplotly(p3)


# Bombing tonnage over time
# Type of aircraft used by forces? 
# types of weapon used by forces and their use over the operation duration
# operation time

unique(thunder$mfunc_desc)

ggplot(thunder[atk_hour > 0]) + 
    geom_bar(aes(x = atk_hour)) + 
    facet_wrap(~mfunc_desc_class, nrow = 2, scales = "free")

atk_hour = thunder[atk_hour > 0, .(
    kinetic = sum(mfunc_desc_class == "KINETIC")
    , nonkinetic = sum(mfunc_desc_class == "NONKINETIC")
), by = atk_hour]

plot(atk_hour$kinetic, atk_hour$nonkinetic)

thunder[, is_recon := stri_detect(mfunc_desc, regex = "RECCE")]

ggplot(thunder[atk_hour > 0]) + 
    geom_bar(aes(x = atk_hour)) + 
    facet_wrap(~is_recon, nrow = 2, scales = "free") + 
    scale_x_continuous(breaks = 1:23)

ggplot(thunder[atk_hour > 0 & mfunc_desc == "STRIKE"]) + 
    geom_bar(aes(x = atk_hour)) + 
    scale_x_continuous(breaks = 1:23)

on_tgt_hour = thunder[, .(count = .N), by = .(timeontarget)]

ggplot(on_tgt_hour[timeontarget > 0], aes(x = as.factor(timeontarget))) + 
    geom_line(aes(y = count, group = 1))

rt65 = plot_bomb_points(north_map, target_coord, 1965, nsample = 2000, plot_title = "Rolling Thunder target map - 1965")

rt65 + 
    annotate("label", x = 108, y = 21, label = "Hanoi & Haiphong\nwere spared", hjust = 0, alpha = 0.8)  + 
    annotate("rect", xmin = 105.5, xmax = 107, ymin = 20.5, ymax = 21.5, alpha = 0.3, size = 15)

ggplotly(rt65)

nsample = 5000
edges = round(north_map$data)
grid_x = seq(edges$lon[1], edges$lon[2], 1)
grid_y = seq(edges$lat[1], edges$lat[3], 1)

target_df <- target_coord %>% 
    filter(
        mfunc_desc_class == "KINETIC"
        , lon >= edges$lon[1],  lon <= edges$lon[2]
        , lat >= edges$lat[1], lat <= edges$lat[3]
        , milservice %in% c("USN", "USAF")
    ) %>% 
    sample_n(nsample)    

label_df = data.table(
    x = c(107.5, 109)
    , y = c(19, 17)
    , label = c("US Navy", "US Air Force")
)

thunder_map = north_map +
    geom_point(data = target_df, aes(x = lon, y = lat, col = milservice), size = 0.1, show.legend = F) +
    geom_vline(xintercept = grid_x, size = 0.1, alpha = 0.7, color = "gray") +
    geom_hline(yintercept = grid_y, size = 0.1, alpha = 0.7, color = "gray") + 
    # geom_label(data = label_df[1], aes(x = x, y = y, label = label), col = "white", fill = force_col["USN"]) +
    # geom_label(data = label_df[2], aes(x = x, y = y, label = label), col = "white", fill = force_col["USAF"]) +
    scale_color_manual(values = force_col, name = NULL) +
    scale_x_continuous(breaks = grid_x) + 
    scale_y_continuous(breaks = grid_y) + 
    ggtitle("Rolling Thunder Bombing map 1965 - 1968") + 
    labs(x = "", y = "") + 
    theme(
        panel.background = element_blank()
        , axis.ticks = element_blank()
        , axis.line = element_line(size = 0.1, color = "gray")
    ) 

ggplotly(thunder_map)
