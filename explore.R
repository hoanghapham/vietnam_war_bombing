
# Setting -----------------------------------------------------------------

source("config/config_pj.R")


# Load data ---------------------------------------------------------------

bombing = fread("data/processed/bombing.csv")

bombing[, msndate := ymd(msndate)]


# Explore -----------------------------------------------------------------
varid = c("thor_data_viet_id", "id", "sourceid", "sourcerecord")
varclass <- lapply(bombing, class) %>% unlist() %>% sort() 
varchar <- varclass[varclass == "character"] %>% names() %>% sort() %>% setdiff(varid)
varnum <- varclass[varclass %in% c("numeric", "integer")] %>% names() %>% sort() %>% setdiff(varid)


# total statistics
uniqueN(bombing$operation_grp)
uniqueN(bombing$operationsupported)
uniqueN(bombing$aircraft_original)
uniqueN(bombing$aircraft_root)
uniqueN(bombing$weapon_class)
unique(bombing$weapon_class)
uniqueN(bombing$weapontype)
unique(bombing$weapontype)
uniqueN(bombing$tgtcountry)
unique(bombing$tgtcountry)

uniqueN(bombing$milservice)
unique(bombing$milservice)
uniqueN(bombing$takeofflocation)


total_tonnage = sum(as.numeric(bombing$weapontypeweight * bombing$numweaponsdelivered), na.rm = T) * 0.45 
total_tonnage %>% comma()

# bombing[, msnyear := factor(msnyear)]
# bombing[, msnym := factor(msnym)]

strikes_year = bombing[mfunc_desc_class == "KINETIC", .(strike_count = .N), by = .(msnyear)]

ggplot(strikes_year) + 
    geom_bar(aes(x = as.factor(msnyear), y = strike_count), stat = "identity")

ops_year = bombing[, .(operation_count = uniqueN(operationsupported)), by = .(msnyear)]

ggplot(ops_year) + 
    geom_bar(aes(x = as.factor(msnyear), y = operation_count), stat = "identity")


opsgrp_year = bombing[, .(operation_count = uniqueN(operation_grp)), by = .(msnyear)]

ggplot(opsgrp_year) + 
    geom_bar(aes(x = as.factor(msnyear), y = operation_count), stat = "identity")

# bombing escalates from 1966, top at 1968 and withdrawal happens gradually
# U.S. press and public began to challenge the Johnson administration’s assurances of success 
# and to question the value of the increasingly costly war.

vnconflict = fread("data/raw/VietnamConflict.csv")

summary(vnconflict$FATALITY_YEAR)

fatal_year = vnconflict[FATALITY_YEAR < 1975, .(fatal_count = .N), by = .(FATALITY_YEAR)]

ggplot(fatal_year) + 
    geom_bar(aes(x = as.factor(FATALITY_YEAR), y = fatal_count), stat = "identity")

View(bombing[weapontype != "", .(weapontype, weapontypeweight, weaponsloadedweight, weapon_class,
                 numweaponsdelivered)])

# tonage

# Other questions ---------------------------------------------------------

# Which branch of the U.S. military flew the most daytime ground-attack missions in Cambodia? 
# Which U.S. Air Force unit dropped the greatest overall tonnage of bombs? 
# What was the average duration of carrier-based U.S. Navy missions? 
# Was napalm used more frequently in some countries than others? 

# Over the course of the war, total U.S. bombing tonnage far exceeded that dropped on 
# Germany, Italy, and Japan in World War II.
# https://www.kaggle.com/usaf/world-war-ii
# What happened in 1972?
# Plot bom tonnage by area



# Rolling Thunder ---------------------------------------------------------

thunder = fread("data/processed/bombing_rolling_thunder.csv")
vnmap = readRDS("data/raw/vnmap.rds")
north_map = readRDS("data/raw/north_map.rds")

replace_missing(thunder)
thunder[, msndate := ymd(msndate)]

# Evolution of mission function over time

mfunc_x_time = thunder[mfunc_desc %in% mfunc_popular, .(
    sortie_count = .N
    , unique_msn_count = uniqueN(missionid)
    , total_delivered = sum(delivered_tonnage)
), by = .(mfunc_desc, msnym)]

p1 = ggplot(mfunc_x_time[msnym >= 196501], aes(x = factor(msnym))) + 
    geom_line(aes(y = unique_msn_count, group = mfunc_desc, col = mfunc_desc)) 


bombing[msnym == 196503, .(operation_grp, operationsupported)] %>% unique()


ggplotly(p1)

# Atk hours & surge of activities

atk = thunder[, .(count = .N), by = .(mfunc_desc_class, atk_hour)]

ggplot(atk[atk_hour > 0], aes(x = factor(atk_hour), y = count, group = mfunc_desc_class, col = mfunc_desc_class)) + 
    geom_line() + 
    facet_wrap(~mfunc_desc_class, nrow = 2, scales = "free")


recce = thunder[str_detect(mfunc_desc, "RECCE"), .(count = .N, msn = uniqueN(missionid)), 
                by = .(mfunc_desc_class, mfunc_desc, atk_hour)]

p2 = ggplot(recce[atk_hour > 0], aes(x = factor(atk_hour), y = count, group = mfunc_desc, col = mfunc_desc)) + 
    geom_line() + 
    facet_wrap(~mfunc_desc_class, nrow = 2, scales = "free")

ggplotly(p2)

# aircraft app vs time

aircraft_stats = thunder[!is.na(aircraft_application) & !is.na(atk_hour), .(
    count = .N
), by = .(aircraft_application)]

View(aircraft_stats)

