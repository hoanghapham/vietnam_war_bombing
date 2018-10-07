

source("config/config_pj.R")

data_raw = fread("data/raw/THOR_Vietnam_Bombing_Operations.csv")

# Processing --------------------------------------------------------------

replace_missing(data_raw)

names(data_raw) <- names(data_raw) %>% tolower()    
names(data_raw) <-gsub("tgtlatdd_ddd_wgs84", "tgt_lat", names(data_raw))
names(data_raw) <-gsub("tgtlonddd_ddd_wgs84", "tgt_lon", names(data_raw))

data_raw[msndate == "19700229", msndate := "19700228"]
data_raw[, msndate := ymd(msndate)]
data_raw[, msnyear := year(msndate)]
data_raw[, msnym := strftime(msndate, "%Y%m")]
data_raw[, msnmonthname := month(msndate, label = T, abbr = T)]

data_raw[, timeontarget := sprintf("%04d", timeontarget)]
data_raw[, timeofftarget := sprintf("%04d", timeofftarget)]
data_raw[!is.na(timeontarget), hourontarget := substr(timeontarget, 1, 2)]
data_raw[!is.na(timeofftarget), hourofftarget := substr(timeofftarget, 1, 2)]

data_raw[, atk_hour := pmin(hourontarget, hourofftarget, na.rm = T) %>% as.numeric()]
data_raw[atk_hour == 24, `:=`(atk_hour = 0, msndate = msndate + 1)]
data_raw[atk_hour > 24, atk_hour := NA]

# Cleaning ----------------------------------------------------------------
# + Varchar -----------------------------------------------------------------

data_raw <- data_raw[msndate >= "1965-10-01" 
                     # & msndate <= "1973-12-31"
                     ]

# Remove dup rows
check_vars <- c(
    "msndate", "takeofflocation", "timeontarget", "timeofftarget", 
    "aircraft_root", "unit", "missionid", "weapontype", "weaponsloadedweight", "numweaponsdelivered", 
    "flthours", "mfunc_desc", "numofacft",
    "tgt_lat", "tgt_lon", "tgtcloudcover"
)

data_raw[, occurence := .N, by = eval(parse(text = check_vars))]
setorderv(data_raw, c(check_vars, "thor_data_viet_id"), order = c(rep(1, length(check_vars)), -1), na.last = F)
latest_record = data_raw[, .I[1], by = check_vars]$V1
data_raw <- data_raw[latest_record]

# Fix aircraft names
data_raw[, aircraft_root := gsub("/", "", aircraft_root)]

# Fix operaton names
ops_fix <- fread("data/raw/ops_fix.csv")

for (i in 1:nrow(ops_fix)){
    data_raw[, operationsupported := gsub(ops_fix$pattern[i], ops_fix$fix[i], operationsupported)]
}


# + Varnum ------------------------------------------------------------------

data_raw[numweaponsdelivered == 9999, numweaponsdelivered := NA]
data_raw[numweaponsjettisoned %in% c(-1, 999), numweaponsjettisoned := NA]
data_raw[numweaponsreturned %in% c(-1, 999), numweaponsreturned := NA]
data_raw[weaponsloadedweight == -1, weaponsloadedweight := NA]

# summary(data_raw[, varnum, with = F])

# View(unique(data_raw$numweaponsdelivered))

# + Drop cols -------------------------------------------------------------
drop_cols = c(
    "weapontypeclass"
    , "tgtid"
    , "releasefltspeed"
    , "airforcesqdn"
    , "airforcegroup"
    , "releasealtitude"
    , "resultsbda"
    , "callsign"
    , "geozone"
    , "tgtorigcoordsformat"
    , "tgtorigcoords"
    , "additionalinfo"
    , "mfunc"
    , "timeontarget"
    , "timeofftarget"
    , "hourontarget"
    , "hourofftarget"
    , "valid_aircraft_root"
    , "flthours"
    , "occurence"
)

data_raw[, (drop_cols) := NULL]


# Expanding ---------------------------------------------------------------
data_raw[, operation_grp := tstrsplit(operationsupported, " |&|-")[1]]
data_raw[, operation_grp := gsub("YANKEE$", "YANKEETEAM", operation_grp)]

# Aircraft type
aircrafts <- fread("data/raw/THOR_Vietnam_Aircraft_Glossary.csv", 
                   select = c("VALIDATED_ROOT", "AIRCRAFT_APPLICATION"))

names(aircrafts) <- names(aircrafts) %>% tolower()
aircrafts[, validated_root := gsub("-", "", validated_root)]
data_raw <- merge(data_raw, aircrafts, 
                  by.x = "aircraft_root", 
                  by.y = "validated_root", 
                  all.x = T)

# Weapons
weapons <- fread("data/raw/THOR_Vietnam_Weapons_Glossary.csv", 
                 select = c("WEAPONTYPE", "WEAPON_CLASS"))
names(weapons) <- names(weapons) %>% tolower()
data_raw <- merge(data_raw, weapons, by = "weapontype", all.x = T)

# View(table(data_raw$weapon_class))

# Save data ---------------------------------------------------------------
fwrite(data_raw, "data/processed/bombing.csv")



