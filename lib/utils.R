
load_libraries <- function(libraries)
{
    for (lib in libraries) 
    {
        if (!require(lib, character.only = TRUE))
        {
            install.packages(lib, dep=TRUE)
            if(!require(lib, character.only = TRUE)) stop("Package not found")
        }
    }
}


plot_bomb_density <- function(vnmap, target_coord, year, nsample = 1000, plot_title = NULL)
{
    edges = round(vnmap$data)
    grid_x = seq(edges$lon[1], edges$lon[2], 1)
    grid_y = seq(edges$lat[1], edges$lat[3], 1)
    
    target_df <- target_coord %>% 
        filter(
            mfunc_desc_class == "KINETIC"
            , lon > min(grid_x), lon < max(grid_x)
            , lat > min(grid_y), lat < max(grid_x)
            , msnyear == year
        ) %>% 
        sample_n(nsample)
    
    bomb_map = vnmap +
        geom_density_2d(data = target_df, aes(x = lon, y = lat), size = 0.3) +
        stat_density_2d(data = target_df, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), show.legend = F, geom = "polygon") + 
        geom_vline(xintercept = grid_x, size = 0.1, alpha = 0.7, color = "gray") +
        geom_hline(yintercept = grid_y, size = 0.1, alpha = 0.7, color = "gray") + 
        scale_alpha(guide = F) + 
        scale_fill_distiller(palette = "Spectral") + 
        scale_x_continuous(breaks = grid_x) + 
        scale_y_continuous(breaks = grid_y) + 
        ggtitle(plot_title) + 
        labs(x = "", y = "") + 
        theme(
            panel.background = element_blank()
            , axis.ticks = element_blank()
            , axis.line = element_line(size = 0.1, color = "gray")
        )
    
    return(bomb_map)
}


plot_bomb_points <- function(vnmap, target_coord, year = NULL, nsample = 1000, plot_title = NULL)
{
    edges = round(vnmap$data)
    grid_x = seq(edges$lon[1], edges$lon[2], 1)
    grid_y = seq(edges$lat[1], edges$lat[3], 1)
    
    if (is.null(year)) {
        target_df <- target_coord %>% 
            filter(
                mfunc_desc_class == "KINETIC"
                , lon >= edges$lon[1],  lon <= edges$lon[2]
                , lat >= edges$lat[1], lat <= edges$lat[3]
            ) %>% 
            sample_n(nsample)    
    } else {
        target_df <- target_coord %>% 
            filter(
                mfunc_desc_class == "KINETIC"
                , lon >= edges$lon[1],  lon <= edges$lon[2]
                , lat >= edges$lat[1], lat <= edges$lat[3]
                , msnyear == year
            ) %>% 
            sample_n(nsample)
    }
    
    lat1 = min(vnmap$data$lat) %>% round()
    lat2 = max(vnmap$data$lat) %>% round()
    lon1 = min(vnmap$data$lon) %>% round()
    lon2 = max(vnmap$data$lon) %>% round()
    
    bomb_map = vnmap +
        geom_point(data = target_df, aes(x = lon, y = lat), size = 0.1, col = "red") +
        geom_vline(xintercept = grid_x, size = 0.1, alpha = 0.7, color = "gray") +
        geom_hline(yintercept = grid_y, size = 0.1, alpha = 0.7, color = "gray") + 
        scale_alpha(guide = F) + 
        scale_x_continuous(breaks = grid_x) + 
        scale_y_continuous(breaks = grid_y) + 
        ggtitle(plot_title) + 
        labs(x = "", y = "") + 
        theme(
            panel.background = element_blank()
            , axis.ticks = element_blank()
            , axis.line = element_line(size = 0.1, color = "gray")
        )
    
    return(bomb_map)
}

extract_uvals <- function(x, split = ',')
{
    uvals = x %>% strsplit(split) %>% unlist() %>% unique() %>% sort()
    uvals = uvals[!is.na(uvals) & uvals != ""]
    return(uvals)
}

count_uvals_occ <- function(x, split = ",")
{
    uvals <- extract_uvals(x, split)
    
    uvals_occ <- c()
    for (i in uvals){
        uvals_occ = c(uvals_occ, sum(grepl(i, x)))
    }
    
    uvals_df = data.table(
        value = uvals
        , occurence = uvals_occ
    )
    
    uvals_df <- uvals_df[order(-occurence)]
    uvals_df[, value := reorder(value, occurence)]
    
    return(uvals_df)
}

replace_missing <- function(df, value = NA)
{
    for (j in names(df)){
        set(df, i = which(df[[j]] == ""), j = j, value = value)
        set(df, i = grep("unknown|UNKNOWN", df[[j]]), j = j, value = value)
    }
}
