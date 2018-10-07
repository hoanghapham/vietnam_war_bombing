
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


plot_bomb_dens_year <- function(vnmap, target_coord, year, nsample = 1000)
{
    target_df <- target_coord %>% 
        filter(
            mfunc_desc_class == "KINETIC"
            , lon >= 100,  lon <= 112
            , lat >= 10, lat <= 24
            , msnyear == year
        ) %>% 
        sample_n(nsample)
    
    bomb_map = vnmap +
        geom_density_2d(data = target_df, aes(x = lon, y = lat), size = 0.3) +
        stat_density_2d(data = target_df, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") + 
        geom_vline(xintercept = seq(102, 114, 1), size = 0.1, alpha = 0.5) +
        geom_hline(yintercept = seq(10, 22, 1), size = 0.1, alpha = 0.5) + 
        scale_color_manual(values = target_col) + 
        scale_fill_gradient(low = "green2", high = "red2") + 
        scale_alpha(guide = F) + 
        scale_x_continuous(breaks = seq(102, 114, 1)) + 
        scale_y_continuous(breaks = seq(10, 22, 1)) + 
        ggtitle(sprintf("Bombing intensity map %s", year)) + 
        labs(x = "", y = "") + 
        theme(
            panel.background = element_blank()
            , axis.ticks = element_blank()
            , axis.line = element_line(size = 0.1)
        )
    
    return(bomb_map)
}


plot_bomb_points <- function(vnmap, target_coord, year, nsample = 1000)
{
    target_df <- target_coord %>% 
        filter(
            mfunc_desc_class == "KINETIC"
            , lon >= 100,  lon <= 112
            , lat >= 10, lat <= 23
            , msnyear == year
        ) %>% 
        sample_n(nsample)
    
    
    bomb_map = vnmap +
        geom_point(data = target_df, aes(x = lon, y = lat), size = 0.1, col = "red") +
        geom_vline(xintercept = seq(102, 114, 1), size = 0.1, alpha = 0.5) +
        geom_hline(yintercept = seq(10, 22, 1), size = 0.1, alpha = 0.5) + 
        scale_color_manual(values = target_col) + 
        scale_fill_gradient(low = "green2", high = "red2") + 
        scale_alpha(guide = F) + 
        scale_x_continuous(breaks = seq(102, 114, 1)) + 
        scale_y_continuous(breaks = seq(10, 22, 1)) + 
        ggtitle(sprintf("Bombing target map %s", year)) + 
        labs(x = "", y = "") + 
        theme(
            panel.background = element_blank()
            , axis.ticks = element_blank()
            , axis.line = element_line(size = 0.1)
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
