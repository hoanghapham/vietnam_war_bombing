pkg_list = c(
    "data.table"
    , "ggplot2"
    , "scales"
    , "dplyr"
    , "lubridate"
    , "ggmap"
    , "knitr"
    , "gridExtra"
    , "gganimate"
    , "stringi"
    , "plotly"
)

script_list = c(
    "lib/utils.R"
)

for (script in script_list)
{
    source(script)
}

load_libraries(pkg_list)

# Turn on plotly
Sys.setenv("plotly_username"="hoanghapham")
Sys.setenv("plotly_api_key"="9TsLOjDB7ownFi05uscy")
