pkg_list = c(
    "data.table"
    , "ggplot2"
    , "scales"
    , "dplyr"
    , "lubridate"
    , "ggmap"
    , "knitr"
)

script_list = c(
    "lib/utils.R"
)

for (script in script_list)
{
    source(script)
}

load_libraries(pkg_list)