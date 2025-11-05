library(BSol.mapR)
library(dplyr)
library(tmap)
library(sf)
library(readxl)
library(stringr)

source("R/config.R")

credits <- paste("Contains OS data \u00A9 Crown copyright and database right",
                 # Get current year
                 format(Sys.Date(), "%Y"),
                 ". Source:\nOffice for National Statistics licensed under the Open Government Licence v.3.0."
)

################################################################################
#                        Load and filter map shape data                        #
################################################################################

WM_LAs <- st_read(
  "data/WestMidLAs/WestMidLAs.shp"
) %>%
  # Coventry not part of the new ICB footprint
  filter(
    LAD24NM != "Coventry"
  )

################################################################################
#                       Child Menthal Health (2023-24)                         #
################################################################################
child_mh_path <- file.path(
  child_data_path,
  "REQ3313_MentalHealthconditionsAdmissions_2023-2024_WestMidlands_Under18_CrudeRates.xlsx"
)

child_mh_data <- read_excel(child_mh_path)

conditions <- unique(child_mh_data$Condition)

for (condition_i in conditions) {
  mh_data_i <- child_mh_data %>%
    filter(
      Condition == condition_i
      )
  
  mh_shape_i <- WM_LAs %>%
    left_join(
      mh_data_i,
      by = join_by("LAD24CD" == "LA Code")
    ) %>%
    mutate(
      `Crude Rate per 100,000 pop` = ifelse(
        is.na(`Crude Rate per 100,000 pop`), 
        0, 
        `Crude Rate per 100,000 pop`
      ),
      PlotVal = round(`Crude Rate per 100,000 pop`, 1)
    )
  
  title_i <- 
    str_wrap(
      paste(
        condition_i,
        "crude rate per 100,000 residents aged <18 yrs",
        "(2023/24)"
      ),
      60
    )
  
  map_i <- tm_shape(mh_shape_i) +
    tm_fill(
      "Crude Rate per 100,000 pop",
      fill.scale = tm_scale_continuous(),
      fill.legend = tm_legend(
        title = ""
      )
    ) +
    tm_text("PlotVal") + 
    tm_borders() +
    tm_title(
      text = title_i
    ) +
    tm_layout(
      legend.position = c("LEFT", "TOP"),
      legend.frame.alpha = 0,
      legend.frame.lwd = 0,
      legend.height = 10,
      title.frame = FALSE,
      title.padding = c(0.03, 0.15, 0, 0.03),
      frame = FALSE,
      inner.margins = c(0.1, 0.13, -0.03, 0.03),
    ) +
    tm_credits(
      credits,
      size = 0.8,
      position = c("LEFT", "BOTTOM")
    ) 
  
  save_path = file.path(
    "output",
    "HES",
    paste0(
      str_replace_all(condition_i, " ", "_"),
      "23-24",
      ".png"
    )
  )
  
  save_map(map_i, save_path, width = 5, height = 4)
}