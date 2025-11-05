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
  child_mh_data_i <- child_mh_data %>%
    filter(
      Condition == condition_i
    ) %>%
    mutate(
      magnitude = 100000,
      Z = qnorm(0.975),
      Count = `No of Admissions_Under18`,
      Denominator = `Population_Under18`,
      p_hat = Count / Denominator,
      a_prime = Count + 1,
      Value = magnitude * Count / Denominator,
      Lower95 = magnitude * Count * (1 - 1/(9*Count) - Z/3 * sqrt(1/a_prime))**3/Denominator,
      Upper95 = magnitude * a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/Denominator
    )
  
  child_mh_shape_i <- WM_LAs %>%
    left_join(
      child_mh_data_i,
      by = join_by("LAD24CD" == "LA Code")
    ) %>%
    mutate(
      `Value` = ifelse(
        is.na(`Value`), 
        0, 
        `Value`
      ),
      CI = case_when(
        Value == 0 ~ "",
        TRUE ~ paste0("(", round(Lower95, 1), " - ", round(Upper95, 1), ")")
      ),
      PlotVal = paste0(
        round(Value, 1),
        "\n",CI
      )
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
  
  map_i <- tm_shape(child_mh_shape_i) +
    tm_fill(
      "Value",
      fill.scale = tm_scale_continuous(),
      fill.legend = tm_legend(
        title = ""
      )
    ) +
    tm_text("PlotVal",
            size = 0.6) + 
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
      inner.margins = c(0.11, 0.18, -0.0, 0.03)
    ) +
    tm_credits(
      credits,
      size = 0.8,
      position = c("LEFT", "BOTTOM")
    ) 
  
  save_path = file.path(
    "output",
    "Child Mental Health",
    paste0(
      str_replace_all(condition_i, " ", "_"),
      "23-24",
      ".png"
    )
  )
  
  save_map(map_i, save_path, width = 5, height = 4)
}

################################################################################
#                              CVD (2023-24)                                   #
################################################################################
cvd_path <- file.path(
  child_data_path,
  "REQ3313_CVDAdmissions_2023-2024_WestMidlands_All Ages_CrudeRates.xlsx"
)

cdv_data <- read_excel(cvd_path)

conditions2 <- unique(cdv_data$Condition)

for (condition_i in conditions2) {
  cvd_data_i <- cdv_data %>%
    filter(
      Condition == condition_i
    ) %>%
    mutate(
      magnitude = 100000,
      Z = qnorm(0.975),
      Count = `No of Admissions_All Ages`,
      Denominator = `Population_All Ages`,
      p_hat = Count / Denominator,
      a_prime = Count + 1,
      Value = magnitude * Count / Denominator,
      Lower95 = magnitude * Count * (1 - 1/(9*Count) - Z/3 * sqrt(1/a_prime))**3/Denominator,
      Upper95 = magnitude * a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/Denominator
    )
  
  cvd_shape_i <- WM_LAs %>%
    left_join(
      cvd_data_i,
      by = join_by("LAD24CD" == "LA Code")
    ) %>%
    mutate(
      `Crude Rate per 100,000 pop` = ifelse(
        is.na(`Crude Rate per 100,000 pop`), 
        0, 
        `Crude Rate per 100,000 pop`
      ),
      CI = case_when(
        `Crude Rate per 100,000 pop` == 0 ~ "",
        TRUE ~ paste0(round(Lower95, -1), " - ", round(Upper95, -1), ")")
        ),
      PlotVal = paste0(
        round(Value, -1),
        "\n(",CI
      )
    )
  
  title_i <- 
    str_wrap(
      paste(
        condition_i,
        "crude rate per 100,000 residents",
        "(2023/24)"
      ),
      60
    )
  
  map_i <- tm_shape(cvd_shape_i) +
    tm_fill(
      "Crude Rate per 100,000 pop",
      fill.scale = tm_scale_continuous(),
      fill.legend = tm_legend(
        title = ""
      )
    ) +
    tm_text("PlotVal",
            size = 0.6) + 
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
      inner.margins = c(0.11, 0.18, -0.0, 0.03)
    ) +
    tm_credits(
      credits,
      size = 0.8,
      position = c("LEFT", "BOTTOM")
    ) 
  
  save_path = file.path(
    "output",
    "CVD",
    paste0(
      str_replace_all(condition_i, " ", "_"),
      "23-24",
      ".png"
    )
  )
  
  save_map(map_i, save_path, width = 5, height = 4)
}
