library(BSol.mapR)
library(dplyr)
library(tmap)
library(sf)
library(readxl)
library(stringr)
library(fingertipsR)

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
#                       Collect and map FingerTips data                        #
################################################################################

ft_indicators <- read_excel("data/FingerTips-Indicators.xlsx")

ft_ids <- ft_indicators$IndicatorID

for (ft_id_i in ft_ids) {
  print(ft_id_i)
  data_i <- fingertips_data(
    AreaTypeID = 502,
    IndicatorID = ft_id_i
  ) %>%
    filter(
      TimeperiodSortable == max(TimeperiodSortable),
      Sex == "Persons",
      AreaCode %in% WM_LAs$LAD24CD
    ) %>%
    mutate(
      PlotVal = paste0(
        round(Value, 1),
        "\n(", round(LowerCI95.0limit, 1), " - ",
        round(UpperCI95.0limit, 1), ")"
      )
    )
  
  map_data_i <- WM_LAs %>%
    left_join(
      data_i,
      by = join_by("LAD24CD" == "AreaCode")
    ) 
  
  map_title_i <- str_wrap(
    paste0(
      ft_indicators$MapTitle[ft_indicators$IndicatorID == ft_id_i],  
      " (", unique(data_i$Timeperiod), ")"
      ), 
    60)


  
  map_i <- tm_shape(map_data_i) +
    tm_fill(
      "Value",
      fill.scale = tm_scale_continuous(),
      fill.legend = tm_legend(
        title = ""
      )
    ) +
    tm_text(
      "PlotVal",
      size = 0.6
      ) + 
    tm_borders() +
    tm_title(
      text = map_title_i
    ) +
    tm_layout(
      legend.position = c("LEFT", "TOP"),
      legend.frame.alpha = 0,
      legend.frame.lwd = 0,
      legend.height = 10,
      title.frame = FALSE,
      title.padding = c(0.03, 0.15, 0, 0.03),
      frame = FALSE,
      inner.margins = c(0.11, 0.15, -0.03, 0.03),
    ) +
    tm_credits(
      credits,
      size = 0.8,
      position = c("LEFT", "BOTTOM")
    ) 
  
  save_path = file.path(
    "output",
    "FingerTips",
    ft_indicators$Theme[ft_indicators$IndicatorID == ft_id_i],
    paste0(
      ft_indicators$IndicatorShortName[ft_indicators$IndicatorID == ft_id_i],
      ".png")
  )
  
  save_map(map_i, save_path, width = 5, height = 4)
}