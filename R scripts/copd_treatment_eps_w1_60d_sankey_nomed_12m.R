# Author: Marleen Bokern
# Date: 04/2023
# Purpose: Create Sankey plot to depict changes in treatment over time, based on https://gist.github.com/matt-dray/a151158489ed090865c5c20a9ab966c2


#install.packages("networkD3")
#install.packages("openxlsx")
library(networkD3)
library(openxlsx)
library(htmlwidgets)
library(htmltools)
library(webshot) 
library(png) 
webshot::install_phantomjs()
setwd(Datadir_copd)

nodes <- data.frame(
  "name" = c(
    # Nodes at time = 1 (1 Mar 2020)
    "ICS group", # Node 0
    "LABA/LAMA group", # Node 1
    "Other inhaled medication", # Node 2
    "No medication",# Node 3
    # Nodes at time = 2 (1 Jun 2020)
    "ICS group", # Node 4
    "LABA/LAMA group", # Node 5
    "Other inhaled medication", # Node 6
    "No medication", #Node 7
    # Nodes at time = 3 (1 Sep 2020)
    "ICS group", # Node 8
    "LABA/LAMA group", # Node 9
    "Other inhaled medication", #Node 10
    "No medication",# Node 11
    # Nodes at time = 4 (1 Dec 2020)
    "ICS group", # Node 12
    "LABA/LAMA group", # Node 13
    "Other inhaled medication", #Node 14
    "No medication",# Node 15
    # Nodes at time = 3 (1 Mar 2021)
    "ICS group", # Node 16
    "LABA/LAMA group", # Node 17
    "Other inhaled medication", #Node 18
    "No medication"# Node 19
  ))

links60d <- read.xlsx("treatment_changes_w1_60d_sankey_nomed_12m.xlsx")
links60d$source <- as.numeric(links60d$source)
links60d$target <- as.numeric(links60d$target)

palette <- met.brewer("Tiepolo")
palette <- c(palette[1], palette[3], palette[5], palette[7])
palette_js <- paste0("'", paste(palette, collapse = "','"), "'")
colors <- paste0("d3.scaleOrdinal([", palette_js, "])")

# Build the network
sankey60d <- sankeyNetwork(
  Links = links60d, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  fontSize = 12, nodeWidth = 15, colourScale = colors
)


sankey60d <- htmlwidgets::prependContent(sankey60d, htmltools::tags$h1("Treatment changes from 1st March 2020 to 1st March 2021",  style = "font-family: Garamond; font-size: 22px"),htmltools::tags$h4("Discontinuation defined as 60 days after presumed end of exposure", style = "font-family: Garamond; font-size: 16px"))


sankey60d

files_to_overwrite <- c("J:/EHR-Working/Marleen/ICScovid/R/treatment_changes_w1_60d_sankey_nomed_12m.html", 
                        "J:/EHR-Working/Marleen/ICScovid/R/treatment_changes_w1_60d_sankey_files_nomed_12m")

#check if files already exist
for (file in files_to_overwrite) {
  if (file.exists(file)) {
    # If the file exists, delete it
    file.remove(file)
  }
}

htmlwidgets::saveWidget(sankey60d, file = "J:/EHR-Working/Marleen/ICScovid/R/sankey/treatment_changes_w1_60d_sankey_nomed_12m.html")

# Capture the Sankey plot as an image using webshot
webshot::webshot("J:/EHR-Working/Marleen/ICScovid/R/sankey/treatment_changes_w1_60d_sankey_nomed_12m.html", "J:/EHR-Working/Marleen/ICScovid/R/sankey/treatment_changes_w1_60d_sankey_nomed_12m.png", vwidth = 1000, vheight = 900)
