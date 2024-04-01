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
library(MetBrewer)
webshot::install_phantomjs()
setwd(Datadir_copd)

sankey_name <- "treatment_changes_60d_sankey_nomed_12m_w1"

nodes <- data.frame(
  "name" = c(
    # Nodes at time = 1 (1 Mar 2020)
    "ICS/LABA", # Node 0
    "LABA/LAMA", # Node 1
    "Other", # Node 2
    "No medication",# Node 3
    # Nodes at time = 2 (1 Jun 2020)
    "ICS/LABA", # Node 4
    "LABA/LAMA", # Node 5
    "Other", # Node 6
    "No medication", #Node 7
    # Nodes at time = 3 (1 Sep 2020)
    "ICS/LABA", # Node 8
    "LABA/LAMA", # Node 9
    "Other", #Node 10
    "No medication",# Node 11
    # Nodes at time = 4 (1 Dec 2020)
    "ICS/LABA", # Node 12
    "LABA/LAMA", # Node 13
    "Other", #Node 14
    "No medication",# Node 15
    # Nodes at time = 3 (1 Mar 2021)
    "ICS/LABA", # Node 16
    "LABA/LAMA", # Node 17
    "Other", #Node 18
    "No medication"# Node 19
  ))

links60d <- read.xlsx(paste0(sankey_name, ".xlsx"))
links60d$source <- as.numeric(links60d$source)
links60d$target <- as.numeric(links60d$target)

palette <- met.brewer("Cassatt2")
palette <- c(palette[9], palette[8], palette[7], palette[6])
palette_js <- paste0("'", paste(palette, collapse = "','"), "'")

colors <- paste0("d3.scaleOrdinal([", palette_js, "])")

# Build the network
sankey60d <- sankeyNetwork(
  Links = links60d, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name", fontFamily = "sans-serif",
  fontSize = 10, nodeWidth = 15, colourScale = colors
)

sankey60d <- htmlwidgets::prependContent(sankey60d, htmltools::tags$h1("Treatment changes from 1st March 2020 to 1st March 2021",  style = "font-family: Garamond; font-size: 22px"),htmltools::tags$h4("Discontinuation defined as 60 days after presumed end of exposure", style = "font-family: Garamond; font-size: 16px"))


sankey60d

full_dir <- paste0(Projectdir, "sankey/", sankey_name)

# Ensure the directory exists or create it if necessary
if (!dir.exists(full_dir)) {
  dir.create(full_dir, recursive = TRUE)
}

# Define the HTML file path
html_file <- paste0(Projectdir, "sankey/", sankey_name, ".html")
print(html_file)

# Save the HTML widget using htmltools::save_html
htmlwidgets::saveWidget(
  sankey60d,
  file = html_file,
  selfcontained = TRUE
)

# Capture the Sankey plot as an image using webshot
webshot::webshot(
  url = paste0(Projectdir, "sankey/", sankey_name, ".html"),
  file = paste0(Projectdir, "sankey/", sankey_name, ".png"),
  vwidth = 1000,
  vheight = 900
)

# Define the folder name to check for existence
folder_to_check <- paste0(stringr::str_remove(html_file, ".html"), "_files")

unlink(folder_to_check, recursive = TRUE)

