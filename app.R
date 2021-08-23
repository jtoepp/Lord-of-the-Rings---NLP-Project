# Load libraries
# specify the packages of interest
packages = c("rsconnect"
             , "shiny"
             , "shinythemes"
             , "leaflet"
             , "plotly"
             , "tidytext"
             , "ggwordcloud"
             , "tidyverse"
             , "tidyquant"
             , "tidyr")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# USER INTERFACE ---- 
ui <- fluidPage(
  
)





# SERVER ---- 
server <- function(input, output) {
  
}



# DEPLOY ---- 
shinyApp(ui = ui, server = server)