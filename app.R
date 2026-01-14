# app.R
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui_caroline, server = server_caroline)