##%######################################################%##
#                                                          #
####                Set up data for app                 ####
#                                                          #
##%######################################################%##

# Libraries
library(shiny)
library(shinydashboard)
library(DT)

# Read in data
latest_visit <-
    ENDIA::get_latest_visit() |>
    dplyr::mutate(site = stringr::str_extract(structured_participant_id, "[A-Z]*\\-[A-Z]*\\-[A-Z]*")) |>
    dplyr::relocate(site, .after = structured_participant_id)

# All visits
all_visits <-
    ENDIA::get_visit_urls() |>
    dplyr::mutate(site = stringr::str_extract(structured_participant_id, "[A-Z]*\\-[A-Z]*\\-[A-Z]*")) |>
    dplyr::relocate(site, .after = structured_participant_id)

# Get current sites
current_sites <- ENDIA::get_current_site()

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- dashboardPage(
    dashboardHeader(title = "ENDIA Participant Dashboard"),
    dashboardSidebar(
        selectInput("site", "Choose a site:",
                    choices = unique(all_visits$site), # Assuming 'Site' column exists
                    selected = NULL),
        numericInput("participant", "Enter Participant Number:", value = 1, min = 1)
    ),
    dashboardBody(
        box(title = "Latest Visit", status = "primary",width = 12,
            dataTableOutput("latestVisitOutput"))
    )
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {

    output$latestVisitOutput <- renderDataTable({
        df <- latest_visit

        # Get structured_participant_id from input
        id_site <- input$site
        id_num <- input$participant |> stringr::str_pad(width = 3, side = "left", pad = "0")

        participant_id = paste0(id_site, "-", id_num)

        # Apply filters
        if (!is.null(input$site)) {
            df <- df[df$structured_participant_id == participant_id, ]
        }

        return(df)
    })
}

##%######################################################%##
#                                                          #
####                Run the application                 ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
