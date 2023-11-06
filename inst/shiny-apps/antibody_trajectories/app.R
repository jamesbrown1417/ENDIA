##%######################################################%##
#                                                          #
####                Set up data for app                 ####
#                                                          #
##%######################################################%##

# Libraries
library(shiny)
library(DT)

# Read in data------------------------------------------------------------------

# Get current sites
current_sites <- ENDIA::get_current_site()

# Get antibody data
antibody_data <- ENDIA::get_processed_data_table()

# Create a function to plot a participants visits over time
plot_participant_visits <- function(participant_id) {
    all_visits |>
        dplyr::filter(structured_participant_id == participant_id) |>
        ggplot2::ggplot(ggplot2::aes(x = visit_date, y = visit_number)) +
        ggplot2::geom_point(colour = "orangered1",
                            size = 3,
                            alpha = 1) +
        ggplot2::labs(
            x = "Visit Date",
            y = "Visit Number",
            title = paste("Visits for:", participant_id)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(family = "Arial", face = "bold"),
            plot.title = ggplot2::element_text(hjust = 0.5, size = 20),
            axis.title.x = ggplot2::element_text(size = 14),
            axis.title.y = ggplot2::element_text(size = 14),
            legend.position = "none"
        )
}


plot_participant_visits("SA-ADEL-WCH-001")

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

# Create sidebar panel with input for participant ID and main panel with plot output
ui <- shiny::navbarPage(
    title = "ENDIA Participant Visits",
    theme = shinythemes::shinytheme("flatly"),
    tabPanel(title = "Plot",
             sidebarLayout(
                 sidebarPanel(
                     shiny::selectInput(
                         inputId = "participant_site",
                         label = "Select Current Site:",
                         choices = current_sites$current_site,
                         multiple = FALSE
                     ),
                     shiny::numericInput(
                         inputId = "participant_id",
                         label = "Select Participant ID Number: ",
                         min = 1,
                         step = 1,
                         max = 286,
                         value = 1
                     )
                 ),
                 mainPanel(
                     plotOutput(
                         outputId = "participant_visits",
                         height = "600px",
                         width = "100%"
                     )
                 )
             )),
    tabPanel(
        title = "Table",
        DT::dataTableOutput(outputId = "participant_visits_table",
                            width = "100%")
    )
)


##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##



##%######################################################%##
#                                                          #
####                Run the application                 ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
