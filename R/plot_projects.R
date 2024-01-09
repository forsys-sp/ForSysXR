#' Plots the projects created by a ForSysX run
#'
#' @param input_shapefile Shapefile containing the stands that were used in the prioritization process
#' @param outputs_base_name Path and name where the outputs should be stored
#'
#' @import dplyr sf ggplot2 shiny
#' @return
#' @export
#'
#' @examples

plot_projects <- function(input_shapefile,outputs_base_name){

  if(class(input_shapefile)[1]=="sf"){
    my_shp <- (input_shapefile)

  }

  if(class(input_shapefile)[1]=="character"){
    my_shp <- sf::st_read(input_shapefile)
    }


  all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

  all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
  all_elements_use <- as.character(all_elements_use)

  path_with_results <- paste(all_elements_use, collapse = '/')


  last_name <- all_elements[,ncol(all_elements)]

  number_scenarios_created <- intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = last_name))



    output_shp_run = number_scenarios_created

    setwd(path_with_results)
    #output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"))

    plot_data <- list()
    all_name_output_shiny <- data.frame()

    for(e in 1:length(number_scenarios_created)){
      output_shp_run_plot <- sf::st_read(paste(path_with_results,output_shp_run[[e]],sep="/"))

      name_output_shiny <- output_shp_run[[e]]

      plot_data[[paste("ttt_", e, sep = "")]] <- my_shp  %>%
        #mutate_at(c('diss'), ~na_if(., 0)) %>%
        #st_combine() %>%
        ggplot() +
        geom_sf(aes(fill=diss),fill="grey",color=NA) +
        #ggtitle("Projects ranking") +
        theme_void()+
        theme(plot.title=element_text(hjust=0.5))+
        #guides(fill="none")+
        geom_sf(data=output_shp_run_plot,aes(fill=ProjectNum),color=NA)+
        scale_fill_viridis_c(option = "turbo",direction=-1)+
        labs(fill='Project number')

      all_name_output_shiny <- rbind(all_name_output_shiny,name_output_shiny)

    }


    colnames(all_name_output_shiny)<-"names"

    ui <- shiny::fluidPage(
      shiny::titlePanel("ForSysX outputs"),
      shiny::mainPanel(
        shiny::plotOutput("current_plot"),
        shiny::fluidRow(
          shiny::column(12, shiny::textOutput("plot_number")),
          shiny::column(12, shiny::textOutput("plot_title"))
        ),
        shiny::actionButton("next_button", "Next Plot")
      )
    )

    # Define the server
    server <- function(input, output) {
      # Initialize reactive values
      current_plot_index <- shiny::reactiveVal(1)
      plot_titles <- shiny::reactiveVal(paste("File: ",all_name_output_shiny$names,sep = ""))

      # Function to render the current plot
      output$current_plot <- shiny::renderPlot({
        print(plot_data[[current_plot_index()]])
      })

      # Function to render the current plot number text
      output$plot_number <- shiny::renderText({
        paste("Plot", current_plot_index(), "of", length(plot_data))
      })

      # Function to render the current plot title
      output$plot_title <- shiny::renderText({
        plot_titles()[current_plot_index()]
      })

      # Observe the click event on the "Next Plot" button
      shiny::observeEvent(input$next_button, {
        # Increment the current plot index
        current_plot_index(current_plot_index() + 1)

        # Reset to the first plot if we reached the end
        if (current_plot_index() > length(plot_data)) {
          current_plot_index(1)
        }
      })
    }

    # Create the shiny app
    return(shiny::shinyApp(ui, server))

  }
