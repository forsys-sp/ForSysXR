#' Explore defined data and ForSysX run before the run
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param available Field from input_shapefile containing the available stands
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param constraints Vector containing the constraint(s) field, the constraint value (or minimum, maximum and step to be used), and the slack value to be used. The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param constraints_logic Vector with two elements containing the constraint logic. The first element refers to if a single value should be used for the constraint ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple constraints must be combined, either selecting stands where all constraints are met ("and") or select stands where any of the constraints are met ("or"). Default is c("single_value","and")
#' @param threshold Vector containing the threshold(s) field, the symbol of inequality or equality (">","<","==",">=","<="), and the threshold value (or minimum, maximum and step to be used). The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param threshold_logic Vector with two elements containing the threshold logic. The first element refers to if a single value should be used for the threshold ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple thresholds must be combined, either selecting stands where all thresholds are met ("and") or select stands where any of the thresholds are met ("or"). Default is c("single_value","and")
#' @param report_name Name to be used in the html report file
#' @return
#' @export
#' @examples
explore <- function(input_shapefile,
                    report_name,
                            stand_id,
                            #area,
                            #available_for_management, #default is to not have stands with availability info
                            available,
                            #exclude_stands, #the default is not to have info on exclude in stands
                            exclude_field,
                            #load_objective_steps,
                            #step_file,
                            #objective_direction,
                            #patch_buster,
                            #weight,
                            #patch_identifier,
                            #patchbuster_identifier,
                            #patchbuster_weight,
                            #constraints_name,
                            #constraints_value,
                            #constraints_slack ="",
                            constraints,
                            constraints_logic = c("single_value","and"),
                            #effect_fields,
                            #objectives,
                            threshold,
                            threshold_logic = c("single_value","and")#,
                            #subunit_field,
                            #master_subunit
) {


  if(class(input_shapefile)[1]=="character"){
    input_shapefile_format <- substrRight(input_shapefile,4)
    if(input_shapefile_format!= ".shp")
      stop("input_shapefile has to be a shapefile!")}



  if(class(input_shapefile)[1]=="sf"){
    my_shp <- (input_shapefile)

    #if(max(nchar(names(my_shp)))>10){
    #  stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
    #}
  }




  if(class(input_shapefile)[1]=="character"){

      my_shp <- sf::st_read(input_shapefile,quiet=TRUE)

      #if(max(nchar(names(my_shp)))>10){
      #  stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
      #}

    }


  #study area contour
  my_shp$dissp <- 1
  my_shp_diss <- my_shp %>%
    group_by(dissp) %>%
    summarise(m = mean(dissp)) %>%
    st_cast()

  #plotting

  #availability
  available_diss <- my_shp[,paste(available)][1]

  available_diss <- available_diss %>%
    group_by(available_diss[,1][[1]]) %>%
    summarise(m = mean(available_diss[,1][[1]])) %>%
    st_cast()

  colnames(available_diss)<-c("available","m","geometry")

  available_plot <- available_diss  %>%
    #mutate_at(c('diss'), ~na_if(., 0)) %>%
    #st_combine() %>%
    ggplot() +
    geom_sf(aes(fill=factor(available)),color="black") +
    #ggtitle("Projects ranking") +
    theme_void()+
    theme(plot.title=element_text(hjust=0.5))+
    #guides(fill="none")+
    labs(fill='Availability')+
    scale_fill_manual(values=c("white", "grey80"),
                      labels=c('0'='Not available','1'= 'Available'))


  suppressWarnings(assign("available_plot",available_plot,pos = 1))


  #convert shapefile to WGS84
  #st_crs(available_diss)

  available_diss$available_cha <- "Available"
  available_diss <- within(available_diss, available_cha[available == 0] <- 'Not available')

  available_diss <- st_transform(available_diss, crs = 4326)
  my_shp_diss<- st_transform(my_shp_diss, crs = 4326)

  # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
  # #palFunc <- (c("white","grey"))

  palFunc <- leaflet::colorFactor(
    palette = c('grey80', 'white'),
    domain = available_diss$available_cha
  )


  available_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', leaflet::providerTileOptions(minZoom = 4, maxZoom = 15)) %>%
    leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                         color = 'black', opacity=1,  weight=1, label=NA) %>%
    leaflet::addPolygons(data = available_diss, fillColor = ~palFunc(available_cha), fillOpacity = 0.75,
                color = 'black', opacity = 1, weight=0.5, label = ~available_cha ,stroke = TRUE,
                highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                group = 'available')%>%
    leaflet::addLegend(data=available_diss, "topright",
                       #colors = c('white', 'grey'),
                       pal = palFunc,
                       values = ~available_cha ,
            title = "Availability",
            labels = c("Not available", "Available"),
            opacity = 1)


  suppressWarnings(assign("available_plot_leaflet",available_plot_leaflet,pos = 1))



  #exclude
  exclude_diss <- my_shp[,paste(exclude_field)][1]

  exclude_diss <- exclude_diss %>%
    group_by(exclude_diss[,1][[1]]) %>%
    summarise(m = mean(exclude_diss[,1][[1]])) %>%
    st_cast()

  colnames(exclude_diss)<-c("exclude","m","geometry")

  exclude_plot <- exclude_diss  %>%
    #mutate_at(c('diss'), ~na_if(., 0)) %>%
    #st_combine() %>%
    ggplot() +
    geom_sf(aes(fill=factor(exclude)),color="black") +
    #ggtitle("Projects ranking") +
    theme_void()+
    theme(plot.title=element_text(hjust=0.5))+
    #guides(fill="none")+
    labs(fill='Exclusion')+
    scale_fill_manual(values=c("grey80", "white"),
                      labels=c('0'='Not exclude','1'= 'exclude'))


  suppressWarnings(assign("exclude_plot",exclude_plot,pos = 1))





  palFunc_exclude <- leaflet::colorFactor(
    palette = c('grey80', 'white'),
    domain = exclude_diss$exclude
  )



  exclude_diss<- st_transform(exclude_diss, crs = 4326)

  available_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', leaflet::providerTileOptions(minZoom = 4, maxZoom = 15)) %>%
    leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                         color = 'black', opacity=1,  weight=1, label=NA) %>%
    leaflet::addPolygons(data = available_diss, fillColor = ~palFunc(available_cha), fillOpacity = 0.75,
                         color = 'black', opacity = 1, weight=0.5, label = ~available_cha ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'available')%>%
    leaflet::addLegend(data=available_diss, "topright",
                       #colors = c('white', 'grey'),
                       pal = palFunc,
                       values = ~available_cha ,
                       title = "Available",
                       labels = c("No", "Yes"),
                       group = "available",
                       opacity = 1)%>%

    leaflet::addPolygons(data = exclude_diss, fillColor = ~palFunc_exclude(exclude), fillOpacity = 0.75,
                         color = 'black', opacity = 1, weight=0.5, label = ~exclude ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'exclude')%>%
    leaflet::addLegend(data=exclude_diss, "topright",
                       #colors = c('white', 'grey'),
                       pal = palFunc_exclude,
                       values = ~exclude ,
                       title = "Exclude",
                       labels = c("No", "Yes"),
                       group = "exclude",
                       opacity = 1) %>%

    leaflet::addLayersControl(overlayGroups = c("available", "exclude"),
                     options = leaflet::layersControlOptions(collapsed = F))




  suppressWarnings(assign("available_plot_leaflet",available_plot_leaflet,pos = 1))









  #threshold
  #first plot individual thresholds

  #get the number of thresholds

  if(!missing(threshold)){
    #if(length(threshold) !=3){
    #  stop("Wrong number of elements used in threshold (expected 3 elements). If using multiple thresholds, please specify the threshold_logic.")
    #}
    total_n_threshold <- base::length(threshold)
    total_n_threshold <- total_n_threshold/3

    #integer_val <- total_n_threshold%%1==0

    integer_val <- decimalplaces(total_n_threshold)

    if(integer_val != 0)
      stop("Wrong number of arguments when defining the threshold")

    if(total_n_threshold > 6)
      stop("Maximum number of threshold reached. Maximum number allowd is 6")

    threshold_df_final<-data.frame()
    for (k in 1:total_n_threshold){

      position_threshold <- (k-1)*3
      my_threshold<-threshold[position_threshold+1]
      my_operator<-threshold[position_threshold+2]
      my_value_threshold <- threshold[position_threshold+3]

      threshold_df <- data.frame(cbind(my_threshold,my_operator,my_value_threshold))
      threshold_df_final <- rbind(threshold_df_final,threshold_df)
    }



    #plot the thresholds

    for(i in 1:total_n_threshold){
      threshold_diss <- my_shp[,paste(threshold_df_final$my_threshold)[i]]

      colnames(threshold_diss)<-c("threshold_field","geometry")

      threshold_diss$bin <- 0


      theshold_command_use <- noquote(paste("threshold_field", noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" "))



      threshold_diss <- within(threshold_diss, bin[eval(parse(text=theshold_command_use))] <- 1)



      threshold_diss <- threshold_diss %>%
        group_by(threshold_diss[,3][[1]]) %>%
        summarise(m = mean(threshold_diss[,3][[1]])) %>%
        st_cast()

      colnames(threshold_diss)<-c("threshold","m","geometry")


      theshold_command_legend_lable <- noquote(paste(noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" "))

      threshold_plot <- threshold_diss  %>%
        #mutate_at(c('diss'), ~na_if(., 0)) %>%
        #st_combine() %>%
        ggplot() +
        geom_sf(aes(fill=factor(threshold)),color="black") +
        #ggtitle("Projects ranking") +
        theme_void()+
        theme(plot.title=element_text(hjust=0.5))+
        #guides(fill="none")+
        labs(fill=threshold_df_final$my_threshold[1])+
        scale_fill_manual(values=c("white", "grey80"),
                          labels=c('0'='Not considered','1'= theshold_command_legend_lable))


      suppressWarnings(assign(paste("threshold_plot_",i,sep=""),threshold_plot,pos = 1)) #,pos = 1
      rm(threshold_plot)
    }




    #count how many thresholds we have in the environment. Then set the number of cols and rows for ggarrange




    list_threshold_plots <- (ls(envir = .GlobalEnv)[grep("threshold_plot_", ls(envir = .GlobalEnv))])

    my_list_final <- list()
    for(k in 1:length(list_threshold_plots)){
      my_list <- list(get(list_threshold_plots[[k]]))
      my_list_final <- c(my_list_final,my_list)
    }



    threshold_final_figure <- ggpubr::ggarrange(plotlist=my_list_final,
                      ncol = 1,nrow=length(list_threshold_plots),common.legend = FALSE)

    suppressWarnings(assign("threshold_final_figure",threshold_final_figure,pos = 1))
    suppressWarnings(assign("n_threshold_plots",length(list_threshold_plots),pos = 1))
}



  #then plot the combination of what we can treat after the availability and the combination of all thresholds

  path_for_rmd <- system.file("rmd_template", package = "ForSysXR")

  #check export html document
  save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

  capture.output(suppressWarnings(suppressMessages(generate_report_explore(output_file=paste("explore_",report_name,".html",sep="")))))

  file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

}
