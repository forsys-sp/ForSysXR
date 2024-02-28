#' Explore defined data and ForSysX run before the run
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param area Field from input_shapefile containing the area of each stands
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param available Field from input_shapefile containing the available stands
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param land_cover Field from input_shapefile identifying the land cover in each stand
#' @param land_ownership Field from input_shapefile identifying the land ownership in each stand
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
                            area,
                            #available_for_management, #default is to not have stands with availability info
                            available,
                            #exclude_stands, #the default is not to have info on exclude in stands
                            exclude_field,
                            land_cover,
                            land_ownership,
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
    theshold_command_all <- character()

    for(i in 1:total_n_threshold){
      threshold_diss <- my_shp[,paste(threshold_df_final$my_threshold)[i]]

      colnames(threshold_diss)<-c("threshold_field","geometry")

      threshold_diss$bin <- 0


      theshold_command_use <- noquote(paste("threshold_field", noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" "))

      theshold_command_all <- c(theshold_command_all,theshold_command_use)

      threshold_diss <- within(threshold_diss, bin[eval(parse(text=theshold_command_use))] <- 1)



      threshold_diss <- threshold_diss %>%
        group_by(threshold_diss[,3][[1]]) %>%
        summarise(m = mean(threshold_diss[,3][[1]])) %>%
        st_cast()

      colnames(threshold_diss)<-c("threshold","m","geometry")

      threshold_diss <- st_transform(threshold_diss, crs = 4326)



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
                          labels=c('0'='Not considered','1'= paste(threshold_df_final$my_threshold[i],theshold_command_legend_lable,sep=" ")))


      suppressWarnings(assign(paste("threshold_plot_",i,sep=""),threshold_plot,pos = 1)) #,pos = 1
      suppressWarnings(assign(paste("threshold_diss_",i,sep=""),threshold_diss,pos = 1)) #,pos = 1
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





  list_threshold_diss_used <- (ls(envir = .GlobalEnv)[grep("threshold_diss_", ls(envir = .GlobalEnv))])
}


  #threshold_diss

  #threshold_df_final



  #suppressWarnings(rm(list = ls()[grep("threshold", ls())], envir = globalenv()))




#




  ###new plot test######

  all_groups_leaflet <- character()

  my_shp_diss <- st_transform(my_shp_diss, crs = 4326)

  available_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

    leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                         color = 'black', opacity=1,  weight=1, label=NA)


  if(!missing(available)){
  available_diss <- my_shp[,paste(available)][1]

  available_diss <- available_diss %>%
    group_by(available_diss[,1][[1]]) %>%
    summarise(m = mean(available_diss[,1][[1]])) %>%
    st_cast()

  colnames(available_diss)<-c("available","m","geometry")


  #convert shapefile to WGS84
  #st_crs(available_diss)

  available_diss$available_cha <- "Yes"
  available_diss <- within(available_diss, available_cha[available == 0] <- 'No')

  available_diss <- st_transform(available_diss, crs = 4326)
  my_shp_diss<- st_transform(my_shp_diss, crs = 4326)

  # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
  # #palFunc <- (c("white","grey"))

  palFunc <- leaflet::colorFactor(
    palette = c('white','grey80'),
    domain = available_diss$available_cha
  )


  available_plot_leaflet<- available_plot_leaflet %>%
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
                       opacity = 1)

  all_groups_leaflet<-c(all_groups_leaflet,"available")

  }

    #



  if(!missing(exclude_field)){
    exclude_diss <- my_shp[,paste(exclude_field)][1]

    exclude_diss <- exclude_diss %>%
      group_by(exclude_diss[,1][[1]]) %>%
      summarise(m = mean(exclude_diss[,1][[1]])) %>%
      st_cast()

    colnames(exclude_diss)<-c("exclude","m","geometry")

    exclude_diss$exclude_cha <- "Yes"
    exclude_diss <- within(exclude_diss, exclude_cha[exclude == 0] <- 'No')


    palFunc_exclude <- leaflet::colorFactor(
      palette = c('grey80', 'white'),
      domain = exclude_diss$exclude_cha
    )




    exclude_diss<- st_transform(exclude_diss, crs = 4326)


    available_plot_leaflet<- available_plot_leaflet %>%
      leaflet::addPolygons(data = exclude_diss, fillColor = ~palFunc_exclude(exclude_cha), fillOpacity = 0.75,
                         color = 'black', opacity = 1, weight=0.5, label = ~exclude_cha ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'exclude')%>%
      leaflet::addLegend(data=exclude_diss, "topright",
                         #colors = c('white', 'grey'),
                         pal = palFunc_exclude,
                         values = ~exclude_cha ,
                         title = "Exclude",
                         labels = c("No", "Yes"),
                         group = "exclude",
                         opacity = 1)


    all_groups_leaflet<-c(all_groups_leaflet,"exclude")

  }



  if(!missing(threshold)){

    if(nrow(threshold_df_final)==1){


      threshold_diss_1$threshold_original <- "Non-threshold-compliant"
      threshold_diss_1 <- within(threshold_diss_1, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[1],threshold_df_final$my_value_threshold[1],sep=" "))


      palFunc_threshold <- leaflet::colorFactor(
        palette = c('grey80', 'white'),
        domain = threshold_diss_1$threshold_original
      )



      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = threshold_diss_1, fillColor = ~palFunc_threshold(threshold_original), fillOpacity = 0.75,
                           color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'threshold_diss_1')%>%
        leaflet::addLegend(data=exclude_diss, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold,
                           values = ~threshold_diss_1$threshold_original ,
                           title = threshold_df_final$my_threshold[1],
                           labels = c("No", "Yes"),
                           group = "threshold_diss_1",
                           opacity = 1)

      all_groups_leaflet<-c(all_groups_leaflet,"threshold_diss_1")


      }


    if(nrow(threshold_df_final)==2){


      threshold_diss_1$threshold_original <- "Non-threshold-compliant"
      threshold_diss_1 <- within(threshold_diss_1, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[1],threshold_df_final$my_value_threshold[1],sep=" "))


      threshold_diss_2$threshold_original <- "Non-threshold-compliant"
      threshold_diss_2 <- within(threshold_diss_2, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[2],threshold_df_final$my_value_threshold[2],sep=" "))



      palFunc_threshold <- leaflet::colorFactor(
        palette = c('grey80', 'white'),
        domain = threshold_diss_1$threshold_original
      )



      palFunc_threshold2 <- leaflet::colorFactor(
        palette = c('grey80', 'white'),
        domain = threshold_diss_2$threshold_original
      )


      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = threshold_diss_1, fillColor = ~palFunc_threshold(threshold_original), fillOpacity = 0.75,
                                                   color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                                                   highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                                                   group = 'threshold_diss_1')%>%
        leaflet::addLegend(data=threshold_diss_1, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold,
                           values = ~threshold_diss_1$threshold_original ,
                           title = threshold_df_final$my_threshold[1],
                           labels = c("No", "Yes"),
                           group = "threshold_diss_1",
                           opacity = 1)%>%


        leaflet::addPolygons(data = threshold_diss_2, fillColor = ~palFunc_threshold2(threshold_original), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'threshold_diss_2')%>%
        leaflet::addLegend(data=threshold_diss_2, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold2,
                           values = ~threshold_diss_2$threshold_original ,
                           title = threshold_df_final$my_threshold[2],
                           labels = c("No", "Yes"),
                           group = "threshold_diss_2",
                           opacity = 1)


      all_groups_leaflet<-c(all_groups_leaflet,"threshold_diss_1","threshold_diss_2")


      }


  }




  if(!missing(land_cover)){
    land_cover_diss <- my_shp[,paste(land_cover)][1]

    land_cover_diss <- land_cover_diss %>%
      group_by(land_cover_diss[,1][[1]]) %>%
      summarise(m = mode(land_cover_diss[,1][[1]])) %>%
      st_cast()

    colnames(land_cover_diss)<-c("land_cover_use","m","geometry")
    land_cover_diss$land_cover_use <- factor(land_cover_diss$land_cover_use)
    land_cover_diss <- st_transform(land_cover_diss, crs = 4326)


    land_cover_diss$category <- 1:nrow(land_cover_diss)

    land_cover_diss$category <- factor(1:nrow(land_cover_diss))

    palFunc_landcover <- leaflet::colorFactor(terrain.colors(nrow(land_cover_diss)), land_cover_diss$land_cover_use)



    available_plot_leaflet<-available_plot_leaflet %>%
      leaflet::addPolygons(data = land_cover_diss, fillColor = ~palFunc_landcover(land_cover_use), fillOpacity = 0.75,
                           color = 'black', opacity = 1, weight=0.5, label = ~land_cover_use ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'land_cover_diss')%>%
      leaflet::addLegend(data=land_cover_diss, "topright",
                         #colors = c('white', 'grey'),
                         pal = palFunc_landcover,
                         values = ~land_cover_diss$land_cover_use ,
                         title = "Land cover",
                         labels = c("No", "Yes"),
                         group = "land_cover_diss",
                         opacity = 1)

    all_groups_leaflet<-c(all_groups_leaflet,"land_cover_diss")


  }




  if(!missing(land_ownership)){

    land_ownership_diss <- my_shp[,paste(land_ownership)][1]

    land_ownership_diss <- land_ownership_diss %>%
      group_by(land_ownership_diss[,1][[1]]) %>%
      summarise(m = mode(land_ownership_diss[,1][[1]])) %>%
      st_cast()

    colnames(land_ownership_diss)<-c("land_ownership_use","m","geometry")
    land_ownership_diss$land_ownership_use <- factor(land_ownership_diss$land_ownership_use)
    land_ownership_diss <- st_transform(land_ownership_diss, crs = 4326)


    #land_ownership_diss$category <- 1:nrow(land_ownership_diss)

    land_ownership_diss$category <- factor(1:nrow(land_ownership_diss))

    #palFunc_land_ownership <- leaflet::colorFactor(topo.colors(nrow(land_ownership_diss)), land_ownership_diss$land_ownership_use)

    palFunc_land_ownership <- leaflet::colorFactor(colorspace::diverge_hcl(nrow(land_ownership_diss)), land_ownership_diss$land_ownership_use)


    available_plot_leaflet<-available_plot_leaflet %>%
      leaflet::addPolygons(data = land_ownership_diss, fillColor = ~palFunc_land_ownership(land_ownership_use), fillOpacity = 0.75,
                           color = 'black', opacity = 1, weight=0.5, label = ~land_ownership_use ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'land_ownership_diss')%>%
      leaflet::addLegend(data=land_ownership_diss, "topright",
                         #colors = c('white', 'grey'),
                         pal = palFunc_land_ownership,
                         values = ~land_ownership_diss$land_ownership_use ,
                         title = "Land ownership",
                         labels = c("No", "Yes"),
                         group = "land_ownership_diss",
                         opacity = 1)

    all_groups_leaflet<-c(all_groups_leaflet,"land_ownership_diss")

  }




  available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addLayersControl(overlayGroups = all_groups_leaflet,
                                  options = leaflet::layersControlOptions(collapsed = F)) %>%

        leaflet::hideGroup(all_groups_leaflet)%>%
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)




  suppressWarnings(assign("available_plot_leaflet",available_plot_leaflet,pos = 1))





  ###tables#####

  #get total area

  total_area <- sum(my_shp[,paste(area)][[1]])

  #get available area
  available_area_sf <- subset(my_shp, get(available)==1)
  available_area<-sum(available_area_sf[,paste(area)][[1]])

  #get exclude area
  if(!missing(exclude_field)){
  not_exclude_area_sf <- subset(my_shp, get(exclude_field)==0)
  not_exclude_area<-sum(not_exclude_area_sf[,paste(area)][[1]])
  }else{
  not_exclude_area<-total_area
  }


  #get the area that meets the threshold(s)
  if(!missing(threshold)){

    threshold_df_final$area_considered <- 0

    for(e in 1:nrow(threshold_df_final)){
      threshold_df_loop_table <- threshold_df_final[e,]

      theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))

      threshold_loop_sf <- subset(my_shp, eval(parse(text=theshold_command_use_loop)))

      threshold_loop_area<-sum(threshold_loop_sf[,paste(area)][[1]])

      threshold_df_final$area_considered[e] <- threshold_loop_area


    }


  }else{
    threshold_df_final$area_considered <- total_area
  }



  #get the combination of all
  if(!missing(available) & !missing(exclude_field)){
    combination_area_sf <- subset(my_shp, get(available)==1 &
                                  get(exclude_field)==0)

    #threshold part
    if(!missing(threshold)){
      for(e in 1:nrow(threshold_df)){
        threshold_df_loop_table <- threshold_df[e,]

        theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))

        combination_area_sf <- subset(combination_area_sf, eval(parse(text=theshold_command_use_loop)))

      }
    }


  }

  combination_area <- sum(combination_area_sf[,paste(area)][[1]])


  #create a table with the area
  final_area_table_report <- data.frame()




  area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,threshold_df_final$area_considered,combination_area))
  colnames(area_considered_table) <- "Area_considered"
  row.names(area_considered_table) <- c("Stands total","Only available","Only not excluded",paste("Only ",theshold_command_all,sep=""),"Combination of criteria")



  suppressWarnings(assign("area_considered_table",area_considered_table,pos = 1))



  #then plot the combination of what we can treat after the availability and the combination of all thresholds

  path_for_rmd <- system.file("rmd_template", package = "ForSysXR")

  #check export html document
  save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

  capture.output(suppressWarnings(suppressMessages(generate_report_explore(output_file=paste("explore_",report_name,".html",sep="")))))

  file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

}
