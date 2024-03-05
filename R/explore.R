#' Explore defined data and ForSysX run before the run
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param area Field from input_shapefile containing the area of each stands
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param available Field from input_shapefile containing the available stands
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param land_cover Field from input_shapefile identifying the land cover in each stand
#' @param land_ownership Field from input_shapefile identifying the land ownership in each stand
#' @param objectives Vector containing the objective(s) field from input_shapefile
#' @param threshold Vector containing the threshold(s) field, the symbol of inequality or equality (">","<","==",">=","<="), and the threshold value (or minimum, maximum and step to be used). The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param threshold_logic Vector with two elements containing the threshold logic. The first element refers to if a single value should be used for the threshold ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple thresholds must be combined, either selecting stands where all thresholds are met ("and") or select stands where any of the thresholds are met ("or"). Default is c("single_value","and")
#' @param report_name Name to be used in the html report file
#' @param export_static_report Logical. If TRUE, then an additional HTML report will be saved with static figures. Default is FALSE
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
                            #constraints,
                            #constraints_logic = c("single_value","and"),
                            #effect_fields,
                            objectives,
                            threshold,
                            threshold_logic = c("single_value","and"),
                            export_static_report = FALSE
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
    sf::st_cast()

  #plotting







  #threshold
  #first plot individual thresholds

  #get the number of thresholds
  threshold_df_final<-data.frame()
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
    theshold_command_all_legend <- character()

    for(i in 1:total_n_threshold){
      threshold_diss <- my_shp[,paste(threshold_df_final$my_threshold)[i]]

      colnames(threshold_diss)<-c("threshold_field","geometry")

      threshold_diss$bin <- 0

      if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
        theshold_command_use <- suppressWarnings(noquote(paste("threshold_field", noquote(threshold_df_final$my_operator[i]),  as.character(threshold_df_final$my_value_threshold[i]),sep=" ")))
        theshold_command_use_legend <- noquote(paste(noquote(threshold_df_final$my_threshold[i]), noquote(threshold_df_final$my_operator[i]), as.character(threshold_df_final$my_value_threshold[i]),sep=" "))

        #put quoatation marks in last element
        theshold_command_use <- suppressWarnings(c(noquote(paste("threshold_field", noquote(threshold_df_final$my_operator[i]),sep=" ")),as.character(paste("'",threshold_df_final$my_value_threshold[i],"'",sep=""))))
        threshold_diss <- within(threshold_diss, bin[eval(parse(text=theshold_command_use))] <- 1)

        theshold_command_legend_lable <- noquote(paste(noquote(threshold_df_final$my_operator[i]), as.character(threshold_df_final$my_value_threshold[i]),sep=" "))

      }else{
        theshold_command_use <- suppressWarnings(noquote(paste("threshold_field", noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" ")))
        theshold_command_use_legend <- noquote(paste(noquote(threshold_df_final$my_threshold[i]), noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" "))
        threshold_diss <- within(threshold_diss, bin[eval(parse(text=theshold_command_use))] <- 1)
        theshold_command_legend_lable <- noquote(paste(noquote(threshold_df_final$my_operator[i]), as.numeric(threshold_df_final$my_value_threshold[i]),sep=" "))
      }



      theshold_command_all <- c(theshold_command_all,theshold_command_use)

      theshold_command_all_legend <- c(theshold_command_all_legend,theshold_command_use_legend)






      threshold_diss <- threshold_diss %>%
        group_by(threshold_diss[,3][[1]]) %>%
        summarise(m = mean(threshold_diss[,3][[1]])) %>%
        st_cast()

      colnames(threshold_diss)<-c("threshold","m","geometry")

      threshold_diss <- st_transform(threshold_diss, crs = 4326)





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




  #same for objectives

  if(!missing(objectives)){
    #if(length(objectives) !=3){
    #  stop("Wrong number of elements used in objectives (expected 3 elements). If using multiple objectivess, please specify the objectives_logic.")
    #}
    total_n_objectives <- base::length(objectives)
    #total_n_objectives <- total_n_objectives/3

    #integer_val <- total_n_objectives%%1==0

    #integer_val <- decimalplaces(total_n_objectives)

    if(integer_val != 0)
      stop("Wrong number of arguments when defining the objectives")

    if(total_n_objectives > 6)
      stop("Maximum number of objectives reached. Maximum number allowd is 6")


    objectives_df_final<-data.frame()
    for (k in 1:total_n_objectives){

      position_objectives <- k
      my_objectives<-objectives[position_objectives]
      #my_operator<-objectives[position_objectives+2]
      #my_value_objectives <- objectives[position_objectives+3]

      #objectives_df <- data.frame(cbind(my_objectives,my_operator,my_value_objectives))
      objectives_df_final <- rbind(objectives_df_final,my_objectives)
    }

    colnames(objectives_df_final)<-c("objectives_list")



    #plot the objectivess

    for(i in 1:total_n_objectives){
      objectives_diss <- my_shp[,paste(objectives)[i]]

      colnames(objectives_diss)<-c("objectives_field","geometry")


      objectives_diss <- st_transform(objectives_diss, crs = 4326)


      #suppressWarnings(assign(paste("objectives_plot_",i,sep=""),objectives_plot,pos = 1)) #,pos = 1
      suppressWarnings(assign(paste("objectives_diss_",i,sep=""),objectives_diss,pos = 1)) #,pos = 1
      #rm(objectives_plot)
    }




    #count how many objectivess we have in the environment. Then set the number of cols and rows for ggarrange





    list_objectives_diss_used <- (ls(envir = .GlobalEnv)[grep("objectives_diss_", ls(envir = .GlobalEnv))])
  }




#




  ###new plot test######

  total_area <- sum(my_shp[,paste(area)][[1]])

  all_groups_leaflet <- character()

  my_shp_diss <- sf::st_transform(my_shp_diss, crs = 4326)

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
    sf::st_cast()

  colnames(available_diss)<-c("available","m","geometry")


  #convert shapefile to WGS84
  #st_crs(available_diss)

  available_diss$available_cha <- "Yes"
  available_diss <- within(available_diss, available_cha[available == 0] <- 'No')

  available_diss <- sf::st_transform(available_diss, crs = 4326)
  my_shp_diss<- sf::st_transform(my_shp_diss, crs = 4326)

  available_diss<-subset(available_diss,available_cha=="Yes")

  # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
  # #palFunc <- (c("white","grey"))

  palFunc <- leaflet::colorFactor(
    palette = c("green"),
    domain = available_diss$available_cha
  )


  available_plot_leaflet<- available_plot_leaflet %>%
    leaflet::addPolygons(data = available_diss, fillColor = ~palFunc(available_cha), fillOpacity = 0.5,
                         color = 'black', opacity = 1, weight=0.5, label = ~available_cha ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'available')%>%
    leaflet::addLegend(data=available_diss, "topright",
                       #colors = c('white', 'grey'),
                       pal = palFunc,
                       values = ~available_cha ,
                       title = "Available",
                       labels = c("Yes"),
                       group = "available",
                       opacity = 0.5)

  all_groups_leaflet<-c(all_groups_leaflet,"available")


  available_area_sf <- subset(my_shp, get(available)==1)
  available_area<-sum(available_area_sf[,paste(area)][[1]])


  if(export_static_report==TRUE){

    available_plot <- available_diss  %>%
      #mutate_at(c('diss'), ~na_if(., 0)) %>%
      #st_combine() %>%
      ggplot() +
      geom_sf(aes(fill=factor(available)),color="black") +
      geom_sf(data=my_shp_diss,fill=NA,color="black") +
      #ggspatial::annotation_map_tile(zoom = 7) +
      #ggtitle("Projects ranking") +
      theme_void()+
      theme(plot.title=element_text(hjust=0.5))+
      #guides(fill="none")+
      labs(fill='Availability')+
      scale_fill_manual(values=c("lightgreen"),
                        labels=c('1'= 'Available'))


    suppressWarnings(assign("available_plot",available_plot,pos = 1))
  }

  }else{
    available_area<-total_area
  }

    #



  if(!missing(exclude_field)){
    exclude_diss <- my_shp[,paste(exclude_field)][1]

    exclude_diss <- exclude_diss %>%
      group_by(exclude_diss[,1][[1]]) %>%
      summarise(m = mean(exclude_diss[,1][[1]])) %>%
      sf::st_cast()

    colnames(exclude_diss)<-c("exclude","m","geometry")

    exclude_diss$exclude_cha <- "Yes"
    exclude_diss <- within(exclude_diss, exclude_cha[exclude == 0] <- 'No')


    exclude_diss<-subset(exclude_diss,exclude_cha=="No")

    palFunc_exclude <- leaflet::colorFactor(
      palette = c("green"),
      domain = exclude_diss$exclude_cha
    )




    exclude_diss<- sf::st_transform(exclude_diss, crs = 4326)


    available_plot_leaflet<- available_plot_leaflet %>%
      leaflet::addPolygons(data = exclude_diss, fillColor = ~palFunc_exclude(exclude_cha), fillOpacity = 0.5,
                         color = 'black', opacity = 1, weight=0.5, label = ~exclude_cha ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'exclude')%>%
      leaflet::addLegend(data=exclude_diss, "topright",
                         #colors = c('white', 'grey'),
                         pal = palFunc_exclude,
                         values = ~exclude_cha ,
                         title = "Exclude",
                         labels = c("No"),
                         group = "exclude",
                         opacity = 0.5)


    all_groups_leaflet<-c(all_groups_leaflet,"exclude")


    not_exclude_area_sf <- subset(my_shp, get(exclude_field)==0)
    not_exclude_area<-sum(not_exclude_area_sf[,paste(area)][[1]])


    if(export_static_report==TRUE){

      available_plot <- available_diss  %>%
        #mutate_at(c('diss'), ~na_if(., 0)) %>%
        #st_combine() %>%
        ggplot() +
        geom_sf(aes(fill=factor(available)),color="black") +
        geom_sf(data=my_shp_diss,fill=NA,color="black") +
        #ggspatial::annotation_map_tile(zoom = 7) +
        #ggtitle("Projects ranking") +
        theme_void()+
        theme(plot.title=element_text(hjust=0.5))+
        #guides(fill="none")+
        labs(fill='Availability')+
        scale_fill_manual(values=c("lightgreen"),
                          labels=c('1'= 'Available'))


      suppressWarnings(assign("available_plot",available_plot,pos = 1))




      exclude_plot <- exclude_diss  %>%
        #mutate_at(c('diss'), ~na_if(., 0)) %>%
        #st_combine() %>%
        ggplot() +
        geom_sf(aes(fill=factor(exclude)),color="black") +
        geom_sf(data=my_shp_diss,fill=NA,color="black") +
        #ggtitle("Projects ranking") +
        theme_void()+
        theme(plot.title=element_text(hjust=0.5))+
        #guides(fill="none")+
        labs(fill='Exclusion')+
        scale_fill_manual(values=c("lightgreen"),
                          labels=c('0'='Not exclude'))


      suppressWarnings(assign("exclude_plot",exclude_plot,pos = 1))


    }


  }else{
    not_exclude_area<-total_area
  }





  if(!missing(threshold)){

    if(nrow(threshold_df_final)==1){


      threshold_diss_1$threshold_original <- "Non-threshold-compliant"
      threshold_diss_1 <- within(threshold_diss_1, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[1],threshold_df_final$my_value_threshold[1],sep=" "))


      threshold_diss_1<-subset(threshold_diss_1,threshold==1)

      palFunc_threshold <- leaflet::colorFactor(
        palette = c("green"),
        domain = threshold_diss_1$threshold_original
      )



      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = threshold_diss_1, fillColor = ~palFunc_threshold(threshold_original), fillOpacity = 0.5,
                           color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'threshold_diss_1')%>%
        leaflet::addLegend(data=threshold_diss_1, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold,
                           values = ~threshold_diss_1$threshold_original ,
                           title = threshold_df_final$my_threshold[1],
                           labels = c("Yes"),
                           group = "threshold_diss_1",
                           opacity = 0.5)

      all_groups_leaflet<-c(all_groups_leaflet,"threshold_diss_1")


      }


    if(nrow(threshold_df_final)==2){


      threshold_diss_1$threshold_original <- "Non-threshold-compliant"
      threshold_diss_1 <- within(threshold_diss_1, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[1],threshold_df_final$my_value_threshold[1],sep=" "))


      threshold_diss_2$threshold_original <- "Non-threshold-compliant"
      threshold_diss_2 <- within(threshold_diss_2, threshold_original[threshold == 1] <- paste(threshold_df_final$my_operator[2],threshold_df_final$my_value_threshold[2],sep=" "))


      threshold_diss_1<-subset(threshold_diss_1,threshold==1)
      threshold_diss_2<-subset(threshold_diss_2,threshold==1)

      palFunc_threshold <- leaflet::colorFactor(
        palette = c("green"),
        domain = threshold_diss_1$threshold_original
      )



      palFunc_threshold2 <- leaflet::colorFactor(
        palette = c("green"),
        domain = threshold_diss_2$threshold_original
      )


      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = threshold_diss_1, fillColor = ~palFunc_threshold(threshold_original), fillOpacity = 0.5,
                                                   color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                                                   highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                                                   group = 'threshold_diss_1')%>%
        leaflet::addLegend(data=threshold_diss_1, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold,
                           values = ~threshold_diss_1$threshold_original ,
                           title = threshold_df_final$my_threshold[1],
                           labels = c("Yes"),
                           group = "threshold_diss_1",
                           opacity = 0.5)%>%


        leaflet::addPolygons(data = threshold_diss_2, fillColor = ~palFunc_threshold2(threshold_original), fillOpacity = 0.5,
                             color = 'black', opacity = 1, weight=0.5, label = ~threshold_original ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'threshold_diss_2')%>%
        leaflet::addLegend(data=threshold_diss_2, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_threshold2,
                           values = ~threshold_diss_2$threshold_original ,
                           title = threshold_df_final$my_threshold[2],
                           labels = c("Yes"),
                           group = "threshold_diss_2",
                           opacity = 0.5)


      all_groups_leaflet<-c(all_groups_leaflet,"threshold_diss_1","threshold_diss_2")


      }



    threshold_df_final$area_considered <- 0

    for(e in 1:nrow(threshold_df_final)){
      threshold_df_loop_table <- threshold_df_final[e,]


      if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
        theshold_command_use_loop <- suppressWarnings(c(noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]),sep=" ")), as.character(paste("'",threshold_df_final$my_value_threshold[e],"'",sep=""))))


      }else{
        theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))
        }



      threshold_loop_sf <- subset(my_shp, eval(parse(text=theshold_command_use_loop)))

      threshold_loop_area<-sum(threshold_loop_sf[,paste(area)][[1]])

      threshold_df_final$area_considered[e] <- threshold_loop_area


    }

}
  # }else{
  #   threshold_df_final$area_considered <- total_area
  # }






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





    #get data for pie chart


    if(!missing(objectives)){

    objectives_per_landuse_df_final <- data.frame(c(unique(my_shp[,paste(land_cover)][[1]])))
    colnames(objectives_per_landuse_df_final)<-c(land_cover)

    for(c in 1:length(objectives)){
    objectives_per_landuse_df <- data.frame(my_shp %>%
      group_by(eval(parse(text=land_cover)))%>%
      #summarise(total_obj_landuse = sum(noquote(paste(objectives[[c]]))))
      summarise(total_obj_landuse = sum(eval(parse(text=objectives[[c]])))))

    objectives_per_landuse_df <- objectives_per_landuse_df[,c(1:2)]
    colnames(objectives_per_landuse_df)<-c(land_cover,objectives[[c]])


    objectives_per_landuse_df_final <- dplyr::left_join(objectives_per_landuse_df_final,objectives_per_landuse_df,by=land_cover)
    }

    }


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



    if(!missing(objectives)){

      objectives_per_landownership_df_final <- data.frame(c(unique(my_shp[,paste(land_ownership)][[1]])))
      colnames(objectives_per_landownership_df_final)<-c(land_ownership)

      for(c in 1:length(objectives)){
        objectives_per_landownership_df <- data.frame(my_shp %>%
                                                  group_by(eval(parse(text=land_ownership)))%>%
                                                  #summarise(total_obj_landownership = sum(noquote(paste(objectives[[c]]))))
                                                  summarise(total_obj_landownership = sum(eval(parse(text=objectives[[c]])))))

        objectives_per_landownership_df <- objectives_per_landownership_df[,c(1:2)]
        colnames(objectives_per_landownership_df)<-c(land_ownership,objectives[[c]])


        objectives_per_landownership_df_final <- dplyr::left_join(objectives_per_landownership_df_final,objectives_per_landownership_df,by=land_ownership)
      }

    }


  }




  if(!missing(objectives)){

    obj_total_landscape_final<-data.frame()

    for(b in 1:length(objectives)){
      obj1_total_landscape <- my_shp[,c(area,paste(objectives)[b])]
      obj1_total_landscape <- sum(obj1_total_landscape[,paste(objectives[[b]])][[1]])
      obj1_total_landscape <- c(obj1_total_landscape,b)
      obj_total_landscape_final <-rbind(obj_total_landscape_final,obj1_total_landscape)
    }

    colnames(obj_total_landscape_final)<-c("objective_value","objective_order")
    obj_total_landscape_final$threshold_order <- 0


    #threshold will require more conditionals or a loop. Maybe a loop

      get_objectives_df_avai_excl <- data.frame()
      #objectives_diss_1$threshold_original <- "Non-threshold-compliant"

      obj1_after_threshold_final <-numeric()
      get_objectives_df_final <- data.frame()

      for(a in 1:length(objectives)){

      if(!missing(available)){
        obj1_after_available <- available_area_sf[,c(area,paste(objectives)[a])]
        obj1_after_available <- sum(obj1_after_available[,paste(objectives)[[a]]][[1]])
      }else{
        obj1_after_available <- obj1_total_landscape
      }

      if(!missing(exclude_field)){
        obj1_after_exclude <- not_exclude_area_sf[,c(area,paste(objectives)[a])]
        obj1_after_exclude <- sum(obj1_after_exclude[,paste(objectives)[[a]]][[1]])
      }else{
        obj1_after_exclude <- obj1_total_landscape
      }



        get_objectives_df_avai_excl <- rbind(get_objectives_df_avai_excl,c(obj1_after_available,obj1_after_exclude,a))
        colnames(get_objectives_df_avai_excl)<-c("after_only_available","after_only_not_excluded","objective_order")


      if(!missing(threshold)){



        for(r in 1:nrow(threshold_df_final)){

          threshold_df_loop_table <- threshold_df_final[r,]



          if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
            theshold_command_use_loop <- suppressWarnings(c(noquote(paste(threshold_df_final$my_threshold[r], noquote(threshold_df_final$my_operator[r]),sep=" ")), as.character(paste("'",threshold_df_final$my_value_threshold[r],"'",sep=""))))


          }else{
            theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[r], noquote(threshold_df_final$my_operator[r]), as.numeric(threshold_df_final$my_value_threshold[r]),sep=" "))
          }


          threshold_loop_sf <- subset(my_shp, eval(parse(text=theshold_command_use_loop)))

          obj1_after_threshold <- threshold_loop_sf[,c(area,paste(objectives)[a])]
          obj1_after_threshold<-sum(obj1_after_threshold[,paste(objectives[[a]])][[1]])

          objective_with_threshold <- c(obj1_after_threshold,a,r)

          #obj1_after_threshold_final <- data.frame(rbind(obj1_after_threshold_final,obj1_after_threshold))
          #obj1_after_threshold_final<-(cbind(obj1_after_threshold_final,a))

          get_objectives_df_final <- rbind(get_objectives_df_final,objective_with_threshold)

        }
      }else{
      get_objectives_df_final <- obj_total_landscape_final
    }
}
    colnames(get_objectives_df_final)<-c("objective_value","objective_order","threshold_order")





    if(nrow(objectives_df_final)==1){


      palFunc_objective_1 <- leaflet::colorNumeric(viridis::turbo(nrow(objectives_diss_1)),
                                                   objectives_diss_1$objectives_field)


      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = objectives_diss_1, fillColor = ~palFunc_objective_1(objectives_field), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'objectives_diss_1')%>%
        leaflet::addLegend(data=objectives_diss_1, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_objective_1,
                           values = ~objectives_diss_1$objectives_field ,
                           title = objectives_df_final$objectives_list[1],
                           labels = c("No", "Yes"),
                           group = "objectives_diss_1",
                           opacity = 1)

      all_groups_leaflet<-c(all_groups_leaflet,"objectives_diss_1")


    }



    if(nrow(objectives_df_final)==2){


      #objectives_diss_1$threshold_original <- "Non-threshold-compliant"


      palFunc_objective_1 <- leaflet::colorNumeric(viridis::turbo(nrow(objectives_diss_1)),
                                                  objectives_diss_1$objectives_field)


      palFunc_objective_2 <- leaflet::colorNumeric(viridis::turbo(nrow(objectives_diss_2)),
                                                  objectives_diss_2$objectives_field)



      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addPolygons(data = objectives_diss_1, fillColor = ~palFunc_objective_1(objectives_field), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'objectives_diss_1')%>%
        leaflet::addLegend(data=objectives_diss_1, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_objective_1,
                           values = ~objectives_diss_1$objectives_field ,
                           title = objectives_df_final$objectives_list[1],
                           labels = c("No", "Yes"),
                           group = "objectives_diss_1",
                           opacity = 1)%>%


        leaflet::addPolygons(data = objectives_diss_2, fillColor = ~palFunc_objective_2(objectives_field), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'objectives_diss_2')%>%
        leaflet::addLegend(data=objectives_diss_2, "topright",
                           #colors = c('white', 'grey'),
                           pal = palFunc_objective_2,
                           values = ~objectives_diss_2$objectives_field ,
                           title = objectives_df_final$objectives_list[2],
                           labels = c("No", "Yes"),
                           group = "objectives_diss_2",
                           opacity = 1)


      all_groups_leaflet<-c(all_groups_leaflet,"objectives_diss_1","objectives_diss_2")



    }



    objectives_df_final$objective_value <- 0

    for(e in 1:nrow(objectives_df_final)){
      objectives_df_loop_table_name <- objectives_df_final$objectives_list[e]


      objectives_df_loop_table <- my_shp

      objectives_loop_area<-sum(objectives_df_loop_table[,paste(objectives_df_loop_table_name)][[1]])

      objectives_df_final$objective_value[e] <- objectives_loop_area


    }



  }







  available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addLayersControl(overlayGroups = all_groups_leaflet,
                                  options = leaflet::layersControlOptions(collapsed = F)) %>%

        leaflet::hideGroup(all_groups_leaflet)%>%
    leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)




  suppressWarnings(assign("available_plot_leaflet",available_plot_leaflet,pos = 1))





  ###tables#####



  #get the combination of all

  combination_area_sf <- my_shp

  if(!missing(objectives)){
  if(!missing(available)){
    combination_area_sf <- subset(combination_area_sf, get(available)==1)
  }

  if(!missing(exclude_field)){
    combination_area_sf <- subset(combination_area_sf, get(exclude_field)==0)
  }


    #threshold part
    if(!missing(threshold)){
      for(e in 1:nrow(threshold_df_final)){
        threshold_df_loop_table <- threshold_df_final[e,]


        if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
          theshold_command_use_loop <- suppressWarnings(c(noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]),sep=" ")), as.character(paste("'",threshold_df_final$my_value_threshold[e],"'",sep=""))))


        }else{
          theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))
        }


        combination_area_sf <- subset(combination_area_sf, eval(parse(text=theshold_command_use_loop)))

      }
    }else{
      threshold_df_final<-data.frame()
    }



  combination_area <- sum(combination_area_sf[,paste(area)][[1]])

  combination_objective_final <- numeric()
  for(z in 1:nrow(objectives_df_final)){


    combination_objective <- sum(combination_area_sf[,paste(objectives_df_final$objectives_list[z])][[1]])

    combination_objective_final <- c(combination_objective_final,combination_objective)
  }









  #export final figure with only considered stands

  combination_area_sf$dissp <- 1

  combination_area_sf_plot <- combination_area_sf %>%
    group_by(dissp) %>%
    summarise(m = mean(dissp)) %>%
    st_cast()


  combination_area_sf_plot$considered <- "Yes"


  combination_area_sf_plot <- st_transform(combination_area_sf_plot, crs = 4326)


  # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
  # #palFunc <- (c("white","grey"))

  palFunc_considered_final <- leaflet::colorFactor(
    palette = c('green'),
    domain = combination_area_sf_plot$considered
  )

  combination_area_sf_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

    leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                         color = 'black', opacity=1,  weight=1, label=NA) %>%

    leaflet::addPolygons(data = combination_area_sf_plot, fillColor = ~palFunc_considered_final(considered), fillOpacity = 0.75,
                         color = 'black', opacity = 1, weight=0.5, label = ~considered ,stroke = TRUE,
                         highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                         group = 'considered')%>%
    leaflet::addLegend(data=combination_area_sf_plot, "topright",
                       #colors = c('darkolivegreen3'),
                       pal = palFunc_considered_final,
                       values = ~considered ,
                       title = "Considered for ForSys run",
                       labels = c("Yes"),
                       group = "considered",
                       opacity = 1)


  suppressWarnings(assign("combination_area_sf_plot_leaflet",combination_area_sf_plot_leaflet,pos = 1))



  #create a table with the area
  final_area_table_report <- data.frame()




  if(nrow(threshold_df_final)>0){
    area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,threshold_df_final$area_considered,combination_area))
    colnames(area_considered_table) <- "Area_considered"
    row.names(area_considered_table) <- c("Stands total","Only available","Only not excluded",paste("Only ",theshold_command_all_legend,sep=""),"Combination of criteria")

  }else{
    area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,combination_area))
    colnames(area_considered_table) <- "Area_considered"
    row.names(area_considered_table) <- c("Stands total","Only available","Only not excluded","Combination of criteria")
  }




  name_obj_temp_final<-c("Area_considered")

  for(v in 1:length(combination_objective_final)){
    area_considered_table$objtemp<-0

    area_considered_table$objtemp[nrow(area_considered_table)] <- combination_objective_final[v]
    name_obj_temp <- paste(objectives_df_final$objectives_list[v])
    name_obj_temp_final <- c(name_obj_temp_final,name_obj_temp)

    colnames(area_considered_table)<-name_obj_temp_final
  }


  for(h in 1:length(objectives)){
    #get_objectives_df_avai_excl
    area_considered_table["Stands total",(h+1)] <- obj_total_landscape_final$objective_value[[h]]

    if(!missing(available)){
      area_considered_table["Only available",(h+1)] <- get_objectives_df_avai_excl$after_only_available[[h]]
    }

    if(!missing(exclude_field)){
      area_considered_table["Only not excluded",(h+1)] <- get_objectives_df_avai_excl$after_only_not_excluded[[h]]
    }

    if(!missing(threshold)){

      get_objectives_df_final_loop_h <- subset(get_objectives_df_final,objective_order ==h)

      for(w in 1:max(get_objectives_df_final$threshold_order)){
        get_objectives_df_final_loop <- subset(get_objectives_df_final_loop_h,threshold_order == w)

        area_considered_table[paste("Only", theshold_command_all_legend)[[w]],(h+1)] <- get_objectives_df_final_loop$objective_value

      }

    }



  }

  area_considered_table_raw <- area_considered_table

  #get the percentage
  for(m in 1:ncol(area_considered_table)){
    perc_loop <- round(area_considered_table[,m]/area_considered_table[1,m]*100,0)
    val_and_perc_loop <- paste(area_considered_table[,m]," (",perc_loop,")",sep="")

    area_considered_table[,m] <-val_and_perc_loop

  }

  suppressWarnings(assign("area_considered_table",area_considered_table,pos = 1))




  #pie chart for landuse and landownership

  if(!missing(land_ownership)){


    objectives_per_landownership_selectable_df_final <- data.frame(c(unique(combination_area_sf[,paste(land_ownership)][[1]])))
    colnames(objectives_per_landownership_selectable_df_final)<-c(land_ownership)

    for(c in 1:length(objectives)){
      objectives_per_landownership_selectable_df <- data.frame(combination_area_sf %>%
                                                                 group_by(eval(parse(text=land_ownership)))%>%
                                                                 #summarise(total_obj_landownership = sum(noquote(paste(objectives[[c]]))))
                                                                 summarise(total_obj_landownership = sum(eval(parse(text=objectives[[c]])))))

      objectives_per_landownership_selectable_df <- objectives_per_landownership_selectable_df[,c(1:2)]
      colnames(objectives_per_landownership_selectable_df)<-c(land_ownership,paste("considered_",objectives[[c]],sep=""))


      objectives_per_landownership_selectable_df_final <- dplyr::left_join(objectives_per_landownership_selectable_df_final,objectives_per_landownership_selectable_df,by=land_ownership)
    }

    objectives_per_landownership_df_final<-dplyr::left_join(objectives_per_landownership_df_final,objectives_per_landownership_selectable_df_final,by=land_ownership)


    #get as percentage
    for(c in 1:length(objectives)){
    objectives_per_landownership_df_final[is.na(objectives_per_landownership_df_final)] <- 0

    objectives_per_landownership_df_final[[paste0("perc_",objectives)[c]]] <- objectives_per_landownership_df_final[,paste(objectives)[c]]/area_considered_table_raw[,paste(objectives)[c]][1]*100

    objectives_per_landownership_df_final[[paste0("perc_considered_",objectives)[c]]] <- objectives_per_landownership_df_final[,paste0("considered_",objectives)[c]]/area_considered_table_raw[,paste(objectives)[c]][1]*100
}



    #plots





    g <- lapply(1:length(objectives), function(i) {

      ggplot(objectives_per_landownership_df_final,
             aes_string(x=1, paste("perc_",objectives[i],sep=""),fill=land_ownership)) +
        geom_bar(width = 1, stat = "identity") + #,color="black"
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(name="Land ownership", palette="Paired") +
        ggtitle(paste(objectives[i],sep=" "))+
        theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))
      #suppressWarnings(assign(paste("landownership_landscape_plot_",i,sep=""),g[i],pos = 1))
    })




    landownership_landscape_ggarrange_plot <- ggpubr::ggarrange(plotlist=g,
                                                          ncol = 1,nrow=length(objectives),common.legend = TRUE,legend="bottom")

    suppressWarnings(assign("landownership_landscape_ggarrange_plot",landownership_landscape_ggarrange_plot,pos = 1))





    #only considered

    #plots


    g <- lapply(1:length(objectives), function(i) {

      ggplot(objectives_per_landownership_df_final,
             aes_string(x=1, paste("perc_considered_",objectives[i],sep=""),fill=land_ownership)) +
        geom_bar(width = 1, stat = "identity") + #,color="black"
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(name="Land ownership", palette="Paired") +
        ggtitle(paste(objectives[i],sep=" "))+
        theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))
      #suppressWarnings(assign(paste("landownership_considered_plot_",i,sep=""),g[i],pos = 1))
    })





    landownership_considered_ggarrange_plot <- ggpubr::ggarrange(plotlist=g,
                                                                ncol = 1,nrow=length(objectives),common.legend = TRUE,legend="bottom")

    suppressWarnings(assign("landownership_considered_ggarrange_plot",landownership_considered_ggarrange_plot,pos = 1))




  }




  if(!missing(land_cover)){

    objectives_per_landuse_selectable_df_final <- data.frame(c(unique(combination_area_sf[,paste(land_cover)][[1]])))
    colnames(objectives_per_landuse_selectable_df_final)<-c(land_cover)

    for(c in 1:length(objectives)){
      objectives_per_landuse_selectable_df <- data.frame(combination_area_sf %>%
                                                           group_by(eval(parse(text=land_cover)))%>%
                                                           #summarise(total_obj_landuse = sum(noquote(paste(objectives[[c]]))))
                                                           summarise(total_obj_landuse = sum(eval(parse(text=objectives[[c]])))))

      objectives_per_landuse_selectable_df <- objectives_per_landuse_selectable_df[,c(1:2)]
      colnames(objectives_per_landuse_selectable_df)<-c(land_cover,paste("considered_",objectives[[c]],sep=""))


      objectives_per_landuse_selectable_df_final <- dplyr::left_join(objectives_per_landuse_selectable_df_final,objectives_per_landuse_selectable_df,by=land_cover)
    }

    objectives_per_landuse_df_final<-dplyr::left_join(objectives_per_landuse_df_final,objectives_per_landuse_selectable_df_final,by=land_cover)


    #get as percentage
    for(c in 1:length(objectives)){
    objectives_per_landuse_df_final[is.na(objectives_per_landuse_df_final)] <- 0

    objectives_per_landuse_df_final[[paste0("perc_",objectives)[c]]] <- objectives_per_landuse_df_final[,paste(objectives)[c]]/area_considered_table_raw[,paste(objectives)[c]][1]*100

    objectives_per_landuse_df_final[[paste0("perc_considered_",objectives)[c]]] <- objectives_per_landuse_df_final[,paste0("considered_",objectives)[c]]/area_considered_table_raw[,paste(objectives)[c]][1]*100
}



    #do the pie chart - land use
    g <- lapply(1:length(objectives), function(i) {

      ggplot(objectives_per_landuse_df_final,
             aes_string(x=1, paste("perc_",objectives[i],sep=""),fill=land_cover)) +
        geom_bar(width = 1, stat = "identity") + #,color="black"
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(name="Land use", palette="Paired") +
        ggtitle(paste(objectives[i],sep=" "))+
        theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))
      #suppressWarnings(assign(paste("land_use_considered_plot_",i,sep=""),g[i],pos = 1))
    })





    landuse_landscape_ggarrange_plot <- ggpubr::ggarrange(plotlist=g,
                                                           ncol = 1,nrow=length(objectives),common.legend = TRUE,legend="bottom")

    suppressWarnings(assign("landuse_landscape_ggarrange_plot",landuse_landscape_ggarrange_plot,pos = 1))




    #only considered


    g <- lapply(1:length(objectives), function(i) {

      ggplot(objectives_per_landuse_df_final,
             aes_string(x=1, paste("perc_considered_",objectives[i],sep=""),fill=land_cover)) +
        geom_bar(width = 1, stat = "identity") + #,color="black"
        coord_polar("y") +
        theme_void() +
        scale_fill_brewer(name="Land use", palette="Paired") +
        ggtitle(paste(objectives[i],sep=" "))+
        theme(axis.text.x=element_blank(),plot.title = element_text(hjust = 0.5))
      #suppressWarnings(assign(paste("land_use_considered_plot_",i,sep=""),g[i],pos = 1))
    })





    landuse_considered_ggarrange_plot <- ggpubr::ggarrange(plotlist=g,
                                                                 ncol = 1,nrow=length(objectives),common.legend = TRUE,legend="bottom")

    suppressWarnings(assign("landuse_considered_ggarrange_plot",landuse_considered_ggarrange_plot,pos = 1))



  }








  }






  if(missing(objectives)){
    if(!missing(available)){
      combination_area_sf <- subset(combination_area_sf, get(available)==1)
    }

    if(!missing(exclude_field)){
      combination_area_sf <- subset(combination_area_sf, get(exclude_field)==0)
    }


    #threshold part
    if(!missing(threshold)){
      for(e in 1:nrow(threshold_df_final)){
        threshold_df_loop_table <- threshold_df_final[e,]

        theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))

        combination_area_sf <- subset(combination_area_sf, eval(parse(text=theshold_command_use_loop)))

      }


      }else{
      threshold_df_final<-data.frame()
    }



    combination_area <- sum(combination_area_sf[,paste(area)][[1]])


    #export final figure with only considered stands

    combination_area_sf$dissp <- 1

    combination_area_sf_plot <- combination_area_sf %>%
      group_by(dissp) %>%
      summarise(m = mean(dissp)) %>%
      st_cast()


    combination_area_sf_plot$considered <- "Yes"


    combination_area_sf_plot <- st_transform(combination_area_sf_plot, crs = 4326)


    # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
    # #palFunc <- (c("white","grey"))

    palFunc_considered_final <- leaflet::colorFactor(
      palette = c('green'),
      domain = combination_area_sf_plot$considered
    )

    combination_area_sf_plot_leaflet <- leaflet::leaflet() %>%
      leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

      leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                           color = 'black', opacity=1,  weight=1, label=NA) %>%

      leaflet::addPolygons(data = combination_area_sf_plot, fillColor = ~palFunc_considered_final(considered), fillOpacity = 0.75,
                           color = 'black', opacity = 1, weight=0.5, label = ~considered ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'considered')%>%
      leaflet::addLegend(data=combination_area_sf_plot, "topright",
                         #colors = c('darkolivegreen3'),
                         pal = palFunc_considered_final,
                         values = ~considered ,
                         title = "Considered for ForSys run",
                         labels = c("Yes"),
                         group = "considered",
                         opacity = 1)


    suppressWarnings(assign("combination_area_sf_plot_leaflet",combination_area_sf_plot_leaflet,pos = 1))




    #create a table with the area
    final_area_table_report <- data.frame()

    if(nrow(threshold_df_final)>0){
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,threshold_df_final$area_considered,combination_area))
      colnames(area_considered_table) <- "Area_considered"
      row.names(area_considered_table) <- c("Stands total","Only available","Only not excluded",paste("Only ",theshold_command_all_legend,sep=""),"Combination of criteria")

    }else{
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,combination_area))
      colnames(area_considered_table) <- "Area_considered"
      row.names(area_considered_table) <- c("Stands total","Only available","Only not excluded","Combination of criteria")
    }



    for(m in 1:ncol(area_considered_table)){
      perc_loop <- round(area_considered_table[,m]/area_considered_table[1,m]*100,0)
      val_and_perc_loop <- paste(area_considered_table[,m]," (",perc_loop,")",sep="")

      area_considered_table[,m] <-val_and_perc_loop

    }



    suppressWarnings(assign("area_considered_table",area_considered_table,pos = 1))
  }










  #then plot the combination of what we can treat after the availability and the combination of all thresholds

  path_for_rmd <- system.file("rmd_template", package = "ForSysXR")

  #check export html document
  save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

  capture.output(suppressWarnings(suppressMessages(generate_report_explore(output_file=paste("explore_",report_name,".html",sep="")))))

  if(export_static_report==TRUE){
    capture.output(suppressWarnings(suppressMessages(generate_report_explore_static(output_file=paste("explore_report_",report_name,".html",sep="")))))

  }

  file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

}
