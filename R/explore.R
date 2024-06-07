#' Explore defined data and ForSysX run before the run
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param area_field Field from input_shapefile containing the area of each stands
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param available_field Field from input_shapefile containing the available stands
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param land_cover Field from input_shapefile identifying the land cover in each stand
#' @param land_ownership Field from input_shapefile identifying the land ownership in each stand
#' @param objectives Vector containing the objective(s) field from input_shapefile
#' @param threshold Vector containing the threshold(s) field, the symbol of inequality or equality (">","<","==",">=","<="), and the threshold value (or minimum, maximum and step to be used). The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param threshold_logic Vector with two elements containing the threshold logic. The first element refers to if a single value should be used for the threshold ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple thresholds must be combined, either selecting stands where all thresholds are met ("and") or select stands where any of the thresholds are met ("or"). Default is c("single_value","and")
#' @param subunit_field Field from input_shapefile identifying the pre-defined planning areas
#' @param burn_probability Field from input_shapefile containing the average burn probability
#' @param flame_length Vector containing the field from input_shapefile with the expected flame length and the units (meters or feet)
#' @param report_name Name to be used in the html report file
#' @param export_static_report Logical. If TRUE, then an additional HTML report will be saved with static figures. Default is FALSE
#' @param web_upload Logical. If TRUE, the HTML file will be automatically uploaded to a default website, allowing for an easy share between users. Default is FALSE.
#' @return
#' @export
#' @examples
explore <- function(input_shapefile,
                    report_name,
                    stand_id,
                    area_field,
                    #available_for_management, #default is to not have stands with availability info
                    available_field,
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
                    burn_probability,
                    flame_length,
                    export_static_report = FALSE,
                    subunit_field,
                    web_upload=FALSE
                    #master_subunit
) {


if(missing(area_field)){
  stop("Missing area_field with no default")
}


  if(class(input_shapefile)[1]=="character"){
    input_shapefile_format <- substrRight(input_shapefile,4)
    if(input_shapefile_format!= ".shp")
      stop("input_shapefile has to be a shapefile!")}



  if(class(input_shapefile)[1]=="sf"){
    my_shp <- (input_shapefile)

    cat("Loading shapefile",'\n')
    my_shp <- rmapshaper::ms_simplify(my_shp, keep = 0.05,
                                    keep_shapes = TRUE)

    #my_shp <- sf::st_make_valid(my_shp)
    #if(max(nchar(names(my_shp)))>10){
    #  stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
    #}
  }




  if(class(input_shapefile)[1]=="character"){
    cat("Loading shapefile",'\n')
    my_shp <- sf::st_read(input_shapefile,quiet=TRUE)

    #if(max(nchar(names(my_shp)))>10){
    #  stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
    #}

    my_shp <- rmapshaper::ms_simplify(my_shp, keep = 0.05,
                                      keep_shapes = TRUE)

    #my_shp <- st_make_valid(my_shp)
  }


  valids<-geos::geos_is_valid(geos::as_geos_geometry(my_shp))
  #unique(valids)

  if(any(valids == FALSE)){
    cat("Problems with geometry. Trying to fix",'\n')
    my_shp <- sf::st_make_valid(my_shp)

    #valids<-sf::st_is_valid(my_shp)
    valids<-geos::geos_is_valid(geos::as_geos_geometry(my_shp))
    unique(valids)
    if(any(valids == FALSE)){
      cat("Unable to fix geomtry",'\n')
    }else{
        cat("Geomtry fixed",'\n')
      }
  }


  cat("Analysing data and preparing maps",'\n')


  suppressMessages(suppressWarnings(sf::sf_use_s2(FALSE)))

  my_shp <- sf::st_transform(my_shp, crs = 4326)
  my_shp_df <- sf::st_drop_geometry(my_shp)


  #study area contour
  my_shp$dissp <- 1


  my_shp<-suppressMessages(suppressWarnings(sf::st_buffer(my_shp,dist=0)))

  my_shp2 <- geos::as_geos_geometry(my_shp)

  my_shp_diss <- suppressMessages(suppressWarnings(my_shp2 %>%
                      #geos::geos_buffer(distance = 0)%>%
                      geos::geos_make_collection() %>%
                      geos::geos_unary_union()))

  my_shp_diss <- sf::st_as_sf(my_shp_diss)

  my_shp_diss<-suppressWarnings(my_shp_diss%>%
                                  sf::st_cast("MULTIPOLYGON") %>%
                                  sf::st_cast("POLYGON"))


  #maybe also linestring because this is only to plot the border#
  my_shp_diss_line<-suppressWarnings(my_shp_diss%>%
                                       sf::st_cast("MULTIPOLYGON") %>%
                                       sf::st_cast("POLYGON") %>%
                                       sf::st_cast("LINESTRING"))

  my_shp_use_gl<-sf::st_cast(my_shp,"POLYGON")


  if(!missing(subunit_field)){
    my_shp_subunit_diss_final_poly <- suppressWarnings(suppressMessages(my_shp %>%
                                      group_by(get(subunit_field)) %>%
                                      summarise(m = 0) %>%
                                        sf::st_cast()))

    my_shp_subunit_diss_final_poly_gl <- my_shp_subunit_diss_final_poly%>%
      sf::st_cast("MULTIPOLYGON")%>%
      #sf::st_cast("LINESTRING")%>%
      sf::st_cast("POLYGON") #%>%
  }









  # my_shp_diss_polygon<-suppressWarnings(my_shp_diss%>%
  #                                         sf::st_cast("MULTIPOLYGON") %>%
  #                                         sf::st_cast("POLYGON"))





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




      threshold_diss <- suppressMessages(suppressWarnings(threshold_diss %>%
        group_by(threshold_diss[,3][[1]]) %>%
        summarise(m = mean(threshold_diss[,3][[1]])) %>%
        #mutate(centrum = T) %>%
        sf::st_cast()))




      colnames(threshold_diss)<-c("threshold","m","geometry")

      #threshold_diss <- st_transform(threshold_diss, crs = 4326)






      # threshold_plot <- threshold_diss  %>%
      #   #mutate_at(c('diss'), ~na_if(., 0)) %>%
      #   #st_combine() %>%
      #   ggplot() +
      #   geom_sf(aes(fill=factor(threshold)),color="black") +
      #   #ggtitle("Projects ranking") +
      #   theme_void()+
      #   theme(plot.title=element_text(hjust=0.5))+
      #   #guides(fill="none")+
      #   labs(fill=threshold_df_final$my_threshold[1])+
      #   scale_fill_manual(values=c("white", "grey80"),
      #                     labels=c('0'='Not considered','1'= paste(threshold_df_final$my_threshold[i],theshold_command_legend_lable,sep=" ")))


      #suppressWarnings(assign(paste("threshold_plot_",i,sep=""),threshold_plot,pos = 1)) #,pos = 1
      suppressWarnings(assign(paste("threshold_diss_",i,sep=""),threshold_diss,pos = 1)) #,pos = 1
      #rm(threshold_plot)
    }




    #count how many thresholds we have in the environment. Then set the number of cols and rows for ggarrange


#
#
#     list_threshold_plots <- (ls(envir = .GlobalEnv)[grep("threshold_plot_", ls(envir = .GlobalEnv))])
#
#     my_list_final <- list()
#     for(k in 1:length(list_threshold_plots)){
#       my_list <- list(get(list_threshold_plots[[k]]))
#       my_list_final <- c(my_list_final,my_list)
#     }
#
#
#
#     threshold_final_figure <- ggpubr::ggarrange(plotlist=my_list_final,
#                                                 ncol = 1,nrow=length(list_threshold_plots),common.legend = FALSE)
#
#     suppressWarnings(assign("threshold_final_figure",threshold_final_figure,pos = 1))
#     suppressWarnings(assign("n_threshold_plots",length(list_threshold_plots),pos = 1))
#




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

    integer_val <- decimalplaces(total_n_objectives)

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


      #objectives_diss <- st_transform(objectives_diss, crs = 4326)


      #suppressWarnings(assign(paste("objectives_plot_",i,sep=""),objectives_plot,pos = 1)) #,pos = 1
      suppressWarnings(assign(paste("objectives_diss_",i,sep=""),objectives_diss,pos = 1)) #,pos = 1
      #rm(objectives_plot)
    }




    #count how many objectivess we have in the environment. Then set the number of cols and rows for ggarrange





    list_objectives_diss_used <- (ls(envir = .GlobalEnv)[grep("objectives_diss_", ls(envir = .GlobalEnv))])
  }




  #




  ###new plot test######

  total_area <- sum(my_shp_df[,paste(area_field)])



  if(!missing(flame_length)){
    if(flame_length[2]=="feet"){
      flame_length_unit <- ">= 8"
    }
    if(flame_length[2]=="meters"){
      flame_length_unit <- ">= 2.5"
    }

    flame_length_logical <- paste(flame_length[1],flame_length_unit,sep=" ")
    my_shp_above_FL_threshold <- subset(my_shp_df,eval(parse(text=flame_length_logical)))
    total_area_above_FL_threshold <- sum(my_shp_above_FL_threshold[,paste(area_field)])
    total_area_above_FL_threshold_perc <- total_area_above_FL_threshold/total_area*100

  }



  if(!missing(burn_probability)){
    total_expected_ba_year <- sum(my_shp_df[,paste(area_field)]*my_shp_df[,paste(burn_probability)])
  }




  all_groups_leaflet <- character()

  #my_shp_diss <- sf::st_transform(my_shp_diss, crs = 4326)


  available_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

    leafgl::addGlPolylines(data = my_shp_diss_line,
                         color = "black",
                         weight = 0.5)


    # leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
    #                      color = 'black', opacity=1,  weight=1, label=NA)
    #leafgl::addGlPolygons(data = my_shp, fillColor = NA, fillOpacity = 0,
    #                    color = 'black', opacity=1,  weight=1, label=NA)





  if(!missing(subunit_field)){


    #colourvalues::colour_palettes()

#
#
#     colnames(my_shp_subunit_diss_final_poly_gl)<-c("subunit","m","geometry")
#
#     #q4_palette <- colorspace::sequential_hcl(15, palette = "sunset")
#
#     cols=colourvalues::colour_values_rgb(my_shp_subunit_diss_final_poly_gl[,paste("subunit")][[1]],  palette = "diverge_hcl",include_alpha = FALSE)/ 255
#
#     palFunc_landcover <- leaflet::colorFactor("RdYlBu",
#                                               my_shp_subunit_diss_final_poly_gl$subunit)
#
#
#     #palFunc_landcover <- leaflet::colorFactor(terrain.colors(nrow(my_shp_subunit_diss_final_poly)), my_shp_subunit_diss_final_poly$subunit)
#
#


    colnames(my_shp_subunit_diss_final_poly)<-c("subunit","m","geometry")

    #q4_palette <- colorspace::sequential_hcl(15, palette = "sunset")

    cols=colourvalues::colour_values_rgb(my_shp_subunit_diss_final_poly[,paste("subunit")][[1]],  palette = "diverge_hcl",include_alpha = FALSE)/ 255

    palFunc_landcover <- leaflet::colorFactor("RdYlBu",
                                              my_shp_subunit_diss_final_poly$subunit)








    available_plot_leaflet <- leaflet::leaflet() %>%
    leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
    #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

    #leafgl::addGlPolylines(data = my_shp_subunit_diss_final_line,
    #                       color = "grey40",
    #                       weight = 0.5,
    #                       group = 'subunit')%>%

    leafgl::addGlPolylines(data = my_shp_diss_line,
                           color = "black",
                           weight = 0.5)%>%

      leaflet::addPolygons(data = my_shp_subunit_diss_final_poly,
                           fillColor = ~palFunc_landcover(subunit),
                           fillOpacity = 0.75,
                           color = 'black',
                           opacity = 1,
                           weight=0.5,
                           label = ~subunit ,stroke = TRUE,
                           highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                           group = 'subunit')%>%


#
#     leafgl::addGlPolygons(data = my_shp_subunit_diss_final_poly_gl,
#                             #cols_fill= cols,
#                             #cols_fill = NA,
#                             fillOpacity = 0.75,
#                             color = cols,
#                             opacity = 1,
#                             weight=0.5,
#                             #popup = "landuse",
#                             #label = ~landuse,
#                             weight = 0.1,
#                             #opacity=1,
#                             #weight=1,
#                             #label=NA,
#                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
#                             group = "subunit")%>%

  leaflet::addLegend(data=my_shp_subunit_diss_final_poly_gl, "topright",
                     #colors = rgb(cols),
                     pal = palFunc_landcover,
                     values = ~my_shp_subunit_diss_final_poly_gl$subunit ,
                     title = "Subunits",
                     #labels = c("No", "Yes"),
                     group = "subunit",
                     opacity = 1)

    # leaflet::addLegend(data=my_shp_subunit_diss_final_line, "topright",
    #                  colors = "grey40",
    #                  #pal = palFunc,
    #                  #values = ~available_cha ,
    #                  title = "Subunits",
    #                  labels = c("Yes"),
    #                  group = "subunit",
    #                  opacity = 0.5)
    #

  all_groups_leaflet<-c(all_groups_leaflet,"subunit")

  }

  ###############

  if(!missing(available_field)){
    available_diss <- my_shp[,paste(available_field)][1]

    available_area_sf <- subset(my_shp, get(available_field)==1)
    available_area<-sum(available_area_sf[,paste(area_field)][[1]])


    available_diss <- suppressMessages(suppressWarnings(available_area_sf %>%
      group_by(available_area_sf[,paste(available_field)][[1]]) %>%
      summarise(m = mean(available_area_sf[,paste(available_field)][[1]])) %>%
      sf::st_cast()))



    colnames(available_diss)<-c("available","m","geometry")


    #convert shapefile to WGS84
    #st_crs(available_diss)

    available_diss$available_cha <- "Yes"
    available_diss <- within(available_diss, available_cha[available == 0] <- 'No')

    #available_diss <- sf::st_transform(available_diss, crs = 4326)
    #my_shp_diss<- sf::st_transform(my_shp_diss, crs = 4326)

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





  }


  # if(export_static_report==TRUE){
  #
  #   available_plot <- available_diss  %>%
  #     #mutate_at(c('diss'), ~na_if(., 0)) %>%
  #     #st_combine() %>%
  #     ggplot() +
  #     geom_sf(aes(fill=factor(available)),color="black") +
  #     geom_sf(data=my_shp_diss,fill=NA,color="black") +
  #     #ggspatial::annotation_map_tile(zoom = 7) +
  #     #ggtitle("Projects ranking") +
  #     theme_void()+
  #     theme(plot.title=element_text(hjust=0.5))+
  #     #guides(fill="none")+
  #     labs(fill='Availability')+
  #     scale_fill_manual(values=c("lightgreen"),
  #                       labels=c('1'= 'Available'))
  #
  #
  #   suppressWarnings(assign("available_plot",available_plot,pos = 1))
  # }
  #
  # }else{
  #   available_area<-total_area
  # }

  #



  if(!missing(exclude_field)){
    exclude_diss <- my_shp[,paste(exclude_field)][1]

    exclude_diss <- suppressMessages(suppressWarnings(exclude_diss %>%
      group_by(exclude_diss[,1][[1]]) %>%
      summarise(m = mean(exclude_diss[,1][[1]])) %>%
      sf::st_cast()))

    colnames(exclude_diss)<-c("exclude","m","geometry")

    exclude_diss$exclude_cha <- "Yes"
    exclude_diss <- within(exclude_diss, exclude_cha[exclude == 0] <- 'No')


    exclude_diss<-subset(exclude_diss,exclude_cha=="No")

    palFunc_exclude <- leaflet::colorFactor(
      palette = c("green"),
      domain = exclude_diss$exclude_cha
    )




    #exclude_diss<- sf::st_transform(exclude_diss, crs = 4326)


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
    not_exclude_area<-sum(not_exclude_area_sf[,paste(area_field)][[1]])
  }

  #   if(export_static_report==TRUE){
  #
  #     available_plot <- available_diss  %>%
  #       #mutate_at(c('diss'), ~na_if(., 0)) %>%
  #       #st_combine() %>%
  #       ggplot() +
  #       geom_sf(aes(fill=factor(available)),color="black") +
  #       geom_sf(data=my_shp_diss,fill=NA,color="black") +
  #       #ggspatial::annotation_map_tile(zoom = 7) +
  #       #ggtitle("Projects ranking") +
  #       theme_void()+
  #       theme(plot.title=element_text(hjust=0.5))+
  #       #guides(fill="none")+
  #       labs(fill='Availability')+
  #       scale_fill_manual(values=c("lightgreen"),
  #                         labels=c('1'= 'Available'))
  #
  #
  #     suppressWarnings(assign("available_plot",available_plot,pos = 1))
  #
  #
  #
  #
  #     exclude_plot <- exclude_diss  %>%
  #       #mutate_at(c('diss'), ~na_if(., 0)) %>%
  #       #st_combine() %>%
  #       ggplot() +
  #       geom_sf(aes(fill=factor(exclude)),color="black") +
  #       geom_sf(data=my_shp_diss,fill=NA,color="black") +
  #       #ggtitle("Projects ranking") +
  #       theme_void()+
  #       theme(plot.title=element_text(hjust=0.5))+
  #       #guides(fill="none")+
  #       labs(fill='Exclusion')+
  #       scale_fill_manual(values=c("lightgreen"),
  #                         labels=c('0'='Not exclude'))
  #
  #
  #     suppressWarnings(assign("exclude_plot",exclude_plot,pos = 1))
  #
  #
  #   }
  #
  #
  # }else{
  #   not_exclude_area<-total_area
  # }





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

      threshold_loop_area<-sum(threshold_loop_sf[,paste(area_field)][[1]])

      threshold_df_final$area_considered[e] <- threshold_loop_area


    }

  }
  # }else{
  #   threshold_df_final$area_considered <- total_area
  # }






  if(!missing(land_cover)){
    land_cover_diss <- my_shp[,paste(land_cover)][1]

    land_cover_diss <- suppressMessages(suppressWarnings(land_cover_diss %>%
      group_by(land_cover_diss[,1][[1]]) %>%
      summarise(m = mode(land_cover_diss[,1][[1]])) %>%
      sf::st_cast()))

    colnames(land_cover_diss)<-c("land_cover_use","m","geometry")
    land_cover_diss$land_cover_use <- factor(land_cover_diss$land_cover_use)
    #land_cover_diss <- st_transform(land_cover_diss, crs = 4326)


    land_cover_diss$category <- 1:nrow(land_cover_diss)

    land_cover_diss$category <- factor(1:nrow(land_cover_diss))

    palFunc_landcover <- leaflet::colorFactor(terrain.colors(nrow(land_cover_diss)), land_cover_diss$land_cover_use)

#
#
#     available_plot_leaflet<-available_plot_leaflet %>%
#       leaflet::addPolygons(data = land_cover_diss, fillColor = ~palFunc_landcover(land_cover_use), fillOpacity = 0.75,
#                            color = 'black', opacity = 1, weight=0.5, label = ~land_cover_use ,stroke = TRUE,
#                            highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
#                            group = 'land_cover_diss')%>%
#       leaflet::addLegend(data=land_cover_diss, "topright",
#                          #colors = c('white', 'grey'),
#                          pal = palFunc_landcover,
#                          values = ~land_cover_diss$land_cover_use ,
#                          title = "Land cover",
#                          labels = c("No", "Yes"),
#                          group = "land_cover_diss",
#                          opacity = 1)
#
#     all_groups_leaflet<-c(all_groups_leaflet,"land_cover_diss")





    ###alternativa#####




    cols=colourvalues::colour_values_rgb(my_shp_use_gl[,paste(land_cover)][[1]],  palette = "terrain",include_alpha = FALSE)/ 255


    available_plot_leaflet<-available_plot_leaflet %>%
      leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      leafgl::addGlPolygons(data = my_shp_use_gl,
                            #cols_fill= cols,
                            #cols_fill = NA,
                            fillOpacity = 0.75,
                            color = cols,
                            opacity = 1,
                            weight=0.5,
                            popup = "landuse",
                            #label = ~landuse,
                            weight = 0.1,
                            #opacity=1,
                            #weight=1,
                            #label=NA,
                            highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                            group = 'Land cover') %>%
      leaflet::addLegend(data=my_shp_use_gl, "topright",
                         #colors = rgb(cols),
                         pal = palFunc_landcover,
                         values = ~land_cover_diss$land_cover_use ,
                         title = "Land cover",
                         #labels = c("No", "Yes"),
                         group = "Land cover",
                         opacity = 1)

    all_groups_leaflet<-c(all_groups_leaflet,"Land cover")



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

    land_ownership_diss <- suppressMessages(suppressWarnings(land_ownership_diss %>%
      group_by(land_ownership_diss[,1][[1]]) %>%
      summarise(m = mode(land_ownership_diss[,1][[1]])) %>%
      sf::st_cast()))

    colnames(land_ownership_diss)<-c("land_ownership_use","m","geometry")
    land_ownership_diss$land_ownership_use <- factor(land_ownership_diss$land_ownership_use)
    #land_ownership_diss <- st_transform(land_ownership_diss, crs = 4326)


    #land_ownership_diss$category <- 1:nrow(land_ownership_diss)

    land_ownership_diss$category <- factor(1:nrow(land_ownership_diss))

    #palFunc_land_ownership <- leaflet::colorFactor(topo.colors(nrow(land_ownership_diss)), land_ownership_diss$land_ownership_use)

    if(nrow(land_ownership_diss)==1){
      palFunc_land_ownership <- leaflet::colorFactor(colorspace::diverge_hcl(nrow(land_ownership_diss)+1), land_ownership_diss$land_ownership_use)
    }

    if(nrow(land_ownership_diss)>1){
      palFunc_land_ownership <- leaflet::colorFactor(colorspace::diverge_hcl(nrow(land_ownership_diss)), land_ownership_diss$land_ownership_use)
    }



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




        objectives_per_landownership_df <- data.frame(my_shp_df %>%
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
      obj1_total_landscape <- my_shp[,c(area_field,paste(objectives)[b])]
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

      if(!missing(available_field)){
        obj1_after_available <- available_area_sf[,c(area_field,paste(objectives)[a])]
        obj1_after_available <- sum(obj1_after_available[,paste(objectives)[[a]]][[1]])
      }else{
        obj1_after_available <- obj1_total_landscape
      }

      if(!missing(exclude_field)){
        obj1_after_exclude <- not_exclude_area_sf[,c(area_field,paste(objectives)[a])]
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

          obj1_after_threshold <- threshold_loop_sf[,c(area_field,paste(objectives)[a])]
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


      palFunc_objective_1 <- leaflet::colorNumeric(viridis::viridis(nrow(objectives_diss_1)),
                                                   objectives_diss_1$objectives_field)


      # available_plot_leaflet<-available_plot_leaflet %>%
      #   leaflet::addPolygons(data = objectives_diss_1, fillColor = ~palFunc_objective_1(objectives_field), fillOpacity = 0.75,
      #                        color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
      #                        highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
      #                        group = 'objectives_diss_1')%>%
      #   leaflet::addLegend(data=objectives_diss_1, "topright",
      #                      #colors = c('white', 'grey'),
      #                      pal = palFunc_objective_1,
      #                      values = ~objectives_diss_1$objectives_field ,
      #                      title = objectives_df_final$objectives_list[1],
      #                      labels = c("No", "Yes"),
      #                      group = "objectives_diss_1",
      #                      opacity = 1)
      #
      # all_groups_leaflet<-c(all_groups_leaflet,"objectives_diss_1")
      #




      #alternativa

      cols=colourvalues::colour_values_rgb(my_shp_use_gl[,paste(objectives[1])][[1]],  palette = "viridis",include_alpha = FALSE)/ 255


      available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        leafgl::addGlPolygons(data = my_shp_use_gl,
                              #cols_fill= cols,
                              #cols_fill = NA,
                              fillOpacity = 0.75,
                              color = cols,
                              opacity = 1,
                              weight=0.5,
                              popup = objectives[1],
                              #label = ~landuse,
                              weight = 0.1,
                              #opacity=1,
                              #weight=1,
                              #label=NA,
                              highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                              group = objectives[1]) %>%
        leaflet::addLegend(data=my_shp_use_gl, "topright",
                           #colors = rgb(cols),
                           pal = palFunc_objective_1,
                           values = ~objectives_diss_1$objectives_field ,
                           title = objectives[1],
                           #labels = c("No", "Yes"),
                           group = objectives[1],
                           opacity = 1)

      all_groups_leaflet<-c(all_groups_leaflet,objectives[1])


#
#
#       leaflet::leaflet() %>%
#         leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
#         leafgl::addGlPolygons(data = my_shp_use_gl,
#                               #cols_fill= cols,
#                               #cols_fill = NA,
#                               fillOpacity = 0.75,
#                               color = cols,
#                               opacity = 1,
#                               weight=0.5,
#                               popup = objectives[1],
#                               #label = ~landuse,
#                               weight = 0.1,
#                               #opacity=1,
#                               #weight=1,
#                               #label=NA,
#                               highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
#                               group = objectives[1]) %>%
#         leaflet::addLegend(data=my_shp_use_gl, "topright",
#                            #colors = rgb(cols),
#                            pal = palFunc_objective_1,
#                            values = ~objectives_diss_1$objectives_field ,
#                            title = objectives[1],
#                            #labels = c("No", "Yes"),
#                            group = objectives[1],
#                            opacity = 1)
#
#



    }



    if(nrow(objectives_df_final)==2){


      #objectives_diss_1$threshold_original <- "Non-threshold-compliant"

      palFunc_objective_1 <- leaflet::colorNumeric(viridis::cividis(nrow(my_shp_use_gl)),
                                                   my_shp_use_gl[,paste(objectives[1])][[1]])

      #palFunc_objective_1 <- leaflet::colorFactor(viridis::viridis(nrow(my_shp_use_gl)), my_shp_use_gl$obj_1)

      # palFunc_objective_1 <- leaflet::colorNumeric(viridis::turbo(nrow(objectives_diss_1)),
      #                                              objectives_diss_1$objectives_field)


      palFunc_objective_2 <- leaflet::colorNumeric(viridis::cividis(nrow(my_shp_use_gl)),
                                                   my_shp_use_gl[,paste(objectives[2])][[1]])


#
#       available_plot_leaflet<-available_plot_leaflet %>%
#         leaflet::addPolygons(data = objectives_diss_1, fillColor = ~palFunc_objective_1(objectives_field), fillOpacity = 0.75,
#                              color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
#                              highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
#                              group = 'objectives_diss_1')%>%
#         leaflet::addLegend(data=objectives_diss_1, "topright",
#                            #colors = c('white', 'grey'),
#                            pal = palFunc_objective_1,
#                            values = ~objectives_diss_1$objectives_field ,
#                            title = objectives_df_final$objectives_list[1],
#                            labels = c("No", "Yes"),
#                            group = "objectives_diss_1",
#                            opacity = 1)%>%
#


        #alternativa
#########
        cols_obj1=colourvalues::colour_values_rgb(my_shp_use_gl[,paste(objectives[1])][[1]],  palette = "cividis",include_alpha = FALSE)
        cols_obj2=colourvalues::colour_values_rgb(my_shp_use_gl[,paste(objectives[2])][[1]],  palette = "cividis",include_alpha = FALSE)

        # leaflet::colorNumeric(viridis::cividis(nrow(my_shp_use_gl)),
        #              my_shp_use_gl$obj_1)
        #

        #colourvalues::colour_palettes(colours = "viridis")

        #cols=colourvalues::colour_values_rgb(leaflet::colorNumeric(viridis::turbo(nrow(objectives_diss_1)),
        #                      objectives_diss_1$objectives_field))

        my_shp_use_gl_obj1 <- my_shp_use_gl
        my_shp_use_gl_obj2 <- my_shp_use_gl

        available_plot_leaflet<-available_plot_leaflet %>%
        leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        leafgl::addGlPolygons(data = my_shp_use_gl_obj1,
                              #cols_fill= cols,
                              #cols_fill = NA,
                              fillOpacity = 0.75,
                              color = cols_obj1,
                              opacity = 1,
                              weight=0.5,
                              popup = objectives[1],
                              #label = ~landuse,
                              weight = 0.1,
                              #opacity=1,
                              #weight=1,
                              #label=NA,
                              highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                              group = objectives[1]) %>%
        leaflet::addLegend(data=my_shp_use_gl_obj1, "topleft",
                           #colors = rgb(cols),
                           pal = palFunc_objective_1,
                           values = ~objectives_diss_1$objectives_field ,
                           title = objectives[1],
                           #labels = c("No", "Yes"),
                           group = objectives[1],
                           opacity = 1)%>%



        #leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        leafgl::addGlPolygons(data = my_shp_use_gl_obj2,
                              #cols_fill= cols,
                              #cols_fill = NA,
                              fillOpacity = 0.75,
                              color = cols_obj2,
                              opacity = 1,
                              weight=0.5,
                              popup = objectives[2],
                              #label = ~landuse,
                              weight = 0.1,
                              #opacity=1,
                              #weight=1,
                              #label=NA,
                              highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                              group = objectives[2]) %>%
        leaflet::addLegend(data=my_shp_use_gl_obj2, "topleft",
                           #colors = rgb(cols),
                           pal = palFunc_objective_2,
                           values = ~objectives_diss_2$objectives_field ,
                           title = objectives[2],
                           #labels = c("No", "Yes"),
                           group = objectives[2],
                           opacity = 1)


        # leaflet::addPolygons(data = objectives_diss_2, fillColor = ~palFunc_objective_2(objectives_field), fillOpacity = 0.75,
        #                      color = 'black', opacity = 1, weight=0.5, label = ~objectives_field ,stroke = TRUE,
        #                      highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
        #                      group = 'objectives_diss_2')%>%
        # leaflet::addLegend(data=objectives_diss_2, "topright",
        #                    #colors = c('white', 'grey'),
        #                    pal = palFunc_objective_2,
        #                    values = ~objectives_diss_2$objectives_field ,
        #                    title = objectives_df_final$objectives_list[2],
        #                    labels = c("No", "Yes"),
        #                    group = "objectives_diss_2",
        #                    opacity = 1)


      all_groups_leaflet<-c(all_groups_leaflet,objectives[1],objectives[2])

      #all_groups_leaflet<-c(all_groups_leaflet,objectives[1])


# #
#         available_plot_leaflet %>%
#           leaflet::addLayersControl(overlayGroups = all_groups_leaflet,
#                                     options = leaflet::layersControlOptions(collapsed = F)) %>%
#
#           leaflet::hideGroup(all_groups_leaflet)%>%
#           leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
#        #
#

#
#
#         leaflet::leaflet()%>%
#           leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
#           leafgl::addGlPolygons(data = my_shp_use_gl,
#                                 #cols_fill= cols,
#                                 #cols_fill = NA,
#                                 fillOpacity = 0.75,
#                                 color = cols,
#                                 opacity = 1,
#                                 weight=0.5,
#                                 popup = objectives[1],
#                                 #label = ~landuse,
#                                 weight = 0.1,
#                                 #opacity=1,
#                                 #weight=1,
#                                 #label=NA,
#                                 highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
#                                 group = objectives[1]) %>%
#           leaflet::addLegend(data=my_shp_use_gl, "topleft",
#                              #colors = rgb(cols),
#                              pal = palFunc_objective_1,
#                              values = ~objectives_diss_1$objectives_field ,
#                              title = objectives[1],
#                              #labels = c("No", "Yes"),
#                              group = objectives[1],
#                              opacity = 1) %>%
#           leaflet::addLayersControl(overlayGroups = all_groups_leaflet,
#                                     options = leaflet::layersControlOptions(collapsed = F)) %>%
#
#           leaflet::hideGroup(all_groups_leaflet)





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

  #combination_area_sf <- my_shp_df

###aqui#####

  combination_area_sf<-my_shp
  available_area <- sum(combination_area_sf[,paste(area_field)][[1]])

  if(!missing(objectives)){
    if(!missing(available_field)){
      combination_area_sf <- subset(my_shp, get(available_field)==1)
      after_available <- combination_area_sf
      available_area <- sum(combination_area_sf[,paste(area_field)][[1]])

      if(!missing(flame_length)){
        #flame_length_logical <- paste(flame_length[1]," >= 8",sep="")
        my_shp_above_FL_threshold_available <- subset(combination_area_sf,eval(parse(text=flame_length_logical)))
        total_above_FL_threshold_available <- sum(my_shp_above_FL_threshold_available[,paste(area_field)][[1]])
        total_above_FL_threshold_available_perc <- round(total_above_FL_threshold_available/total_area*100,1)
      }else{
        total_above_FL_threshold_available_perc<-NA
      }


      if(!missing(burn_probability)){
        total_expected_ba_year_available <- sum(combination_area_sf[,paste(area_field)][[1]]*combination_area_sf[,paste(burn_probability)][[1]])
      }else{
        total_expected_ba_year_available<-NA
      }


      get_objectives_df_avai <- data.frame()
      for(a in 1:length(objectives)){

        if(!missing(available_field)){
          obj1_after_available <- combination_area_sf[,c(area_field,paste(objectives)[a])]
          obj1_after_available <- sum(obj1_after_available[,paste(objectives)[[a]]][[1]])
        }else{
          obj1_after_available <- obj1_total_landscape
        }


        get_objectives_df_avai <- rbind(get_objectives_df_avai,c(obj1_after_available,a))
        colnames(get_objectives_df_avai)<-c("after_only_available","objective_order")

      }


      #plot after available
      #after_available_and_exclude_and_thresholds


      after_available$dissp <- 1

      after_available_sf_plot <- suppressMessages(suppressWarnings(after_available %>%
        group_by(dissp) %>%
        summarise(m = mean(dissp)) %>%
        sf::st_cast()))


      after_available_sf_plot$considered <- "Yes"

      #after_available_sf_plot <- st_transform(after_available_sf_plot, crs = 4326)

      palFunc_considered_final <- leaflet::colorFactor(
        palette = c('green'),
        domain = after_available_sf_plot$considered
      )

      after_available_sf_plot_leaflet <- leaflet::leaflet() %>%
        leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

        leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                             color = 'black', opacity=1,  weight=1, label=NA) %>%

        leaflet::addPolygons(data = after_available_sf_plot, fillColor = ~palFunc_considered_final(considered), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~considered ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'considered')%>%
        leaflet::addLegend(data=after_available_sf_plot, "topright",
                           #colors = c('darkolivegreen3'),
                           pal = palFunc_considered_final,
                           values = ~considered ,
                           title = "Withdrawals from treatments",
                           labels = c("Yes"),
                           group = "considered",
                           opacity = 1)%>%
        leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
                                  position = "topleft")


      suppressWarnings(assign("after_available_sf_plot_leaflet",after_available_sf_plot_leaflet,pos = 1))


    }

    if(!missing(exclude_field)){
      combination_area_sf <- subset(combination_area_sf, get(exclude_field)==0)
      after_available_and_exclude <- combination_area_sf
      not_exclude_area <- sum(combination_area_sf[,paste(area_field)][[1]])


      if(!missing(flame_length)){
        #flame_length_logical <- paste(flame_length[1]," >= 8",sep="")
        my_shp_above_FL_threshold_available_exclude <- subset(combination_area_sf,eval(parse(text=flame_length_logical)))
        total_above_FL_threshold_available_exclude <- sum(my_shp_above_FL_threshold_available_exclude[,paste(area_field)][[1]])
        total_above_FL_threshold_available_exclude_perc <- total_above_FL_threshold_available_exclude/total_area*100
      }else{
        total_above_FL_threshold_available_exclude_perc<-NA
      }

      if(!missing(burn_probability)){
        total_expected_ba_year_available_exclude <- sum(combination_area_sf[,paste(area_field)][[1]]*combination_area_sf[,paste(burn_probability)][[1]])
      }else{
        total_expected_ba_year_available_exclude<-NA
      }

      get_objectives_df_avai_excl <- data.frame()
      for(a in 1:length(objectives)){
        if(!missing(exclude_field)){
          obj1_after_exclude <- combination_area_sf[,c(area_field,paste(objectives)[a])]
          obj1_after_exclude <- sum(obj1_after_exclude[,paste(objectives)[[a]]][[1]])
        }else{
          obj1_after_exclude <- obj1_total_landscape
        }

        get_objectives_df_avai_excl <- rbind(get_objectives_df_avai_excl,c(obj1_after_exclude,a))
        colnames(get_objectives_df_avai_excl)<-c("after_only_exclude","objective_order")
      }


      #plot after available
      #after_available_and_exclude


      #not sure if I can comment this out
      # after_available_and_exclude$dissp <- 1
      #
      # after_available_and_exclude_sf_plot <- after_available_and_exclude %>%
      #   group_by(dissp) %>%
      #   summarise(m = mean(dissp)) %>%
      #   st_cast()
      #
      #
      # after_available_and_exclude_sf_plot$considered <- "Yes"
      #
      # #after_available_and_exclude_sf_plot <- st_transform(after_available_and_exclude_sf_plot, crs = 4326)
      #
      # palFunc_considered_final <- leaflet::colorFactor(
      #   palette = c('green'),
      #   domain = after_available_and_exclude_sf_plot$considered
      # )
      #
      # after_available_and_exclude_sf_plot_leaflet <- leaflet::leaflet() %>%
      #   leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #   leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #   #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      #   #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%
      #
      #   leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
      #                        color = 'black', opacity=1,  weight=1, label=NA) %>%
      #
      #   leaflet::addPolygons(data = after_available_and_exclude_sf_plot, fillColor = ~palFunc_considered_final(considered), fillOpacity = 0.75,
      #                        color = 'black', opacity = 1, weight=0.5, label = ~considered ,stroke = TRUE,
      #                        highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
      #                        group = 'considered')%>%
      #   leaflet::addLegend(data=after_available_and_exclude_sf_plot, "topright",
      #                      #colors = c('darkolivegreen3'),
      #                      pal = palFunc_considered_final,
      #                      values = ~considered ,
      #                      title = "Residual stands after removal from exclude field",
      #                      labels = c("Yes"),
      #                      group = "considered",
      #                      opacity = 1)%>%
      #   leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
      #                             position = "topleft")
      #
      #
      # suppressWarnings(assign("after_available_and_exclude_sf_plot_leaflet",after_available_and_exclude_sf_plot_leaflet,pos = 1))
      #
      #

    }


    #threshold part
    if(!missing(threshold)){

      total_above_FL_threshold_available_exclude_threshold_perc_df <- numeric()
      total_expected_ba_year_available_exclude_threshold_df<-numeric()
      get_objectives_df_avai_excl_threshold <- data.frame()
      for(e in 1:nrow(threshold_df_final)){
        threshold_df_loop_table <- threshold_df_final[e,]


        if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
          theshold_command_use_loop <- suppressWarnings(c(noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]),sep=" ")), as.character(paste("'",threshold_df_final$my_value_threshold[e],"'",sep=""))))


        }else{
          theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))
        }



        combination_area_sf <- subset(combination_area_sf, eval(parse(text=theshold_command_use_loop)))

        threshold_loop_area<-sum(combination_area_sf[,paste(area_field)][[1]])

        threshold_df_final$area_considered[e] <- threshold_loop_area



        #flame lenght

        if(!missing(flame_length)){
          #flame_length_logical <- paste(flame_length[1]," >= 8",sep="")
          my_shp_above_FL_threshold_available_exclude_threshold <- subset(combination_area_sf,eval(parse(text=flame_length_logical)))
          total_above_FL_threshold_available_exclude_threshold <- sum(my_shp_above_FL_threshold_available_exclude_threshold[,paste(area_field)][[1]])
          total_above_FL_threshold_available_exclude_threshold_perc <- round(total_above_FL_threshold_available_exclude_threshold/total_area*100,1)


          total_above_FL_threshold_available_exclude_threshold_perc_df<-c(total_above_FL_threshold_available_exclude_threshold_perc_df,total_above_FL_threshold_available_exclude_threshold_perc)
        }else{
          total_above_FL_threshold_available_exclude_threshold_perc_df<-NA
        }


        if(!missing(burn_probability)){
          total_expected_ba_year_available_exclude_threshold <- sum(combination_area_sf[,paste(area_field)][[1]]*combination_area_sf[,paste(burn_probability)][[1]])
          total_expected_ba_year_available_exclude_threshold_df<-c(total_expected_ba_year_available_exclude_threshold_df,total_expected_ba_year_available_exclude_threshold)
        }else{
          total_expected_ba_year_available_exclude_threshold_df<-NA
        }




        for(a in 1:length(objectives)){
          if(!missing(exclude_field)){
            obj1_after_threshold <- combination_area_sf[,c(area_field,paste(objectives)[a])]
            obj1_after_threshold <- sum(obj1_after_threshold[,paste(objectives)[[a]]][[1]])
          }else{
            obj1_after_threshold <- obj1_total_landscape
          }

          get_objectives_df_avai_excl_threshold <- rbind(get_objectives_df_avai_excl_threshold,c(obj1_after_threshold,a,e))
          colnames(get_objectives_df_avai_excl_threshold)<-c("objective_value","objective_order","threshold_order")
        }



      }
      #cbind(threshold_df_final[,c(1:3)],total_above_FL_threshold_available_exclude_threshold_perc_df)





      ###quizas tenho de tirar esta parte porque esta a calcular a area para os thresholds individuais - a ideia e ter em conjunto####
      #tirar daqui e meter no loop de cima
      for(e in 1:nrow(threshold_df_final)){
        threshold_df_loop_table <- threshold_df_final[e,]


        if(suppressWarnings(is.na(as.numeric(threshold_df_final$my_value_threshold[i])))){
          theshold_command_use_loop <- suppressWarnings(c(noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]),sep=" ")), as.character(paste("'",threshold_df_final$my_value_threshold[e],"'",sep=""))))


        }else{
          theshold_command_use_loop <- noquote(paste(threshold_df_final$my_threshold[e], noquote(threshold_df_final$my_operator[e]), as.numeric(threshold_df_final$my_value_threshold[e]),sep=" "))
        }


        combination_area_sf <- subset(combination_area_sf, eval(parse(text=theshold_command_use_loop)))
        after_available_and_exclude_and_thresholds <- combination_area_sf
      }



      #plot after available
      #after_available_and_exclude_and_thresholds
      after_available_and_exclude_and_thresholds$dissp <- 1

      after_available_and_exclude_and_thresholds_sf_plot <- suppressMessages(suppressWarnings(after_available_and_exclude_and_thresholds %>%
        group_by(dissp) %>%
        summarise(m = mean(dissp)) %>%
        sf::st_cast()))


      after_available_and_exclude_and_thresholds_sf_plot$considered <- "Yes"

      #after_available_and_exclude_and_thresholds_sf_plot <- st_transform(after_available_and_exclude_and_thresholds_sf_plot, crs = 4326)

      palFunc_considered_final <- leaflet::colorFactor(
        palette = c('green'),
        domain = after_available_and_exclude_and_thresholds_sf_plot$considered
      )

      after_available_and_exclude_and_thresholds_sf_plot_leaflet <- leaflet::leaflet() %>%
        leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
        #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%

        leaflet::addPolygons(data = my_shp_diss, fillColor = NA, fillOpacity = 0,
                             color = 'black', opacity=1,  weight=1, label=NA) %>%

        leaflet::addPolygons(data = after_available_and_exclude_and_thresholds_sf_plot, fillColor = ~palFunc_considered_final(considered), fillOpacity = 0.75,
                             color = 'black', opacity = 1, weight=0.5, label = ~considered ,stroke = TRUE,
                             highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                             group = 'considered')%>%
        leaflet::addLegend(data=after_available_and_exclude_and_thresholds_sf_plot, "topright",
                           #colors = c('darkolivegreen3'),
                           pal = palFunc_considered_final,
                           values = ~considered ,
                           title = "Residual stands after removal from treatment thresholds",
                           labels = c("Yes"),
                           group = "considered",
                           opacity = 1)%>%
        leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
                                  position = "topleft")


      suppressWarnings(assign("after_available_and_exclude_and_thresholds_sf_plot_leaflet",after_available_and_exclude_and_thresholds_sf_plot_leaflet,pos = 1))


    }else{
      threshold_df_final<-data.frame()
    }



    combination_area <- sum(combination_area_sf[,paste(area_field)][[1]])

    combination_objective_final <- numeric()
    for(z in 1:nrow(objectives_df_final)){


      combination_objective <- sum(combination_area_sf[,paste(objectives_df_final$objectives_list[z])][[1]])

      combination_objective_final <- c(combination_objective_final,combination_objective)
    }









    #export final figure with only considered stands

    combination_area_sf$dissp <- 1

    combination_area_sf_plot <- suppressMessages(suppressWarnings(combination_area_sf %>%
      group_by(dissp) %>%
      summarise(m = mean(dissp)) %>%
      sf::st_cast()))


    combination_area_sf_plot$considered <- "Yes"


    #combination_area_sf_plot <- st_transform(combination_area_sf_plot, crs = 4326)


    # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
    # #palFunc <- (c("white","grey"))

    palFunc_considered_final <- leaflet::colorFactor(
      palette = c('green'),
      domain = combination_area_sf_plot$considered
    )

    combination_area_sf_plot_leaflet <- leaflet::leaflet() %>%
      leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
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
                         opacity = 1)%>%
      leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
                                position = "topleft")


    suppressWarnings(assign("combination_area_sf_plot_leaflet",combination_area_sf_plot_leaflet,pos = 1))










    #
    #   ###flame lenght in table####
    #   if(!missing(flame_length)){
    #     #flame_length_logical <- paste(flame_length[1]," >= 8",sep="")
    #     my_shp_above_FL_threshold <- subset(my_shp,eval(parse(text=flame_length_logical)))
    #     total_area_above_FL_threshold <- sum(my_shp_above_FL_threshold[,paste(area)][[1]])
    #     total_area_above_FL_threshold_perc <- total_area_above_FL_threshold/total_area*100
    #
    #   }

    #create a table with the area
    final_area_table_report <- data.frame()




    if(nrow(threshold_df_final)>0){
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,threshold_df_final$area_considered))
      colnames(area_considered_table) <- "Area considered"
      row.names(area_considered_table) <- c("Planning area","Available land","Only not excluded",paste("Meets ",theshold_command_all_legend,sep=""))

    }else{
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area))
      colnames(area_considered_table) <- "Area considered"
      row.names(area_considered_table) <- c("Planning area","Available land","Only not excluded")
    }

    area_considered_table$`Area reduction` <- area_considered_table$`Area considered`- total_area
    area_considered_table$`Residual area (% of total)` <- round((total_area + area_considered_table$`Area reduction`)*100/total_area,1)

    name_obj_temp_final<-c("Area considered","Area reduction","Residual area (% of total)")

    for(v in 1:length(combination_objective_final)){
      area_considered_table$objtemp<-0

      area_considered_table$objtemp[nrow(area_considered_table)] <- combination_objective_final[v]
      name_obj_temp <- paste(objectives_df_final$objectives_list[v])
      name_obj_temp_final <- c(name_obj_temp_final,name_obj_temp)

      colnames(area_considered_table)<-name_obj_temp_final
    }


    for(h in 1:length(objectives)){
      #get_objectives_df_avai_excl
      area_considered_table["Planning area",(h+3)] <- round(obj_total_landscape_final$objective_value[[h]]/obj_total_landscape_final$objective_value[[h]]*100,1)


      if(!missing(available_field)){
        area_considered_table["Available land",(h+3)] <- round(get_objectives_df_avai$after_only_available[[h]]*100/obj_total_landscape_final$objective_value[[h]],1)
      }

      if(!missing(exclude_field)){
        area_considered_table["Only not excluded",(h+3)] <- round(get_objectives_df_avai_excl$after_only_exclude[[h]]*100/obj_total_landscape_final$objective_value[[h]],1)
      }

      if(!missing(threshold)){
        #
        get_objectives_df_final_loop_h <- subset(get_objectives_df_avai_excl_threshold,objective_order ==h)

        for(w in 1:max(get_objectives_df_avai_excl_threshold$threshold_order)){
          get_objectives_df_final_loop <- subset(get_objectives_df_final_loop_h,threshold_order == w)

          area_considered_table[paste("Meets", theshold_command_all_legend)[[w]],(h+3)] <- round(get_objectives_df_final_loop$objective_value*100/obj_total_landscape_final$objective_value[[h]],1)

        }

      }


      area_considered_table[,(1+ncol(area_considered_table))] <- area_considered_table[,paste(objectives[h])]/area_considered_table[,1]*100
      names(area_considered_table)[ncol(area_considered_table)]<-paste(objectives[h],"Potential treatment leverage (PCP per area)",sep=" ")
    }


    #flame length
    if(!missing(flame_length)){
      flame_length_col <- c(total_area_above_FL_threshold_perc,total_above_FL_threshold_available_perc,
                            total_above_FL_threshold_available_exclude_perc,total_above_FL_threshold_available_exclude_threshold_perc_df)

      #exclude NAs
      flame_length_col <- flame_length_col[!is.na(flame_length_col)]
      flame_length_col <- round(flame_length_col,1)

      #names(area_considered_table)[length(names(area_considered_table))]<-"Residual % of total area burning at high intensity"
      area_considered_table$`Residual % of total area burning at high intensity`<-flame_length_col

    }


    if(!missing(burn_probability)){
      burn_probability_col <- c(total_expected_ba_year,total_expected_ba_year_available,
                                total_expected_ba_year_available_exclude,
                                total_expected_ba_year_available_exclude_threshold_df)

      #exclude NAs
      burn_probability_col <- burn_probability_col[!is.na(burn_probability_col)]


      #names(area_considered_table)[length(names(area_considered_table))]<-"Expected fire impacts"
      area_considered_table$`Expected fire impacts`<-burn_probability_col

      for(t in  1:length(objectives)){
        area_considered_table$fire_impact_on_priority <- round(area_considered_table$`Expected fire impacts`*area_considered_table[,paste(objectives[t])],1)
        area_considered_table$fire_impact_on_priority <- round(area_considered_table$fire_impact_on_priority*100/area_considered_table$fire_impact_on_priority[1],1)
        names(area_considered_table)[length(names(area_considered_table))]<-paste("Expected fire impacts on",objectives[t],"(% burned per year)",sep=" ")
      }
    }


    area_considered_table_raw <- area_considered_table

    #get col area considered reduction


    #get the percentage
    # for(m in 1:ncol(area_considered_table)){
    #   perc_loop <- round(area_considered_table[,m]/area_considered_table[1,m]*100,0)
    #   val_and_perc_loop <- paste(area_considered_table[,m]," (",perc_loop,")",sep="")
    #
    #   area_considered_table[,m] <-val_and_perc_loop
    #
    # }
    area_considered_table <- area_considered_table[!(row.names(area_considered_table) %in% "Only not excluded"),]

    suppressWarnings(assign("area_considered_table",area_considered_table,pos = 1))




    #pie chart for landuse and landownership

    if(!missing(land_ownership)){


      objectives_per_landownership_selectable_df_final <- data.frame(c(unique(combination_area_sf[,paste(land_ownership)][[1]])))
      colnames(objectives_per_landownership_selectable_df_final)<-c(land_ownership)

      combination_area_sf_df <- st_drop_geometry(combination_area_sf)

      for(c in 1:length(objectives)){
        objectives_per_landownership_selectable_df <- data.frame(combination_area_sf_df %>%
                                                                   group_by(eval(parse(text=land_ownership)))%>%
                                                                   #summarise(total_obj_landownership = sum(noquote(paste(objectives[[c]]))))
                                                                   summarise(total_obj_landownership = sum(eval(parse(text=objectives[[c]])))))

        objectives_per_landownership_selectable_df <- objectives_per_landownership_selectable_df[,c(1:2)]
        colnames(objectives_per_landownership_selectable_df)<-c(land_ownership,paste("considered_",objectives[[c]],sep=""))


        objectives_per_landownership_selectable_df_final <- dplyr::left_join(objectives_per_landownership_selectable_df_final,objectives_per_landownership_selectable_df,by=land_ownership)
      }

      objectives_per_landownership_df_final<-dplyr::left_join(objectives_per_landownership_df_final,objectives_per_landownership_selectable_df_final,by=land_ownership)


      #get as percentage
      my_vector_landownership_considered_final<-character()
      my_vector_landownership_final<-character()
      for(c in 1:length(objectives)){
        objectives_per_landownership_df_final[is.na(objectives_per_landownership_df_final)] <- 0

        objectives_per_landownership_df_final[[paste0("perc_",objectives)[c]]] <- objectives_per_landownership_df_final[,paste(objectives)[c]]/sum(objectives_per_landownership_df_final[,paste(objectives)[c]])*100#area_considered_table_raw[,paste(objectives)[c]][1]*100

        objectives_per_landownership_df_final[[paste0("perc_considered_",objectives)[c]]] <- objectives_per_landownership_df_final[,paste0("considered_",objectives)[c]]/sum(objectives_per_landownership_df_final[,paste0("considered_",objectives)[c]])*100#area_considered_table_raw[,paste(objectives)[c]][1]*100

        my_vector_landownership <- paste(objectives_per_landownership_df_final$landownership,": ",round(objectives_per_landownership_df_final[,paste0("perc_",objectives)[c]],1),"%",sep="",collapse = ", ")
        my_vector_landownership <- paste("For ",objectives[c]," the proportions are as follows: ",my_vector_landownership,sep="")
        my_vector_landownership_final <- c(my_vector_landownership_final,my_vector_landownership)

        my_vector_landownership_considered <- paste(objectives_per_landownership_df_final$landownership,": ",round(objectives_per_landownership_df_final[,paste0("perc_considered_",objectives)[c]],1),"%",sep="",collapse = ", ")
        my_vector_landownership_considered <- paste("For ",objectives[c]," the proportions are as follows: ",my_vector_landownership_considered,sep="")
        my_vector_landownership_considered_final <- c(my_vector_landownership_considered_final,my_vector_landownership_considered)

      }

      my_vector_landownership_final_use<-paste(my_vector_landownership_final,collapse = ". ")
      my_vector_landownership_considered_final_use<-paste(my_vector_landownership_considered_final,collapse = ". ")





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

      suppressWarnings(assign("my_vector_landownership_final_use",my_vector_landownership_final_use,pos = 1))
      suppressWarnings(assign("my_vector_landownership_considered_final_use",my_vector_landownership_considered_final_use,pos = 1))



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

      combination_area_sf_df <- st_drop_geometry(combination_area_sf)

      for(c in 1:length(objectives)){
        objectives_per_landuse_selectable_df <- data.frame(combination_area_sf_df %>%
                                                             group_by(eval(parse(text=land_cover)))%>%
                                                             #summarise(total_obj_landuse = sum(noquote(paste(objectives[[c]]))))
                                                             summarise(total_obj_landuse = sum(eval(parse(text=objectives[[c]])))))

        objectives_per_landuse_selectable_df <- objectives_per_landuse_selectable_df[,c(1:2)]
        colnames(objectives_per_landuse_selectable_df)<-c(land_cover,paste("considered_",objectives[[c]],sep=""))


        objectives_per_landuse_selectable_df_final <- dplyr::left_join(objectives_per_landuse_selectable_df_final,objectives_per_landuse_selectable_df,by=land_cover)
      }

      objectives_per_landuse_df_final<-dplyr::left_join(objectives_per_landuse_df_final,objectives_per_landuse_selectable_df_final,by=land_cover)


      #get as percentage
      my_vector_landuse_final <- character()
      my_vector_landuse_considered_final <- character()
      for(c in 1:length(objectives)){
        objectives_per_landuse_df_final[is.na(objectives_per_landuse_df_final)] <- 0

        objectives_per_landuse_df_final[[paste0("perc_",objectives)[c]]] <- objectives_per_landuse_df_final[,paste(objectives)[c]]/sum(objectives_per_landuse_df_final[,paste(objectives)[c]])*100#area_considered_table_raw[,paste(objectives)[c]][1]*100

        objectives_per_landuse_df_final[[paste0("perc_considered_",objectives)[c]]] <- objectives_per_landuse_df_final[,paste0("considered_",objectives)[c]]/sum(objectives_per_landuse_df_final[,paste0("considered_",objectives)[c]])*100#area_considered_table_raw[,paste(objectives)[c]][1]*100

        my_vector_landuse <- paste(objectives_per_landuse_df_final[,land_cover],": ",round(objectives_per_landuse_df_final[,paste0("perc_",objectives)[c]],1),"%",sep="",collapse = ", ")
        my_vector_landuse <- paste("For ",objectives[c]," the proportions are as follows: ",my_vector_landuse,sep="")
        my_vector_landuse_final <- c(my_vector_landuse_final,my_vector_landuse)

        my_vector_landuse_considered <- paste(objectives_per_landuse_df_final[,land_cover],": ",round(objectives_per_landuse_df_final[,paste0("perc_considered_",objectives)[c]],1),"%",sep="",collapse = ", ")
        my_vector_landuse_considered <- paste("For ",objectives[c]," the proportions are as follows: ",my_vector_landuse_considered,sep="")
        my_vector_landuse_considered_final <- c(my_vector_landuse_considered_final,my_vector_landuse_considered)

      }

      my_vector_landuse_final_use<-paste(my_vector_landuse_final,collapse = ". ")
      my_vector_landuse_considered_final_use<-paste(my_vector_landuse_considered_final,collapse = ". ")

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
      suppressWarnings(assign("my_vector_landuse_final_use",my_vector_landuse_final_use,pos = 1))
      suppressWarnings(assign("my_vector_landuse_considered_final_use",my_vector_landuse_considered_final_use,pos = 1))





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
    if(!missing(available_field)){
      combination_area_sf <- subset(combination_area_sf, get(available_field)==1)
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



    combination_area <- sum(combination_area_sf[,paste(area_field)][[1]])


    #export final figure with only considered stands

    combination_area_sf$dissp <- 1

    combination_area_sf_plot <- suppressMessages(suppressWarnings(combination_area_sf %>%
      group_by(dissp) %>%
      summarise(m = mean(dissp)) %>%
      st_cast()))


    combination_area_sf_plot$considered <- "Yes"


    #combination_area_sf_plot <- st_transform(combination_area_sf_plot, crs = 4326)


    # palFunc <- leaflet::colorNumeric(c("white","grey"), 2, domain = NULL)
    # #palFunc <- (c("white","grey"))

    palFunc_considered_final <- leaflet::colorFactor(
      palette = c('green'),
      domain = combination_area_sf_plot$considered
    )

    combination_area_sf_plot_leaflet <- leaflet::leaflet() %>%
      leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
      leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
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
                         opacity = 1)%>%
      leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
                                position = "topleft")


    suppressWarnings(assign("combination_area_sf_plot_leaflet",combination_area_sf_plot_leaflet,pos = 1))




    #create a table with the area
    final_area_table_report <- data.frame()

    if(nrow(threshold_df_final)>0){
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area,threshold_df_final$area_considered))
      colnames(area_considered_table) <- "Area considered"
      row.names(area_considered_table) <- c("Planning area","Available land","Only not excluded",paste("Meets ",theshold_command_all_legend,sep=""))

    }else{
      area_considered_table <- data.frame(c(total_area,available_area,not_exclude_area))
      colnames(area_considered_table) <- "Area considered"
      row.names(area_considered_table) <- c("Planning area","Available land","Only not excluded")
    }



    for(m in 1:ncol(area_considered_table)){
      perc_loop <- round(area_considered_table[,m]/area_considered_table[1,m]*100,0)
      val_and_perc_loop <- paste(area_considered_table[,m]," (",perc_loop,")",sep="")

      area_considered_table[,m] <-val_and_perc_loop

    }

    area_considered_table<-area_considered_table[!(row.names(area_considered_table) %in% "Only not excluded"),]

    suppressWarnings(assign("area_considered_table",area_considered_table,pos = 1))
  }










  #then plot the combination of what we can treat after the availability and the combination of all thresholds

  path_for_rmd <- system.file("rmd_template", package = "ForSysXR")

  #remove unused variables from environment
  suppressWarnings(rm(list=setdiff(ls(), c("path_for_rmd","available_plot_leaflet","area_considered_table","after_available_sf_plot_leaflet",
                          "after_available_and_exclude_sf_plot_leaflet","after_available_and_exclude_and_thresholds_sf_plot_leaflet",
                          "combination_area_sf_plot_leaflet","landuse_landscape_ggarrange_plot","landuse_considered_ggarrange_plot",
                          "landownership_landscape_ggarrange_plot","landownership_considered_ggarrange_plot","my_vector_landownership_considered_final_use",
                          "my_vector_landownership_final_use","my_vector_landuse_considered_final_use","my_vector_landuse_final_use","report_name")),
                           envir = globalenv()))


  #check export html document
  save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

  cat("Creating report")
  capture.output(suppressWarnings(suppressMessages(generate_report_explore(output_file=paste("explore_",report_name,".html",sep="")))))

  if(export_static_report==TRUE){
    capture.output(suppressWarnings(suppressMessages(generate_report_explore_static(output_file=paste("explore_report_",report_name,".html",sep="")))))

  }

  file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

  if(web_upload==TRUE){
    output_folder <- getwd()
    ForSysXR:::upload_content(output_folder_web=output_folder,
                                    last_name_web=report_name,
                                    explore=TRUE)
  }

}
