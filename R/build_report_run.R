#' Title
#'
#' @param outputs_base_name
#' @param stand_shapefile
#' @param effect_fields
#' @param area
#' @param subunit_field
#' @param report_variables
#' @param static
#' @param interactive
#'
#' @return
#' @export
#'
#' @examples
build_report_run<-function(outputs_base_name,
                           stand_shapefile,
                           effect_fields,
                           area,
                           subunit_field=NULL,
                           report_variables,
                           static=TRUE,
                           interactive=FALSE,
                           write_commands=FALSE){


  all_effects <- length(effect_fields)

  all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

  all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
  all_elements_use <- as.character(all_elements_use)

  path_with_results <- paste(all_elements_use, collapse = '/')

  #list patterns
  #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

  last_name <- all_elements[,ncol(all_elements)]


    output_summary_run = intersect(list.files(path_with_results, "Summary.txt$"), list.files(path_with_results,pattern = last_name))



    all_text_summary<-readtext::readtext(paste(path_with_results,output_summary_run,sep="/"), text_field = "texts")

    all_text_summary<-all_text_summary$text

    all_text_summary <- gsub("\n","",all_text_summary)
    all_text_summary <- gsub("\t"," ",all_text_summary)


    #get if csv stands and/or shapefile was saved
    outputs_saved <- (list.files(path_with_results,pattern = last_name))

    outputs_saved_shp <- outputs_saved[grepl(pattern = ".shp$", x = outputs_saved)]

    outputs_saved_shp <- outputs_saved_shp[!grepl(pattern = "_stand_data.shp$", x = outputs_saved_shp)]

    stand_csv_saved<-outputs_saved[grepl(pattern = ".csv$", x = outputs_saved)]

    stand_csv_saved <- stand_csv_saved[!grepl(pattern = "_Results.csv$", x = stand_csv_saved)]



    prj_results <- read.csv(paste(path_with_results,"/",last_name,"_Results.csv",sep=""))

    number_scenarios_created<-length(unique(prj_results$PointOutputsName))


    if(length(number_scenarios_created) == 1){

      output_csv_run = intersect(list.files(path_with_results, ".csv$"), list.files(path_with_results,pattern = last_name))

      #load the _Results
      prj_results <- read.csv(paste(path_with_results,"/",last_name,"_Results.csv",sep=""))

      #exclude this file from the output_csv_run
      exclude_this_file_csv <- paste(last_name,"_Results.csv",sep="")

      #prj_results



      df_loop_effects_final <- data.frame()


      if(length(prj_results$Subunit)>0){

        for(x in 1:all_effects){
          my_effect_chosen <- effect_fields[x]
          df_loop_effects <- prj_results[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen),"Subunit")]
          df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)


          colnames(df_loop_effects) <- c("ProjectNumber", "effect", "Subunit","effect_name")

          df_loop_effects_x <-data.frame(df_loop_effects %>%
                                           group_by(Subunit)%>%
                                           reframe(effect_cumulative=cumsum(effect)))

          df_loop_effects <- df_loop_effects[order(df_loop_effects$Subunit),]
          df_loop_effects$effect_cumulative <- df_loop_effects_x$effect_cumulative


          #df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

          df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)

        }




        plot_attainment_per_project <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name,linetype=factor(Subunit)))+
                                                                            geom_point()+
                                                                            geom_line(linewidth=1)+
                                                                            scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                            xlab("Project number")+
                                                                            ylab("Objective attainment")+
                                                                            labs(color = "Effect name",linetype="Subunit",tag="a)")+
                                                                            theme_classic())))

        suppressWarnings(assign("plot_attainment_per_project",plot_attainment_per_project,pos = 1))



        plot_attainment_per_project_cum <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name,linetype=factor(Subunit)))+
                                                                                geom_point()+
                                                                                geom_line(linewidth=1)+
                                                                                scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                xlab("Project number")+
                                                                                ylab("Cumulative objective attainment")+
                                                                                labs(color = "Effect name",linetype="Subunit",tag="b)")+
                                                                                theme_classic())))

        suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))




      }else{

        df_loop_effects_per_proj_final <- data.frame()
        for(x in 1:all_effects){
          my_effect_chosen <- effect_fields[x]
          df_loop_effects <- prj_results[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen),paste0("Treat_",area))]

          df_loop_effects_per_proj <- df_loop_effects

          df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)


          colnames(df_loop_effects) <- c("ProjectNumber", "effect", "Treat_Area","effect_name")

          df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)
          df_loop_effects$area_cumulative <- cumsum(df_loop_effects$Treat_Area)

          df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)


          df_loop_effects_per_proj$effect_name <- names(df_loop_effects_per_proj)[2]
          colnames(df_loop_effects_per_proj)<-c("ProjectNumber", "treated_value","Treat_Area","effect_name")

          df_loop_effects_per_proj_final<-rbind(df_loop_effects_per_proj_final,df_loop_effects_per_proj)

        }




        #get the min value. If negative, we add an y=0 line
        numeric_cols <- sapply(df_loop_effects_final, is.numeric)
        min_value_pcps <- min(unlist(df_loop_effects_final[,numeric_cols]))


        if(min_value_pcps<0){
          plot_attainment_per_project <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                              geom_point()+
                                                                              geom_line(linewidth=1)+
                                                                              geom_segment(x = 0, y = 0, xend = (max(df_loop_effects_final$ProjectNumber)+0.5), yend = 0,linewidth=1,color="black")+
                                                                              scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                              scale_color_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                                                                                                 name = "Effect name") +
                                                                              xlab("Project number")+
                                                                              ylab("Objective attainment")+
                                                                              labs(color = "Effect name",tag="a)")+
                                                                              theme_classic())))

          suppressWarnings(assign("plot_attainment_per_project",plot_attainment_per_project,pos = 1))




          plot_attainment_per_project_cum <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=area_cumulative,y=effect_cumulative,color=effect_name))+
                                                                                  geom_point()+
                                                                                  geom_line(linewidth=1)+
                                                                                  geom_segment(x = 0, y = 0, xend = (max(df_loop_effects_final$area_cumulative)+0.5), yend = 0,linewidth=1,color="black")+
                                                                                  #scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                  scale_color_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                                                                                                     name = "Effect name") +
                                                                                  xlab("Area treated")+
                                                                                  ylab("Cumulative objective attainment")+
                                                                                  labs(color = "Effect name",tag="b)")+
                                                                                  theme_classic())))


          suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))

        }




        if(min_value_pcps>=0){
          plot_attainment_per_project <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                              geom_point()+
                                                                              geom_line(linewidth=1)+
                                                                              #geom_segment(x = 0, y = 0, xend = (max(df_loop_effects_final$ProjectNumber)+0.5), yend = 0,linewidth=1,color="black")+
                                                                              scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                              scale_color_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                                                                                                 name = "Effect name") +
                                                                              xlab("Project number")+
                                                                              ylab("Objective attainment")+
                                                                              labs(color = "Effect name",tag="a)")+
                                                                              theme_classic())))

          suppressWarnings(assign("plot_attainment_per_project",plot_attainment_per_project,pos = 1))




          plot_attainment_per_project_cum <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=area_cumulative,y=effect_cumulative,color=effect_name))+
                                                                                  geom_point()+
                                                                                  geom_line(linewidth=1)+
                                                                                  #geom_segment(x = 0, y = 0, xend = (max(df_loop_effects_final$area_cumulative)+0.5), yend = 0,linewidth=1,color="black")+
                                                                                  #scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                  scale_color_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                                                                                                     name = "Effect name") +
                                                                                  xlab("Area treated")+
                                                                                  ylab("Cumulative objective attainment")+
                                                                                  labs(color = "Effect name",tag="b)")+
                                                                                  theme_classic())))


          suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))

        }





        #barplot



        plot_attainment_per_project_barplot <- ggplot(df_loop_effects_per_proj_final, aes(x = ProjectNumber, y = treated_value, fill = effect_name)) +
          geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "black") +
          xlab("Project rank") +
          ylab("Objective treated") +
          cowplot::theme_cowplot() +
          scale_fill_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                            name = "Effect") +
          #scale_fill_discrete(name = "Effect")+
          #scale_x_continuous(breaks = my_breaks) +
          theme(legend.position = "bottom",
                legend.box = "horizontal",
                legend.box.just = "center",
                legend.title = element_text(hjust = 0.5),
                legend.text = element_text(hjust = 0.5),
                legend.justification = "center")+
          geom_segment(aes(x = 0.5, y = 0, xend = (max(ProjectNumber)+0.5), yend = 0),linewidth=1)

        suppressWarnings(assign("plot_attainment_per_project_barplot",plot_attainment_per_project_barplot,pos = 1))


      }





      #plot cumulative




      plot_attainment_per_project_ggarranged<- ggpubr::ggarrange(plot_attainment_per_project,plot_attainment_per_project_cum,
                                                                 ncol = 1,nrow=2,common.legend = TRUE)


      suppressWarnings(assign("plot_attainment_per_project_ggarranged",plot_attainment_per_project_ggarranged,pos = 1))

      suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
      #suppressWarnings(assign("caption_fig_constraint2",paste("Proportion of constraint treated and total constraint inside project (%) per project. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))




    }








    #start to get the variables
    objectives_pt1 <- gsub(".*Objectives: ","",all_text_summary)
    objectives_pt1 <- gsub("Thresholds.*","",objectives_pt1)
    objectives_pt1 <- stringr::str_sub(objectives_pt1,5)

    #separate the elements

    objectives_pt1_use <- (strsplit(objectives_pt1, " +")[[1]])
    length(objectives_pt1_use)

    #see how many elements can be numeric
    choose_numeric_objective <- suppressWarnings(as.numeric(objectives_pt1_use))
    all_objectives <- which(is.na(choose_numeric_objective)==TRUE)
    all_values_objectives <- which(is.na(choose_numeric_objective)==FALSE)



    #get the objective direction
    objective_direction <- gsub(".*Objective Direction:","",all_text_summary)
    objective_direction <- gsub("Check.*","",objective_direction)


    if(length(all_objectives)>1){

      all_objectives_elements <- as.list((paste(objectives_pt1_use[all_objectives],objective_direction,sep=", ")))
      all_objectives_elements <- do.call("rbind", all_objectives_elements)

      all_objectives_values <- as.list((paste(objectives_pt1_use[all_values_objectives],objective_direction,sep=", ")))
      all_objectives_values <- do.call("rbind", all_objectives_values)

      introduction_table <- cbind(all_objectives_elements,all_objectives_values)


      introduction_table <- data.frame(cbind("Objective(s)",introduction_table))

      colnames(introduction_table)<-c("Parameter","Variable","Value")


    }


    if(length(all_objectives)==1){
      introduction_table <- cbind(paste(objectives_pt1_use[all_objectives],objective_direction,sep=", "),objectives_pt1_use[all_values_objectives])


      introduction_table <- data.frame(cbind("Objective(s)",introduction_table))

      colnames(introduction_table)<-c("Parameter","Variable","Value")
    }





    #threshold part

    thresholds_pt1 <- gsub(".*Thresholds: ","",all_text_summary)
    thresholds_pt1 <- gsub("Constraints.*","",thresholds_pt1)
    thresholds_pt1 <- stringr::str_sub(thresholds_pt1,5)

    #separate the elements

    thresholds_pt1_use <- (strsplit(thresholds_pt1, " +")[[1]])
    length(thresholds_pt1_use)

    #see how many elements can be numeric
    choose_numeric_thresholds <- suppressWarnings(as.numeric(thresholds_pt1_use))
    all_thresholds <- which(is.na(choose_numeric_thresholds)==TRUE)
    all_values_thresholds <- which(is.na(choose_numeric_thresholds)==FALSE)


    if(length(all_thresholds)==2){

      table_threshold <- cbind(thresholds_pt1_use[all_thresholds][1],paste(thresholds_pt1_use[all_thresholds][2],thresholds_pt1_use[all_values_thresholds],sep=" "))


      table_threshold <- data.frame(cbind("Threshold(s)",table_threshold))

      colnames(table_threshold)<-c("Parameter","Variable","Value")

      introduction_table <- rbind(introduction_table,table_threshold)
    }


    if(length(all_thresholds)>2){

      all_thresholds_names <- seq(all_thresholds[1], length(all_thresholds), 2)
      all_thresholds_symbol <- seq(all_thresholds[2], length(all_thresholds), 2)

      all_thresholds_names<-all_thresholds[all_thresholds_names]
      all_thresholds_symbol<-all_thresholds[all_thresholds_symbol]

      all_thresholds_elements <- as.list((thresholds_pt1_use[all_thresholds_names]))
      all_thresholds_elements <- do.call("rbind", all_thresholds_elements)


      all_thresholds_value <- as.list(paste(thresholds_pt1_use[all_thresholds_symbol],thresholds_pt1_use[all_values_thresholds],sep=" "))
      all_thresholds_value <- do.call("rbind", all_thresholds_value)



      table_threshold <- cbind(all_thresholds_elements,all_thresholds_value)


      table_threshold <- data.frame(cbind("Threshold(s)",table_threshold))

      colnames(table_threshold)<-c("Parameter","Variable","Value")

      introduction_table <- rbind(introduction_table,table_threshold)
    }




    #constraints

    constraints_pt1 <- gsub(".*Constraints: ","",all_text_summary)
    constraints_pt1 <- gsub("Objective Direction.*","",constraints_pt1)
    constraints_pt1 <- stringr::str_sub(constraints_pt1,5)

    #separate the elements

    constraints_pt1_use <- (strsplit(constraints_pt1, " +")[[1]])
    length(constraints_pt1_use)

    #see how many elements can be numeric
    choose_numeric_constraints <- suppressWarnings(as.numeric(constraints_pt1_use))
    all_constraints <- which(is.na(choose_numeric_constraints)==TRUE)
    all_values_constraints <- which(is.na(choose_numeric_constraints)==FALSE)




    nth_element <- function(vector, starting_position, n) {
      vector[seq(starting_position, length(vector), n)]
    }


    if(length(all_constraints)==2){

      all_constraints_names <- seq(all_constraints[1], length(all_constraints), 2)
      all_constraints_value_start <- nth_element(all_values_constraints, 1, 2)
      all_constraints_value_end <- nth_element(all_values_constraints, 2, 2)

      all_constraints_names<-all_constraints[all_constraints_names]
      #all_constraints_value_start<-all_values_constraints[all_constraints_value_start]

      all_constraints_elements <- as.list((constraints_pt1_use[all_constraints_names]))
      all_constraints_elements <- do.call("rbind", all_constraints_elements)


      all_constraints_value <- as.list(paste(constraints_pt1_use[all_constraints_value_start],"-",constraints_pt1_use[all_constraints_value_end],sep=" "))
      all_constraints_value <- do.call("rbind", all_constraints_value)


      table_constraints <- cbind(constraints_pt1_use[all_constraints][1],paste(constraints_pt1_use[all_values_constraints][1]," - ",constraints_pt1_use[all_values_constraints][2],sep=" "))


      table_constraints <- data.frame(cbind("Constraint(s)",table_constraints))

      colnames(table_constraints)<-c("Parameter","Variable","Value")

      introduction_table <- rbind(introduction_table,table_constraints)
    }









    if(length(all_constraints)>2){

      all_constraints_names <- seq(all_constraints[1], length(all_constraints), 2)
      all_constraints_value_start <- nth_element(all_values_constraints, 1, 2)
      all_constraints_value_end <- nth_element(all_values_constraints, 2, 2)

      all_constraints_names<-all_constraints[all_constraints_names]
      #all_constraints_value_start<-all_values_constraints[all_constraints_value_start]

      all_constraints_elements <- as.list((constraints_pt1_use[all_constraints_names]))
      all_constraints_elements <- do.call("rbind", all_constraints_elements)


      all_constraints_value <- as.list(paste(constraints_pt1_use[all_constraints_value_start],"-",constraints_pt1_use[all_constraints_value_end],sep=" "))
      all_constraints_value <- do.call("rbind", all_constraints_value)



      table_constraint <- cbind(all_constraints_elements,all_constraints_value)


      table_constraint <- data.frame(cbind("Constraint(s)",table_constraint))

      colnames(table_constraint)<-c("Parameter","Variable","Value")

      introduction_table <- rbind(introduction_table,table_constraint)
    }



    #max number of projects

    Nprojects_pt1 <- gsub(".*Max Number Projects:  ","",all_text_summary)
    Nprojects_pt1 <- gsub("Max Project Diameter.*","",Nprojects_pt1)

    table_Nprojects <- rbind(c("Max Number Projects","-",Nprojects_pt1))
    colnames(table_Nprojects)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_Nprojects)


    #max diameter
    diameter_pt1 <- gsub("Seed Stand Percent:  .*","",all_text_summary)
    diameter_pt1 <- gsub(".*Max Project Diameter ","",diameter_pt1)
    diameter_pt1 <- stringr::str_sub(diameter_pt1,11)


    table_diameter <- rbind(c("Max Project Diameter (meters)","-",diameter_pt1))
    colnames(table_diameter)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_diameter)



    #IDP
    IDP_pt1 <- gsub("Patch Buster:  .*","",all_text_summary)
    IDP_pt1 <- gsub(".*Inverse Distance Power:  ","",IDP_pt1)


    table_IDP <- rbind(c("Inverse Distance Power","-",IDP_pt1))
    colnames(table_IDP)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_IDP)




    #% seeds used
    seeds_perc_pt1 <- gsub("Number of CPUs:.*","",all_text_summary)
    seeds_perc_pt1 <- gsub(".*Seed Stand Percent:  ","",seeds_perc_pt1)


    table_seeds_perc <- rbind(c("Seed Stand Percent","-",seeds_perc_pt1))
    colnames(table_seeds_perc)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_seeds_perc)


    #patch buster
    patch_bus_pt1 <- gsub("Max Number Projects:.*","",all_text_summary)
    patch_bus_pt1 <- gsub(".*Patch Buster:   ","",patch_bus_pt1)


    table_patch_bus <- rbind(c("Patch Buster","-",patch_bus_pt1))
    colnames(table_patch_bus)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_patch_bus)



    #spatial optimization?
    spat_opti_pt1 <- gsub("Inverse Distance Power.*","",all_text_summary)
    spat_opti_pt1 <- gsub(".*Spatial Optimization:  ","",spat_opti_pt1)


    table_spat_opti <- rbind(c("Spatial Optimization","-",spat_opti_pt1))
    colnames(table_spat_opti)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_spat_opti)



    #check availability?
    check_avai_pt1 <- gsub("Spatial Optimization.*","",all_text_summary)
    check_avai_pt1 <- gsub(".*Check Availability:  ","",check_avai_pt1)


    table_check_avai <- rbind(c("Check Availability","-",check_avai_pt1))
    colnames(table_check_avai)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_check_avai)


    #check exclude?
    check_exc_pt1 <- gsub("Check Availability.*","",all_text_summary)
    check_exc_pt1 <- gsub(".*Check Excluded Stands:  ","",check_exc_pt1)


    table_check_exc <- rbind(c("Check Excluded Stands","-",check_exc_pt1))
    colnames(table_check_exc)<-c("Parameter","Variable","Value")

    introduction_table <- rbind(introduction_table,table_check_exc)







    suppressWarnings(assign("introduction_table",introduction_table,pos = 1))








    ###if exporting shapefile######
    if(length(outputs_saved_shp)>0){

      output_shp_run <- sf::st_read(paste(path_with_results,outputs_saved_shp,sep="/"),quiet=TRUE)


      if(class(stand_shapefile)[1]=="character"){
        my_shp <- sf::st_read(stand_shapefile,quiet=TRUE)
      }

      if(class(stand_shapefile)[1]=="sf"){
        my_shp <- stand_shapefile
      }



      my_shp$diss <- 1


      my_shp$sub_id_package <- 0

      if(!is.null(subunit_field)){
        my_shp$sub_id_package <- my_shp[,paste(subunit_field)][[1]]
      }




      geos_my_shp<- my_shp[,"sub_id_package"]
      geos_my_shp <- geos::as_geos_geometry(geos_my_shp)

      geos_my_shp_diss <- geos_my_shp%>%
        geos::geos_make_collection() %>%
        geos::geos_unary_union()

      my_shp_diss <- sf::st_as_sf(geos_my_shp_diss)

      my_shp_diss_line<-suppressWarnings(my_shp_diss%>%
                                           sf::st_cast("MULTIPOLYGON") %>%
                                           sf::st_cast("POLYGON") %>%
                                           sf::st_cast("LINESTRING"))

      my_shp_diss_polygon<-suppressWarnings(my_shp_diss%>%
                                              sf::st_cast("MULTIPOLYGON") %>%
                                              sf::st_cast("POLYGON"))




      my_breaks<-max(output_shp_run$ProjectNum)/4

      my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


      #dissolve the projects - has to be using sf

      #geos_output_shp_run <- geos::as_geos_geometry(output_shp_run)

      geos_output_shp_run_diss <- output_shp_run%>%
        group_by(ProjectNum) %>%
        summarise(m = mean(ProjectNum))

      #output_shp_run_diss <- sf::st_as_sf(geos_output_shp_run_diss)

      output_shp_run_diss<-suppressWarnings(geos_output_shp_run_diss%>%
                                              sf::st_cast("MULTIPOLYGON") %>%
                                              sf::st_cast("POLYGON"))


      output_shp_run_treat <- subset(output_shp_run,Treat==1)
      output_shp_run_treat_diss <- output_shp_run_treat%>%
        group_by(ProjectNum) %>%
        summarise(m = mean(ProjectNum))

      #output_shp_run_diss <- sf::st_as_sf(geos_output_shp_run_diss)

      output_shp_run_diss_treat<-suppressWarnings(output_shp_run_treat_diss%>%
                                                    sf::st_cast("MULTIPOLYGON") %>%
                                                    sf::st_cast("POLYGON"))





      #plot other variables


      #plot report_variables
      if(!missing(report_variables)){

        # output_shp_run_variable<-subset(output_shp_run,Treat==1)
        #
        my_shp$id_join_loop<-1:nrow(my_shp)
        #
        # output_shp_run_diss<-output_shp_run_variable%>%
        #   group_by(ProjectNum)%>%
        #   summarise(m = mean(ProjectNum)) %>%
        #   sf::st_cast()
        #

        output_shp_run_diss_treat$id_join <- 1:nrow(output_shp_run_diss_treat)

        stands_treated_list <- data.frame(sf::st_within(my_shp,output_shp_run_diss_treat))

        stands_treated_list <- dplyr::left_join(stands_treated_list,output_shp_run_diss_treat,by=c("col.id"="id_join"))

        my_shp_df_variable <-sf::st_drop_geometry(my_shp)
        my_shp_df_variable<-dplyr::left_join(my_shp_df_variable,stands_treated_list,by=c("id_join_loop"="row.id"))

        my_shp_df_variable_use <- subset(my_shp_df_variable, (!is.na(my_shp_df_variable$col.id)))


        Pattern1_list <- lapply(1:length(report_variables), function(w) {
          treated_per_variable <- my_shp_df_variable_use %>%
            group_by(ProjectNum, .data[[report_variables[w]]]) %>%
            summarise(total_area_treated = sum(.data[[area]]), .groups = 'drop')

          colnames(treated_per_variable) <- c("ProjectNumber", report_variables[w], "Area_treated")

          my_breaks <- max(treated_per_variable$ProjectNumber) / 4
          my_breaks <- round(c(my_breaks, my_breaks*2, my_breaks*3, my_breaks*4), 0)

          ggplot(treated_per_variable, aes_string(x = "ProjectNumber", y = "Area_treated", fill = report_variables[w])) +
            geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "black") +
            xlab("Project rank") +
            ylab("Area treated") +
            cowplot::theme_cowplot() +
            scale_fill_manual(values = as.vector(pals::kovesi.isoluminant_cgo_80_c38(length(unique(treated_per_variable[[2]])))),
                              name = report_variables[w]) +
            scale_x_continuous(breaks = my_breaks) +
            theme(legend.position = "bottom",
                  legend.box = "horizontal",
                  legend.box.just = "center",
                  legend.title = element_text(hjust = 0.5),
                  legend.text = element_text(hjust = 0.5),
                  legend.justification = "center")




          ###plotly####

          # jp<-ggplot(treated_per_variable, aes_string(x = "ProjectNumber", y = "Area_treated", fill = report_variables[w])) +
          #   geom_bar(stat = "identity", position = position_stack(reverse = TRUE), color = "black") +
          #   xlab("Project rank") +
          #   ylab("Area treated") +
          #   cowplot::theme_cowplot() +
          #   scale_fill_manual(values = as.vector(pals::kovesi.isoluminant_cgo_80_c38(length(unique(treated_per_variable[[2]])))),
          #                     name = report_variables[w]) +
          #   scale_x_continuous(breaks = my_breaks) +
          #   theme(legend.position = "bottom",
          #         legend.box = "horizontal",
          #         legend.box.just = "center",
          #         legend.title = element_text(hjust = 0.5),
          #         legend.text = element_text(hjust = 0.5),
          #         legend.justification = "center")
          #
          # plotly::ggplotly(jp)


        })



        my_plot_variables_bars_report<- ggpubr::ggarrange(plotlist=Pattern1_list,
                                                          ncol = 1,nrow=length(Pattern1_list),common.legend = FALSE)

        fig.height_variables_bars <- 5*length(Pattern1_list)
        suppressWarnings(assign("fig.height_variables_bars",fig.height_variables_bars,pos = 1))
        suppressWarnings(assign("my_plot_variables_bars_report",my_plot_variables_bars_report,pos = 1))

        caption_report_variables <- paste("Amount of area treated per project and per costum variable(s), namely",paste(report_variables,collapse = " and "),sep=" ")
        suppressWarnings(assign("caption_report_variables",caption_report_variables,pos = 1))

      }else{
        fig.height_variables_bars <- 5
        caption_report_variables<-""
        suppressWarnings(assign("fig.height_variables_bars",fig.height_variables_bars,pos = 1))
        suppressWarnings(assign("caption_report_variables",caption_report_variables,pos = 1))
      }








      #constraints

      constraints_pt1 <- gsub(".*Constraints: ","",all_text_summary)
      constraints_pt1 <- gsub("Objective Direction.*","",constraints_pt1)
      constraints_pt1 <- stringr::str_sub(constraints_pt1,5)

      #separate the elements

      constraints_pt1_use <- (strsplit(constraints_pt1, " +")[[1]])
      length(constraints_pt1_use)

      constraints_name_for_figure<-constraints_pt1_use[as.numeric(all_constraints_names)]

      df_loop_constraints_final <- data.frame()

      if(length(number_scenarios_created) == 1){
        if(length(prj_results$Subunit)>0){

          for(x in 1:length(constraints_name_for_figure)){
            my_constraint_chosen <- constraints_name_for_figure[x]
            df_loop_constraints <- prj_results[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen),"Subunit")]
            df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


            colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint","Subunit", "constraint_name")



            df_loop_constraints_x <-data.frame(df_loop_constraints %>%
                                                 group_by(Subunit)%>%
                                                 reframe(treat_constraint_cumulative=cumsum(Treat_constraint),
                                                         total_constraint_cumulative=cumsum(Total_constraint)))

            df_loop_constraints <- df_loop_constraints[order(df_loop_constraints$Subunit),]
            df_loop_constraints$treat_constraint_cumulative <- df_loop_constraints_x$treat_constraint_cumulative
            df_loop_constraints$total_constraint_cumulative <- df_loop_constraints_x$total_constraint_cumulative

            #
            #
            # df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
            # df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

            #get percentage of constraint treated of the total constraint inside the project
            df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


            df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)



          }
          plot_treated_constraint <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name,linetype=factor(Subunit)))+
                                                                          geom_point()+
                                                                          geom_line(linewidth=1)+
                                                                          scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                          xlab("Project number")+
                                                                          ylab("Treated constraint")+
                                                                          labs(color = "Constraint name",linetype="Subunit")+
                                                                          theme_classic())))

          suppressWarnings(assign("plot_treated_constraint",plot_treated_constraint,pos = 1))



          plot_perc_treated_constraint_in_proj <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name,linetype=factor(Subunit)))+
                                                                                       geom_point()+
                                                                                       geom_line(linewidth=1)+
                                                                                       scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                       xlab("Project number")+
                                                                                       ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                       labs(color = "Constraint name",linetype="Subunit")+
                                                                                       theme_classic())))

        }else{

          for(x in 1:length(constraints_name_for_figure)){
            my_constraint_chosen <- constraints_name_for_figure[x]
            df_loop_constraints <- prj_results[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen))]
            df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


            colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint", "constraint_name")

            df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
            df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

            #get percentage of constraint treated of the total constraint inside the project
            df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


            df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)

          }


          plot_treated_constraint <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name))+
                                                                          geom_point()+
                                                                          geom_line(linewidth=1)+
                                                                          scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                          xlab("Project number")+
                                                                          ylab("Treated constraint")+
                                                                          labs(color = "Constraint name",linetype="Subunit")+
                                                                          theme_classic())))

          suppressWarnings(assign("plot_treated_constraint",plot_treated_constraint,pos = 1))



          plot_perc_treated_constraint_in_proj <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name))+
                                                                                       geom_point()+
                                                                                       geom_line(linewidth=1)+
                                                                                       scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                       xlab("Project number")+
                                                                                       ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                       labs(color = "Constraint name",linetype="Subunit")+
                                                                                       theme_classic())))

        }







        plot_treated_constraint_in_proj_ggarrange <- ggpubr::ggarrange(plot_treated_constraint,
                                                                       plot_perc_treated_constraint_in_proj,
                                                                       ncol = 1,nrow=2,common.legend = TRUE)


        suppressWarnings(assign("plot_treated_constraint_in_proj_ggarrange",plot_treated_constraint_in_proj_ggarrange,pos = 1))

        #suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
        suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project (top) and Proportion of constraint treated and total constraint inside project (%) per project (bottom)."),pos = 1))



        #suppressWarnings(assign("plot_perc_treated_constraint_in_proj",plot_perc_treated_constraint_in_proj,pos = 1))
        #suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project"),pos = 1))
        #suppressWarnings(assign("caption_fig_constraint2",paste("Proportion of constraint treated and total constraint inside project (%) per project"),pos = 1))

      }







      if(length(prj_results$Subunit)>0){

        if(length(number_scenarios_created) > 1){
          all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

          all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
          all_elements_use <- as.character(all_elements_use)

          path_with_results <- paste(all_elements_use, collapse = '/')

          #list patterns
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          last_name <- all_elements[,ncol(all_elements)]
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          output_csv_run = intersect(list.files(path_with_results, ".csv$"), list.files(path_with_results,pattern = last_name))

          #load the _Results
          prj_results <- read.csv(paste(path_with_results,"/",last_name,"_Results.csv",sep=""))

          #names(prj_results)[names(prj_results) == 'ProjectNum'] <- 'ProjectNumber'

          #exclude this file from the output_csv_run
          exclude_this_file_csv <- paste(last_name,"_Results.csv",sep="")

          #prj_results


          #output_shp_run_lowest1_highest2
          #output_shp_run_highest1_lowest2
          #output_shp_run_balanced



          df_loop_effects_final <- data.frame()

          for(x in 1:all_effects){
            my_effect_chosen <- effect_fields[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)


            df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_lowest1_highest2,sep="\\"))

            df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen),"Subunit")]
            df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)





            colnames(df_loop_effects) <- c("ProjectNumber", "effect", "Subunit","effect_name")


            df_loop_effects_x <-data.frame(df_loop_effects %>%
                                             group_by(Subunit)%>%
                                             reframe(effect_cumulative=cumsum(effect)))

            df_loop_effects <- df_loop_effects[order(df_loop_effects$Subunit),]
            df_loop_effects$effect_cumulative <- df_loop_effects_x$effect_cumulative


            #df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

            df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)



          }




          plot_attainment_per_project1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name,linetype=factor(Subunit)))+
                                                                               geom_point()+
                                                                               geom_line(linewidth=1)+
                                                                               scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                               xlab("Project number")+
                                                                               ylab("Objective attainment")+
                                                                               labs(color = "Effect name",linetype="Subunit",tag = "a)")+
                                                                               theme_classic())))







          #plot cumulative



          plot_attainment_per_project_cum1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name,linetype=factor(Subunit)))+
                                                                                   geom_point()+
                                                                                   geom_line(linewidth=1)+
                                                                                   scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                   xlab("Project number")+
                                                                                   ylab("Cumulative objective attainment")+
                                                                                   labs(color = "Effect name",linetype="Subunit",tag = "a)")+
                                                                                   theme_classic())))






          #output_shp_run_lowest1_highest2
          #output_shp_run_highest1_lowest2
          #output_shp_run_balanced



          df_loop_effects_final <- data.frame()

          for(x in 1:all_effects){
            my_effect_chosen <- effect_fields[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)


            df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_highest1_lowest2,sep="\\"))


            df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen),"Subunit")]
            df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)





            colnames(df_loop_effects) <- c("ProjectNumber", "effect", "Subunit","effect_name")


            df_loop_effects_x <-data.frame(df_loop_effects %>%
                                             group_by(Subunit)%>%
                                             reframe(effect_cumulative=cumsum(effect)))

            df_loop_effects <- df_loop_effects[order(df_loop_effects$Subunit),]
            df_loop_effects$effect_cumulative <- df_loop_effects_x$effect_cumulative


            #df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

            df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)

          }




          plot_attainment_per_project2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name,linetype=factor(Subunit)))+
                                                                               geom_point()+
                                                                               geom_line(linewidth=1)+
                                                                               scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                               xlab("Project number")+
                                                                               ylab("Objective attainment")+
                                                                               labs(color = "Effect name",linetype="Subunit",tag = "b)")+
                                                                               theme_classic())))







          #plot cumulative



          plot_attainment_per_project_cum2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name,linetype=factor(Subunit)))+
                                                                                   geom_point()+
                                                                                   geom_line(linewidth=1)+
                                                                                   scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                   xlab("Project number")+
                                                                                   ylab("Cumulative objective attainment")+
                                                                                   labs(color = "Effect name",linetype="Subunit",tag = "b)")+
                                                                                   theme_classic())))









          #output_shp_run_lowest1_highest2
          #output_shp_run_highest1_lowest2
          #output_shp_run_balanced



          df_loop_effects_final <- data.frame()

          for(x in 1:all_effects){
            my_effect_chosen <- effect_fields[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)


            df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_balanced,sep="\\"))


            df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen),"Subunit")]
            df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)


            colnames(df_loop_effects) <- c("ProjectNumber", "effect", "Subunit","effect_name")


            df_loop_effects_x <-data.frame(df_loop_effects %>%
                                             group_by(Subunit)%>%
                                             reframe(effect_cumulative=cumsum(effect)))

            df_loop_effects <- df_loop_effects[order(df_loop_effects$Subunit),]
            df_loop_effects$effect_cumulative <- df_loop_effects_x$effect_cumulative


            #df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

            df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)



          }




          plot_attainment_per_project3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name,linetype=factor(Subunit)))+
                                                                               geom_point()+
                                                                               geom_line(linewidth=1)+
                                                                               scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                               xlab("Project number")+
                                                                               ylab("Objective attainment")+
                                                                               labs(color = "Effect name",linetype="Subunit",tag = "c)")+
                                                                               theme_classic())))







          #plot cumulative



          plot_attainment_per_project_cum3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name,linetype=factor(Subunit)))+
                                                                                   geom_point()+
                                                                                   geom_line(linewidth=1)+
                                                                                   scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                   xlab("Project number")+
                                                                                   ylab("Cumulative objective attainment")+
                                                                                   labs(color = "Effect name",linetype="Subunit",tag = "c)")+
                                                                                   theme_classic())))




          plot_attainment_per_project<- ggpubr::ggarrange(plot_attainment_per_project1,plot_attainment_per_project2,plot_attainment_per_project3,
                                                          ncol = 3,common.legend = TRUE)


          plot_attainment_per_project_cum<- ggpubr::ggarrange(plot_attainment_per_project_cum1,plot_attainment_per_project_cum2,plot_attainment_per_project_cum3,
                                                              ncol = 3,common.legend = TRUE)

          suppressWarnings(assign("plot_attainment_per_project",plot_attainment_per_project,pos = 1))
          suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))



          plot_attainment_per_project_ggarranged<- ggpubr::ggarrange(plot_attainment_per_project1,plot_attainment_per_project2,plot_attainment_per_project3,
                                                                     plot_attainment_per_project_cum1,plot_attainment_per_project_cum2,plot_attainment_per_project_cum3,
                                                                     ncol = 3,nrow=2,common.legend = TRUE)


          suppressWarnings(assign("plot_attainment_per_project_ggarranged",plot_attainment_per_project_ggarranged,pos = 1))

          #suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
          suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))







          #constraint

          #output_shp_run_lowest1_highest2
          #output_shp_run_highest1_lowest2
          #output_shp_run_balanced
          #
          constraints_name_for_figure<-constraints_pt1_use[as.numeric(all_constraints_names)]

          df_loop_constraints_final <- data.frame()

          for(x in 1:length(constraints_name_for_figure)){
            my_constraint_chosen <- constraints_name_for_figure[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)
            df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_lowest1_highest2,sep="\\"))


            df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen),"Subunit")]
            df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


            colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint","Subunit", "constraint_name")



            df_loop_constraints_x <-data.frame(df_loop_constraints %>%
                                                 group_by(Subunit)%>%
                                                 reframe(treat_constraint_cumulative=cumsum(Treat_constraint),
                                                         total_constraint_cumulative=cumsum(Total_constraint)))

            df_loop_constraints <- df_loop_constraints[order(df_loop_constraints$Subunit),]
            df_loop_constraints$treat_constraint_cumulative <- df_loop_constraints_x$treat_constraint_cumulative
            df_loop_constraints$total_constraint_cumulative <- df_loop_constraints_x$total_constraint_cumulative

            #
            #
            # df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
            # df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

            #get percentage of constraint treated of the total constraint inside the project
            df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


            df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)



          }



          plot_treated_constraint1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name,linetype=factor(Subunit)))+
                                                                           geom_point()+
                                                                           geom_line(linewidth=1)+
                                                                           scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                           xlab("Project number")+
                                                                           ylab("Treated constraint")+
                                                                           labs(color = "Constraint name",linetype="Subunit",tag = "a)")+
                                                                           theme_classic())))





          plot_perc_treated_constraint_in_proj1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name,linetype=factor(Subunit)))+
                                                                                        geom_point()+
                                                                                        geom_line(linewidth=1)+
                                                                                        scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                        xlab("Project number")+
                                                                                        ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                        labs(color = "Constraint name",linetype="Subunit",tag = "a)")+
                                                                                        theme_classic())))







          df_loop_constraints_final <- data.frame()

          for(x in 1:length(constraints_name_for_figure)){
            my_constraint_chosen <- constraints_name_for_figure[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)
            df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_highest1_lowest2,sep="\\"))


            df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen),"Subunit")]
            df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


            colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint","Subunit", "constraint_name")



            df_loop_constraints_x <-data.frame(df_loop_constraints %>%
                                                 group_by(Subunit)%>%
                                                 reframe(treat_constraint_cumulative=cumsum(Treat_constraint),
                                                         total_constraint_cumulative=cumsum(Total_constraint)))

            df_loop_constraints <- df_loop_constraints[order(df_loop_constraints$Subunit),]
            df_loop_constraints$treat_constraint_cumulative <- df_loop_constraints_x$treat_constraint_cumulative
            df_loop_constraints$total_constraint_cumulative <- df_loop_constraints_x$total_constraint_cumulative

            #
            #
            # df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
            # df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

            #get percentage of constraint treated of the total constraint inside the project
            df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


            df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)


          }



          plot_treated_constraint2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name,linetype=factor(Subunit)))+
                                                                           geom_point()+
                                                                           geom_line(linewidth=1)+
                                                                           scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                           xlab("Project number")+
                                                                           ylab("Treated constraint")+
                                                                           labs(color = "Constraint name",linetype="Subunit",tag = "b)")+
                                                                           theme_classic())))





          plot_perc_treated_constraint_in_proj2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name,linetype=factor(Subunit)))+
                                                                                        geom_point()+
                                                                                        geom_line(linewidth=1)+
                                                                                        scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                        xlab("Project number")+
                                                                                        ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                        labs(color = "Constraint name",linetype="Subunit",tag = "b)")+
                                                                                        theme_classic())))






          df_loop_constraints_final <- data.frame()

          for(x in 1:length(constraints_name_for_figure)){
            my_constraint_chosen <- constraints_name_for_figure[x]

            path_with_results_bars <- gsub("/","\\\\",path_with_results)
            df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_balanced,sep="\\"))


            df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen),"Subunit")]
            df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


            colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint","Subunit", "constraint_name")



            df_loop_constraints_x <-data.frame(df_loop_constraints %>%
                                                 group_by(Subunit)%>%
                                                 reframe(treat_constraint_cumulative=cumsum(Treat_constraint),
                                                         total_constraint_cumulative=cumsum(Total_constraint)))

            df_loop_constraints <- df_loop_constraints[order(df_loop_constraints$Subunit),]
            df_loop_constraints$treat_constraint_cumulative <- df_loop_constraints_x$treat_constraint_cumulative
            df_loop_constraints$total_constraint_cumulative <- df_loop_constraints_x$total_constraint_cumulative

            #
            #
            # df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
            # df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

            #get percentage of constraint treated of the total constraint inside the project
            df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


            df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)


          }



          plot_treated_constraint3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name,linetype=factor(Subunit)))+
                                                                           geom_point()+
                                                                           geom_line(linewidth=1)+
                                                                           scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                           xlab("Project number")+
                                                                           ylab("Treated constraint")+
                                                                           labs(color = "Constraint name",linetype="Subunit",tag = "c)")+
                                                                           theme_classic())))





          plot_perc_treated_constraint_in_proj3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name,linetype=factor(Subunit)))+
                                                                                        geom_point()+
                                                                                        geom_line(linewidth=1)+
                                                                                        scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                        xlab("Project number")+
                                                                                        ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                        labs(color = "Constraint name",linetype="Subunit",tag = "c)")+
                                                                                        theme_classic())))



          plot_treated_constraint<- ggpubr::ggarrange(plot_treated_constraint1,plot_treated_constraint2,plot_treated_constraint3,
                                                      ncol = 3,common.legend = TRUE)

          plot_perc_treated_constraint_in_proj<- ggpubr::ggarrange(plot_perc_treated_constraint_in_proj1,plot_perc_treated_constraint_in_proj2,plot_perc_treated_constraint_in_proj3,
                                                                   ncol = 3,common.legend = TRUE)




          plot_treated_constraint_in_proj_ggarrange <- ggpubr::ggarrange(plot_treated_constraint1,plot_treated_constraint2,plot_treated_constraint3,
                                                                         plot_perc_treated_constraint_in_proj1,plot_perc_treated_constraint_in_proj2,plot_perc_treated_constraint_in_proj3,
                                                                         ncol = 3,nrow=2,common.legend = TRUE)


          suppressWarnings(assign("plot_treated_constraint_in_proj_ggarrange",plot_treated_constraint_in_proj_ggarrange,pos = 1))

          #suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
          suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project (top) and Proportion of constraint treated and total constraint inside project (%) per project (bottom). Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))



          #suppressWarnings(assign("plot_treated_constraint",plot_treated_constraint,pos = 1))
          #suppressWarnings(assign("plot_perc_treated_constraint_in_proj",plot_perc_treated_constraint_in_proj,pos = 1))

          #suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))
          #suppressWarnings(assign("caption_fig_constraint2",paste("Proportion of constraint treated and total constraint inside project (%) per project. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))

        }else{

          if(length(number_scenarios_created) > 1){
            all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

            all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
            all_elements_use <- as.character(all_elements_use)

            path_with_results <- paste(all_elements_use, collapse = '/')

            #list patterns
            #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

            last_name <- all_elements[,ncol(all_elements)]
            #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

            output_csv_run = intersect(list.files(path_with_results, ".csv$"), list.files(path_with_results,pattern = last_name))

            #load the _Results
            prj_results <- read.csv(paste(path_with_results,"/",last_name,"_Results.csv",sep=""))

            #names(prj_results)[names(prj_results) == 'ProjectNum'] <- 'ProjectNumber'

            #exclude this file from the output_csv_run
            exclude_this_file_csv <- paste(last_name,"_Results.csv",sep="")

            #prj_results


            #output_shp_run_lowest1_highest2
            #output_shp_run_highest1_lowest2
            #output_shp_run_balanced



            df_loop_effects_final <- data.frame()

            for(x in 1:all_effects){
              my_effect_chosen <- effect_fields[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)


              df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_lowest1_highest2,sep="\\"))

              df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen))]
              df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)





              colnames(df_loop_effects) <- c("ProjectNumber", "effect", "effect_name")

              df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

              df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)

            }




            plot_attainment_per_project1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                                 geom_point()+
                                                                                 geom_line(linewidth=1)+
                                                                                 scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                 xlab("Project number")+
                                                                                 ylab("Objective attainment")+
                                                                                 labs(color = "Effect name",tag = "a)")+
                                                                                 theme_classic())))







            #plot cumulative



            plot_attainment_per_project_cum1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name))+
                                                                                     geom_point()+
                                                                                     geom_line(linewidth=1)+
                                                                                     scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                     xlab("Project number")+
                                                                                     ylab("Cumulative objective attainment")+
                                                                                     labs(color = "Effect name",tag = "a)")+
                                                                                     theme_classic())))






            #output_shp_run_lowest1_highest2
            #output_shp_run_highest1_lowest2
            #output_shp_run_balanced



            df_loop_effects_final <- data.frame()

            for(x in 1:all_effects){
              my_effect_chosen <- effect_fields[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)


              df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_highest1_lowest2,sep="\\"))


              df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen))]
              df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)




              colnames(df_loop_effects) <- c("ProjectNumber", "effect", "effect_name")

              df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

              df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)

            }




            plot_attainment_per_project2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                                 geom_point()+
                                                                                 geom_line(linewidth=1)+
                                                                                 scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                 xlab("Project number")+
                                                                                 ylab("Objective attainment")+
                                                                                 labs(color = "Effect name",tag = "b)")+
                                                                                 theme_classic())))







            #plot cumulative



            plot_attainment_per_project_cum2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name))+
                                                                                     geom_point()+
                                                                                     geom_line(linewidth=1)+
                                                                                     scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                     xlab("Project number")+
                                                                                     ylab("Cumulative objective attainment")+
                                                                                     labs(color = "Effect name",tag = "b)")+
                                                                                     theme_classic())))









            #output_shp_run_lowest1_highest2
            #output_shp_run_highest1_lowest2
            #output_shp_run_balanced



            df_loop_effects_final <- data.frame()

            for(x in 1:all_effects){
              my_effect_chosen <- effect_fields[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)


              df_loop_effects <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_balanced,sep="\\"))


              df_loop_effects <- df_loop_effects[,c("ProjectNumber",paste0("ETrt_",my_effect_chosen))]
              df_loop_effects$effect_name <- paste0("ETrt_",my_effect_chosen)





              colnames(df_loop_effects) <- c("ProjectNumber", "effect", "effect_name")

              df_loop_effects$effect_cumulative <- cumsum(df_loop_effects$effect)

              df_loop_effects_final<-rbind(df_loop_effects_final,df_loop_effects)

            }




            plot_attainment_per_project3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                                 geom_point()+
                                                                                 geom_line(linewidth=1)+
                                                                                 scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                 xlab("Project number")+
                                                                                 ylab("Objective attainment")+
                                                                                 labs(color = "Effect name",tag = "c)")+
                                                                                 theme_classic())))







            #plot cumulative



            plot_attainment_per_project_cum3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect_cumulative,color=effect_name))+
                                                                                     geom_point()+
                                                                                     geom_line(linewidth=1)+
                                                                                     scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                     xlab("Project number")+
                                                                                     ylab("Cumulative objective attainment")+
                                                                                     labs(color = "Effect name",tag = "c)")+
                                                                                     theme_classic())))




            plot_attainment_per_project<- ggpubr::ggarrange(plot_attainment_per_project1,plot_attainment_per_project2,plot_attainment_per_project3,
                                                            ncol = 3,common.legend = TRUE)


            plot_attainment_per_project_cum<- ggpubr::ggarrange(plot_attainment_per_project_cum1,plot_attainment_per_project_cum2,plot_attainment_per_project_cum3,
                                                                ncol = 3,common.legend = TRUE)

            suppressWarnings(assign("plot_attainment_per_project",plot_attainment_per_project,pos = 1))
            suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))



            plot_attainment_per_project_ggarranged<- ggpubr::ggarrange(plot_attainment_per_project1,plot_attainment_per_project2,plot_attainment_per_project3,
                                                                       plot_attainment_per_project_cum1,plot_attainment_per_project_cum2,plot_attainment_per_project_cum3,
                                                                       ncol = 3,nrow=2,common.legend = TRUE)


            suppressWarnings(assign("plot_attainment_per_project_ggarranged",plot_attainment_per_project_ggarranged,pos = 1))

            #suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
            suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))







            #constraint

            #output_shp_run_lowest1_highest2
            #output_shp_run_highest1_lowest2
            #output_shp_run_balanced

            constraints_name_for_figure<-constraints_pt1_use[as.numeric(all_constraints_names)]

            df_loop_constraints_final <- data.frame()

            for(x in 1:length(constraints_name_for_figure)){
              my_constraint_chosen <- constraints_name_for_figure[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)
              df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_lowest1_highest2,sep="\\"))


              df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen))]
              df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


              colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint", "constraint_name")

              df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
              df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

              #get percentage of constraint treated of the total constraint inside the project
              df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


              df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)

            }



            plot_treated_constraint1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name))+
                                                                             geom_point()+
                                                                             geom_line(linewidth=1)+
                                                                             scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                             xlab("Project number")+
                                                                             ylab("Treated constraint")+
                                                                             labs(color = "Constraint name",tag = "a)")+
                                                                             theme_classic())))





            plot_perc_treated_constraint_in_proj1 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name))+
                                                                                          geom_point()+
                                                                                          geom_line(linewidth=1)+
                                                                                          scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                          xlab("Project number")+
                                                                                          ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                          labs(color = "Constraint name",tag = "a)")+
                                                                                          theme_classic())))







            df_loop_constraints_final <- data.frame()

            for(x in 1:length(constraints_name_for_figure)){
              my_constraint_chosen <- constraints_name_for_figure[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)
              df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_highest1_lowest2,sep="\\"))


              df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen))]
              df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


              colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint", "constraint_name")

              df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
              df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

              #get percentage of constraint treated of the total constraint inside the project
              df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


              df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)

            }



            plot_treated_constraint2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name))+
                                                                             geom_point()+
                                                                             geom_line(linewidth=1)+
                                                                             scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                             xlab("Project number")+
                                                                             ylab("Treated constraint")+
                                                                             labs(color = "Constraint name",tag = "b)")+
                                                                             theme_classic())))





            plot_perc_treated_constraint_in_proj2 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name))+
                                                                                          geom_point()+
                                                                                          geom_line(linewidth=1)+
                                                                                          scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                          xlab("Project number")+
                                                                                          ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                          labs(color = "Constraint name",tag = "b)")+
                                                                                          theme_classic())))






            df_loop_constraints_final <- data.frame()

            for(x in 1:length(constraints_name_for_figure)){
              my_constraint_chosen <- constraints_name_for_figure[x]

              path_with_results_bars <- gsub("/","\\\\",path_with_results)
              df_loop_constraints <- subset(prj_results,PointOutputsName==paste(path_with_results_bars,output_shp_run_balanced,sep="\\"))


              df_loop_constraints <- df_loop_constraints[,c("ProjectNumber",paste0("Treat_",my_constraint_chosen),paste0("Total_",my_constraint_chosen))]
              df_loop_constraints$constraint_name <- paste0(my_constraint_chosen)


              colnames(df_loop_constraints) <- c("ProjectNumber", "Treat_constraint","Total_constraint", "constraint_name")

              df_loop_constraints$treat_constraint_cumulative <- cumsum(df_loop_constraints$Treat_constraint)
              df_loop_constraints$total_constraint_cumulative <- cumsum(df_loop_constraints$Total_constraint)

              #get percentage of constraint treated of the total constraint inside the project
              df_loop_constraints$perc_treated_constraint_in_proj <- df_loop_constraints$Treat_constraint/df_loop_constraints$Total_constraint*100


              df_loop_constraints_final<-rbind(df_loop_constraints_final,df_loop_constraints)

            }



            plot_treated_constraint3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=Treat_constraint,color=constraint_name))+
                                                                             geom_point()+
                                                                             geom_line(linewidth=1)+
                                                                             scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                             xlab("Project number")+
                                                                             ylab("Treated constraint")+
                                                                             labs(color = "Constraint name",tag = "c)")+
                                                                             theme_classic())))





            plot_perc_treated_constraint_in_proj3 <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_constraints_final,aes(x=ProjectNumber,y=perc_treated_constraint_in_proj,color=constraint_name))+
                                                                                          geom_point()+
                                                                                          geom_line(linewidth=1)+
                                                                                          scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                          xlab("Project number")+
                                                                                          ylab("Proportion of constraint treated and total constraint inside project (%)")+
                                                                                          labs(color = "Constraint name",tag = "c)")+
                                                                                          theme_classic())))



            plot_treated_constraint<- ggpubr::ggarrange(plot_treated_constraint1,plot_treated_constraint2,plot_treated_constraint3,
                                                        ncol = 3,common.legend = TRUE)

            plot_perc_treated_constraint_in_proj<- ggpubr::ggarrange(plot_perc_treated_constraint_in_proj1,plot_perc_treated_constraint_in_proj2,plot_perc_treated_constraint_in_proj3,
                                                                     ncol = 3,common.legend = TRUE)




            plot_treated_constraint_in_proj_ggarrange <- ggpubr::ggarrange(plot_treated_constraint1,plot_treated_constraint2,plot_treated_constraint3,
                                                                           plot_perc_treated_constraint_in_proj1,plot_perc_treated_constraint_in_proj2,plot_perc_treated_constraint_in_proj3,
                                                                           ncol = 3,nrow=2,common.legend = TRUE)


            suppressWarnings(assign("plot_treated_constraint_in_proj_ggarrange",plot_treated_constraint_in_proj_ggarrange,pos = 1))

            #suppressWarnings(assign("attainment_fig_constraint1",paste("Attainment per project (top) and the cumulative attainment (bottom) for the effects stored"),pos = 1))
            suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project (top) and Proportion of constraint treated and total constraint inside project (%) per project (bottom). Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))



            #suppressWarnings(assign("plot_treated_constraint",plot_treated_constraint,pos = 1))
            #suppressWarnings(assign("plot_perc_treated_constraint_in_proj",plot_perc_treated_constraint_in_proj,pos = 1))

            #suppressWarnings(assign("caption_fig_constraint1",paste("Treated constraint(s) per project. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))
            #suppressWarnings(assign("caption_fig_constraint2",paste("Proportion of constraint treated and total constraint inside project (%) per project. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))

          }}

      }



      #generate report
      if(write_commands==TRUE){
        my_list_input_variable<-get_input_names_long()


        all_matching_set_forsysx_run <- grep("set_forsysx_run", my_list_input_variable$Last.command)
        #select the highest - most recent command

        most_recent_set_forsysx_run <- max(all_matching_set_forsysx_run)

        commands_given_print <- my_list_input_variable$Last.command[most_recent_set_forsysx_run:length(my_list_input_variable$Last.command)]

        suppressWarnings(assign("commands_given_print",commands_given_print,pos = 1))
      }





    }





    if(static==TRUE){


      #plot projects in report

      if(length(outputs_saved_shp)>0){
        if(length(number_scenarios_created) == 1){


          my_plot_projects_report <- my_shp_diss_polygon  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color="black") +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run_diss,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            #geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number')

          suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
          suppressWarnings(assign("caption_fig1","Location of the projects created by ForSysX. Warmer colors represent higher priority",pos = 1))


          #output_shp_run_treat <- subset(output_shp_run,Treat==1)

          my_plot_projects_treat_report <- my_shp_diss_polygon  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color="black") +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run_diss_treat,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            #geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number')

          suppressWarnings(assign("my_plot_projects_treat_report",my_plot_projects_treat_report,pos = 1))
          suppressWarnings(assign("caption_fig2","Location of the stands targeted for treatment per project created by ForSysX. Warmer colors represent higher priority",pos = 1))


        }

      } else {
        my_plot_projects_report <- cat("No projects to show. Did you export the shapefile in ForSysX run?")
        suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
      }







      if(length(outputs_saved_shp)>0){
        if(length(number_scenarios_created) > 1){ #em vez de number_scenarios, quero o numero de cenarios so dos objectivos


          if(class(input_shapefile)[1]=="character"){
            my_shp <- sf::st_read(input_shapefile,quiet=TRUE)
          }

          all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

          all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
          all_elements_use <- as.character(all_elements_use)

          path_with_results <- paste(all_elements_use, collapse = '/')

          #list patterns
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          last_name <- all_elements[,ncol(all_elements)]
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          output_shp_run = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = last_name))

          #select only the scenarios where:
          #the first objective is the lowest and the second the highest
          #the second objective is the lowest and the first the highest
          #the two are 50-50 (or the most similar)

          output_shp_run_only_names <- gsub(paste0(last_name,"_"),"",output_shp_run)

          output_shp_run_only_names_use <- (strsplit(output_shp_run_only_names, "_"))

          only_value_objective_one <- sapply(output_shp_run_only_names_use, "[[", 1)
          only_value_objective_two <- sapply(output_shp_run_only_names_use, "[[", 2)

          #put all the combinations in a table

          objective_values_df_use <- data.frame(cbind(as.numeric(only_value_objective_one),as.numeric(only_value_objective_two)))



          #get the number of objectives used



          #get the lowest first objective with the highest second objective
          min_objective_for_obj1 <- which(objective_values_df_use$X1==min(objective_values_df_use$X1))

          min_objective_for_obj1_in_df <- objective_values_df_use[min_objective_for_obj1,]

          max_objective_for_obj2 <- which(min_objective_for_obj1_in_df$X2==max(min_objective_for_obj1_in_df$X2))

          lowest_obj1_highest_obj2_df <- min_objective_for_obj1_in_df[max_objective_for_obj2,]



          #get the highest first objective with the lowest second objective
          max_objective_for_obj1 <- which(objective_values_df_use$X1==max(objective_values_df_use$X1))

          max_objective_for_obj1_in_df <- objective_values_df_use[max_objective_for_obj1,]

          min_objective_for_obj2 <- which(max_objective_for_obj1_in_df$X2==min(max_objective_for_obj1_in_df$X2))

          highest_obj1_lowest_obj2_df <- max_objective_for_obj1_in_df[min_objective_for_obj2,]



          #get the most balanced weights between the objectives where none of the scenarios is 0
          objective_values_df_use$difference <- abs(objective_values_df_use$X1-objective_values_df_use$X2)

          objective_values_df_use_no0 <- subset(objective_values_df_use,X1 >0 & X2>0)

          most_balanced_non_zero <- which(objective_values_df_use_no0$difference==min(objective_values_df_use_no0$difference))

          most_balanced_non_zero_df <- objective_values_df_use_no0[most_balanced_non_zero,]




          #load the scenarios chosen and then plot them

          output_shp_run_lowest1_highest2 = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,lowest_obj1_highest_obj2_df$X1,lowest_obj1_highest_obj2_df$X2,sep="_")))
          output_shp_run_highest1_lowest2 = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,highest_obj1_lowest_obj2_df$X1,highest_obj1_lowest_obj2_df$X2,sep="_")))
          output_shp_run_balanced = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,most_balanced_non_zero_df$X1,most_balanced_non_zero_df$X2,sep="_")))


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_lowest1_highest2,sep="/"),quiet=TRUE)

          my_shp$diss <- 1



          my_shp$sub_id_package <- 0

          if(subunit_field_exists == TRUE){
            my_shp$sub_id_package <- my_shp[,paste(subunit_field)][[1]]
          }



          my_shp_subunits <- my_shp %>%
            group_by(sub_id_package) %>%
            summarise(m = mean(sub_id_package)) %>%
            sf::st_cast()


          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


          my_plot_projects_report1 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "a)")


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_highest1_lowest2,sep="/"),quiet=TRUE)

          my_shp$diss <- 1

          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


          my_plot_projects_report2 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "b)")


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_balanced,sep="/"),quiet=TRUE)

          my_shp$diss <- 1

          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)



          my_plot_projects_report3 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "c)")



          my_plot_projects_report<- ggpubr::ggarrange(my_plot_projects_report1,my_plot_projects_report2,my_plot_projects_report3,
                                                      ncol = 3,common.legend = TRUE)

          suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
          #suppressWarnings(assign("my_plot_projects_report",list_objectives,pos = 1))
          suppressWarnings(assign("caption_fig1",paste("Location of the projects created by ForSysX. Warmer colors represent higher priority. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))

        }

      }








      suppressWarnings(assign("last_name",last_name,pos = 1)) #,pos = 1

      suppressWarnings(assign("build_interac_report",interactive,pos = 1)) #,pos = 1

      path_for_rmd <- system.file("rmd_template", package = "ForSysXR")
      #setwd(system.file("rmd_template", package = "ForSysXR"))
      save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

      #rmarkdown::render("testing_2.Rmd")





      cat("Generating htlm report",'\n')
      setwd(path_with_results)
      #suppressWarnings(suppressMessages(generate_report()))
      capture.output(suppressWarnings(suppressMessages(generate_report(output_file=paste("report_",last_name,".html",sep="")))))


    }



    if(interactive==TRUE){



      #plot projects in report

      if(length(outputs_saved_shp)>0){
        if(length(number_scenarios_created) == 1){


          output_shp_run_wgs84 <-sf::st_transform(output_shp_run_diss,crs = 4326)
          output_shp_run_treat_wgs84 <-sf::st_transform(output_shp_run_diss_treat,crs = 4326)
          my_shp_diss_line_wgs84 <-sf::st_transform(my_shp_diss_line,crs = 4326)




          #this is required to avoid strange lines in the plot. Not elegant, but working
          #A
          output_shp_run_wgs84<-suppressWarnings(output_shp_run_wgs84%>%
                                                   sf::st_cast("LINESTRING")%>%
                                                   sf::st_cast("POLYGON"))

          #B

          output_shp_run_wgs84 <- suppressMessages(suppressWarnings(output_shp_run_wgs84%>%
                                                                      group_by(ProjectNum) %>%
                                                                      summarise(m = mean(ProjectNum))))

          #C
          output_shp_run_wgs84<-suppressWarnings(output_shp_run_wgs84%>%
                                                   sf::st_cast("MULTIPOLYGON") %>%
                                                   sf::st_cast("POLYGON"))




          #A
          output_shp_run_treat_wgs84<-suppressWarnings(output_shp_run_treat_wgs84%>%
                                                         sf::st_cast("LINESTRING")%>%
                                                         sf::st_cast("POLYGON"))

          #B

          output_shp_run_treat_wgs84 <- suppressMessages(suppressWarnings(output_shp_run_treat_wgs84%>%
                                                                            group_by(ProjectNum) %>%
                                                                            summarise(m = mean(ProjectNum))))

          #C
          output_shp_run_treat_wgs84<-suppressWarnings(output_shp_run_treat_wgs84%>%
                                                         sf::st_cast("MULTIPOLYGON") %>%
                                                         sf::st_cast("POLYGON"))






          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)

          my_breaks <- unique(my_breaks)

          my_breaks<- my_breaks[my_breaks > 0]



          #palFunc_objective_1 <- leaflet::colorNumeric(viridis::turbo(nrow(output_shp_run)),
          #                                             output_shp_run[,paste("ProjectNum")][[1]])



          palFunc_objective_1 <- leaflet::colorNumeric("RdYlBu",
                                                       output_shp_run_diss[,paste("ProjectNum")][[1]])

          cols_obj1=colourvalues::colour_values_rgb(output_shp_run_diss[,paste("ProjectNum")][[1]],  palette = "rdylbu",include_alpha = FALSE)



          palFunc_treat <- leaflet::colorNumeric("RdYlBu",
                                                 output_shp_run_diss_treat[,paste("ProjectNum")][[1]])

          cols_treat=colourvalues::colour_values_rgb(output_shp_run_diss_treat[,paste("ProjectNum")][[1]],  palette = "rdylbu",include_alpha = FALSE)




          #my_shp_wgs84<-st_cast(my_shp_wgs84,"LINESTRING")

          ################################
          projects_plot_leaflet <- leaflet::leaflet() %>%
            leaflet::addProviderTiles('Esri.NatGeoWorldMap', group = "Esri.NatGeoWorldMap") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
            leaflet::addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery") %>%
            #leaflet::addProviderTiles("Esri.WorldImagery", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
            #leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Basemaps") %>% #,leaflet::providerTileOptions(minZoom = 4, maxZoom = 15
            #leaflet::addLayersControl(baseGroups = c('Esri.NatGeoWorldMap',"Esri.WorldImagery","Esri.WorldTopoMap"), position = "topleft")%>%


            #leaflet::addPolygons(data = my_shp_wgs84, fillColor = NA, fillOpacity = 0,
            #                     color = 'black', opacity=1,  weight=1, label=NA) %>%

            leafgl::addGlPolylines(data = my_shp_diss_line_wgs84,
                                   #cols_fill= cols,
                                   #fillColor = "transparent",
                                   #fillOpacity = 0,
                                   #color = NA,
                                   color = "black",
                                   #opacity = 1,
                                   #weight=0.5,
                                   #popup = "ProjectNum",
                                   #label = ~landuse,
                                   weight = 0.5) %>%#,
            #opacity=1,
            #weight=1,
            #label=NA,
            #highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
            #group = "ProjectNum")


            leafgl::addGlPolygons(data = output_shp_run_wgs84,
                                  #cols_fill= cols,
                                  #cols_fill = NA,
                                  fillOpacity = 0.75,
                                  #fillColor = cols_obj1,
                                  color = cols_obj1,
                                  opacity = 1,
                                  #weight=0.5,
                                  #popup = "ProjectNum",
                                  #stroke_colour = NA,
                                  #stroke_width = 0,
                                  #label = ~landuse,
                                  weight = 0,
                                  #opacity=1,
                                  #weight=1,
                                  #label=NA,
                                  highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                                  group = "Projects") %>%
            leaflet::addLegend(data=output_shp_run_wgs84, "topleft",
                               #colors = rgb(cols),
                               pal = palFunc_objective_1,
                               values = ~output_shp_run_wgs84$ProjectNum ,
                               title = "Project number",
                               labels =  my_breaks,
                               #group = "ProjectNum",
                               opacity = 1)%>%


            leafgl::addGlPolygons(data = output_shp_run_treat_wgs84,
                                  #cols_fill= cols,
                                  #cols_fill = NA,
                                  fillOpacity = 0.75,
                                  color = cols_treat,
                                  opacity = 1,
                                  weight=0.5,
                                  popup = "ProjectNum",
                                  #label = ~landuse,
                                  weight = 0.1,
                                  #opacity=1,
                                  #weight=1,
                                  #label=NA,
                                  highlightOptions = leaflet::highlightOptions(weight=2, fillOpacity = 0, opacity=1, color='black'),
                                  group = "Projects - Treat") %>%
            # leaflet::addLegend(data=output_shp_run_treat_wgs84, "topleft",
            #                    #colors = rgb(cols),
            #                    pal = palFunc_treat,
            #                    values = ~output_shp_run_treat_wgs84$ProjectNum ,
            #                    title = "Project number",
            #                    #labels = c("No", "Yes"),
            #                    group = "ProjectNum_treat",
            #                    opacity = 1)%>%

            leaflet::addLayersControl(baseGroups = c("Esri.NatGeoWorldMap", "ESRI World Imagery"),
                                      position = "topright",
                                      overlayGroups = c("Projects","Projects - Treat"),
                                      options = leaflet::layersControlOptions(collapsed = F))%>%
            leaflet::hideGroup(c("Projects","Projects - Treat"))%>%
            leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)


          #
          # projects_plot_leaflet <- projects_plot_leaflet %>%
          #   leaflet::addLayersControl(overlayGroups = "ProjectNum",
          #                             options = leaflet::layersControlOptions(collapsed = F)) %>%
          #   leaflet::hideGroup("ProjectNum")
          #



          suppressWarnings(assign("projects_plot_leaflet",projects_plot_leaflet,pos = 1))


          #suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
          suppressWarnings(assign("caption_fig1","Location of the projects created by ForSysX. Warmer colors represent higher priority",pos = 1))



        }

      } else {
        my_plot_projects_report <- cat("No projects to show. Did you export the shapefile in ForSysX run?")
        suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
      }







      if(length(outputs_saved_shp)>0){
        if(length(number_scenarios_created) > 1){ #em vez de number_scenarios, quero o numero de cenarios so dos objectivos


          if(class(input_shapefile)[1]=="character"){
            my_shp <- sf::st_read(input_shapefile,quiet=TRUE)
          }

          all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

          all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
          all_elements_use <- as.character(all_elements_use)

          path_with_results <- paste(all_elements_use, collapse = '/')

          #list patterns
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          last_name <- all_elements[,ncol(all_elements)]
          #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

          output_shp_run = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = last_name))

          #select only the scenarios where:
          #the first objective is the lowest and the second the highest
          #the second objective is the lowest and the first the highest
          #the two are 50-50 (or the most similar)

          output_shp_run_only_names <- gsub(paste0(last_name,"_"),"",output_shp_run)

          output_shp_run_only_names_use <- (strsplit(output_shp_run_only_names, "_"))

          only_value_objective_one <- sapply(output_shp_run_only_names_use, "[[", 1)
          only_value_objective_two <- sapply(output_shp_run_only_names_use, "[[", 2)

          #put all the combinations in a table

          objective_values_df_use <- data.frame(cbind(as.numeric(only_value_objective_one),as.numeric(only_value_objective_two)))



          #get the number of objectives used



          #get the lowest first objective with the highest second objective
          min_objective_for_obj1 <- which(objective_values_df_use$X1==min(objective_values_df_use$X1))

          min_objective_for_obj1_in_df <- objective_values_df_use[min_objective_for_obj1,]

          max_objective_for_obj2 <- which(min_objective_for_obj1_in_df$X2==max(min_objective_for_obj1_in_df$X2))

          lowest_obj1_highest_obj2_df <- min_objective_for_obj1_in_df[max_objective_for_obj2,]



          #get the highest first objective with the lowest second objective
          max_objective_for_obj1 <- which(objective_values_df_use$X1==max(objective_values_df_use$X1))

          max_objective_for_obj1_in_df <- objective_values_df_use[max_objective_for_obj1,]

          min_objective_for_obj2 <- which(max_objective_for_obj1_in_df$X2==min(max_objective_for_obj1_in_df$X2))

          highest_obj1_lowest_obj2_df <- max_objective_for_obj1_in_df[min_objective_for_obj2,]



          #get the most balanced weights between the objectives where none of the scenarios is 0
          objective_values_df_use$difference <- abs(objective_values_df_use$X1-objective_values_df_use$X2)

          objective_values_df_use_no0 <- subset(objective_values_df_use,X1 >0 & X2>0)

          most_balanced_non_zero <- which(objective_values_df_use_no0$difference==min(objective_values_df_use_no0$difference))

          most_balanced_non_zero_df <- objective_values_df_use_no0[most_balanced_non_zero,]




          #load the scenarios chosen and then plot them

          output_shp_run_lowest1_highest2 = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,lowest_obj1_highest_obj2_df$X1,lowest_obj1_highest_obj2_df$X2,sep="_")))
          output_shp_run_highest1_lowest2 = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,highest_obj1_lowest_obj2_df$X1,highest_obj1_lowest_obj2_df$X2,sep="_")))
          output_shp_run_balanced = intersect(list.files(path_with_results,pattern = ".shp$"), list.files(path_with_results,pattern = paste(last_name,most_balanced_non_zero_df$X1,most_balanced_non_zero_df$X2,sep="_")))


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_lowest1_highest2,sep="/"),quiet=TRUE)

          my_shp$diss <- 1



          my_shp$sub_id_package <- 0

          if(subunit_field_exists == TRUE){
            my_shp$sub_id_package <- my_shp[,paste(subunit_field)][[1]]
          }



          my_shp_subunits <- my_shp %>%
            group_by(sub_id_package) %>%
            summarise(m = mean(sub_id_package)) %>%
            sf::st_cast()


          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


          my_plot_projects_report1 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "a)")


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_highest1_lowest2,sep="/"),quiet=TRUE)

          my_shp$diss <- 1

          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


          my_plot_projects_report2 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "b)")


          output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run_balanced,sep="/"),quiet=TRUE)

          my_shp$diss <- 1

          my_breaks<-max(output_shp_run$ProjectNum)/4

          my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)



          my_plot_projects_report3 <- my_shp  %>%
            #mutate_at(c('diss'), ~na_if(., 0)) %>%
            #st_combine() %>%
            ggplot2::ggplot() +
            geom_sf(aes(fill=diss),fill="grey",color=NA) +
            #ggtitle("Projects ranking") +
            theme_void()+
            theme(plot.title=element_text(hjust=0.5))+
            #guides(fill="none")+
            geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
            scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
            geom_sf(data=my_shp_subunits,color="black",fill=NA)+
            labs(fill='Project number',tag = "c)")



          my_plot_projects_report<- ggpubr::ggarrange(my_plot_projects_report1,my_plot_projects_report2,my_plot_projects_report3,
                                                      ncol = 3,common.legend = TRUE)

          suppressWarnings(assign("my_plot_projects_report",my_plot_projects_report,pos = 1))
          #suppressWarnings(assign("my_plot_projects_report",list_objectives,pos = 1))
          suppressWarnings(assign("caption_fig1",paste("Location of the projects created by ForSysX. Warmer colors represent higher priority. Panel a) represent the run with lowest weight for ",list_objectives[1,1], " and highest weight for ",list_objectives[2,1],"; panel b) represent the run with highest weight for ",list_objectives[1,1]," and lowest weight for ",list_objectives[2,1],"; panel c) represent the most weight-balanced run between the two objectives"),pos = 1))

        }

      }





      suppressWarnings(assign("last_name",last_name,pos = 1)) #,pos = 1

      path_for_rmd <- system.file("rmd_template", package = "ForSysXR")
      #setwd(system.file("rmd_template", package = "ForSysXR"))
      save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

      #rmarkdown::render("testing_2.Rmd")





      cat("Generating interactive htlm report",'\n')
      setwd(path_with_results)
      #suppressWarnings(suppressMessages(generate_report()))
      capture.output(suppressWarnings(suppressMessages(generate_report_interac(output_file=paste("report_",last_name,".html",sep="")))))

      #clean-up unecessary files
      # file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))
      # suppressWarnings(rm(list = ls()[grep("commands_given_print", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("last_name", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("plot_attainment_per_project", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("plot_attainment_per_project_cum", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("plot_perc_treated_constraint_in_proj", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("plot_treated_constraint", ls())], envir = globalenv()))
      #
      # suppressWarnings(rm(list = ls()[grep("introduction_table", ls())], envir = globalenv()))
      # suppressWarnings(rm(list = ls()[grep("my_plot_projects_report", ls())], envir = globalenv()))
      #
      # (rm(list = ls()[grep("attainment_fig_constraint1", ls())], envir = globalenv()))
      # (rm(list = ls()[grep("caption_fig_constraint1", ls())], envir = globalenv()))
      # (rm(list = ls()[grep("caption_fig1", ls())], envir = globalenv()))




      #rm(commands_given_print)


      #AQUI REMOVER OS STANDS E PROJS!
      suppressWarnings(rm(list = ls()[grep("prj_results", ls())], envir = globalenv()))
      #rm(list = ls()[grep("last_name", ls())], envir = globalenv())


    }



    file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))


}
