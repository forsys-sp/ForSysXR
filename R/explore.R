#' Title
#'
#' @param input_shapefile
#' @param outputs_base_name
#' @param stand_id
#' @param area
#' @param available
#' @param exclude_field
#' @param patchbuster_identifier
#' @param patchbuster_weight
#' @param constraints
#' @param constraints_logic
#' @param effect_fields
#' @param objectives
#' @param threshold
#' @param threshold_logic
#' @param subunit_field
#' @param master_subunit
#'
#' @return
#' @export
#'
#' @examples
set_forsysx_run <- function(input_shapefile,
                            outputs_base_name,
                            stand_id,
                            area,
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
                            patchbuster_identifier,
                            patchbuster_weight,
                            #constraints_name,
                            #constraints_value,
                            #constraints_slack ="",
                            constraints,
                            constraints_logic = c("single_value","and"),
                            effect_fields,
                            objectives,
                            threshold,
                            threshold_logic = c("single_value","and"),
                            subunit_field,
                            master_subunit
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



  #threshold
  #first plot individual thresholds

  #get the number of thresholds

  if(!missing(threshold) & missing(threshold_logic)){
    if(length(threshold) !=3){
      stop("Wrong number of elements used in threshold (expected 3 elements). If using multiple thresholds, please specify the threshold_logic.")
    }
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


    threshold_plot_1
    threshold_plot_2



    #count how many thresholds we have in the environment. Then set the number of cols and rows for ggarrange




    #delete the unused threshold

    #if multiple thresholds used, then we need to get the thresholdLogic
    #threshold_logic = c("single_value","and")



    if(!missing(threshold_logic)){


      if(length(threshold_logic)!=2){
        stop("threshold_logic has to have two elements. The first has to be single_value or multiple_value, and the second and or or.")
      }

      if(threshold_logic[1]!="single_value" & threshold_logic[1]!="multiple_value"){
        stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(threshold_logic[2]!="and" & threshold_logic[2]!="or"){
        stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(threshold_logic[2]=="and"){
        threshold_logic_val <- 0
      }

      if(threshold_logic[2]=="or"){
        threshold_logic_val <- 1
      }

      xml_data_use <- gsub("ThresholdLogic=\"0\"", paste("ThresholdLogic=\"",threshold_logic_val,"\"", sep=""),unlist(xml_data_use))


      if(threshold_logic[1]=="single_value"){
        threshold_logic_single <- 1
      }


      if(threshold_logic[1]=="multiple_value"){
        threshold_logic_single <- 0
      }

      xml_data_use <- gsub("ThresholdSingleValue=\"1\"", paste("ThresholdSingleValue=\"",threshold_logic_single,"\"", sep=""),unlist(xml_data_use))


    }}







  if(!missing(threshold) & !missing(threshold_logic)){

    if(threshold_logic[1]=="multiple_value"){

      total_n_threshold <- base::length(threshold)
      total_n_threshold <- total_n_threshold/5

      #integer_val <- total_n_threshold%%1==0

      integer_val <- decimalplaces(total_n_threshold)

      if(integer_val != 0)
        stop("Wrong number of arguments when defining the threshold")

      if(total_n_threshold > 6)
        stop("Maximum number of threshold reached. Maximum number allowd is 6")

      for (k in 1:total_n_threshold){

        position_threshold <- (k-1)*5
        xml_data_use <- gsub(paste("my_threshold",k,"\"",sep=""),paste(threshold[position_threshold+1],"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_operator",k,sep=""),
                             if(threshold[position_threshold+2]==">="){"&gt;="} else
                               if(threshold[position_threshold+2]=="<="){"&lt;="} else
                                 if(threshold[position_threshold+2]=="<"){"&lt;"} else
                                   if(threshold[position_threshold+2]==">"){"&gt;"} else
                                     if(threshold[position_threshold+2]=="=="){"=="},
                             unlist(xml_data_use))
        xml_data_use <- gsub(paste("min_val_threshold",k,sep=""),threshold[position_threshold+3],unlist(xml_data_use))
        xml_data_use <- gsub(paste("max_val_threshold",k,sep=""),threshold[position_threshold+4],unlist(xml_data_use))

        #if we only want to run one threshold and not steps, we do this
        xml_data_use <- gsub(paste("step_threshold",k,sep=""),threshold[position_threshold+5],unlist(xml_data_use))

      }


      #delete the unused threshold
      if(total_n_threshold < 6){
        diff_threshold <- 6-total_n_threshold
        position_unused <- (1:6)
        position_unused <- tail(position_unused,diff_threshold)

        for(q in min(position_unused):max(position_unused)){
          xml_data_use <- gsub(paste("<Threshold Field=\"my_threshold",q,"\""," Operator=\"my_operator",q,"\""," Value=\"1.00\" MinValue=\"min_val_threshold",q,"\""," MaxValue=\"max_val_threshold",q,"\" Step=\"step_threshold",q,"\" />",sep=""),"",unlist(xml_data_use))
        }
      }


      #if multiple thresholds used, then we need to get the thresholdLogic
      #threshold_logic = c("single_value","and")



      if(!missing(threshold_logic)){


        if(length(threshold_logic)!=2){
          stop("threshold_logic has to have two elements. The first has to be single_value or multiple_value, and the second and or or.")
        }

        if(threshold_logic[1]!="single_value" & threshold_logic[1]!="multiple_value"){
          stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
        }

        if(threshold_logic[2]!="and" & threshold_logic[2]!="or"){
          stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
        }

        if(threshold_logic[2]=="and"){
          threshold_logic_val <- 0
        }

        if(threshold_logic[2]=="or"){
          threshold_logic_val <- 1
        }

        xml_data_use <- gsub("ThresholdLogic=\"0\"", paste("ThresholdLogic=\"",threshold_logic_val,"\"", sep=""),unlist(xml_data_use))


        if(threshold_logic[1]=="single_value"){
          threshold_logic_single <- 1
        }


        if(threshold_logic[1]=="multiple_value"){
          threshold_logic_single <- 0
        }

        xml_data_use <- gsub("ThresholdSingleValue=\"1\"", paste("ThresholdSingleValue=\"",threshold_logic_single,"\"", sep=""),unlist(xml_data_use))


      }}


    if(threshold_logic[1]=="single_value"){
      total_n_threshold <- base::length(threshold)
      total_n_threshold <- total_n_threshold/3

      #integer_val <- total_n_threshold%%1==0

      integer_val <- decimalplaces(total_n_threshold)

      if(integer_val != 0)
        stop("Wrong number of arguments when defining the threshold")

      if(total_n_threshold > 6)
        stop("Maximum number of threshold reached. Maximum number allowd is 6")

      for (k in 1:total_n_threshold){

        position_threshold <- (k-1)*3
        xml_data_use <- gsub(paste("my_threshold",k,"\"",sep=""),paste(threshold[position_threshold+1],"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_operator",k,sep=""),
                             if(threshold[position_threshold+2]==">="){"&gt;="} else
                               if(threshold[position_threshold+2]=="<="){"&lt;="} else
                                 if(threshold[position_threshold+2]=="<"){"&lt;"} else
                                   if(threshold[position_threshold+2]==">"){"&gt;"} else
                                     if(threshold[position_threshold+2]=="=="){"=="},
                             unlist(xml_data_use))
        xml_data_use <- gsub(paste("min_val_threshold",k,sep=""),threshold[position_threshold+3],unlist(xml_data_use))

        #if we only want to run one threshold and not steps, we do this
        xml_data_use <- gsub(paste("step_threshold",k,sep=""),"0",unlist(xml_data_use))

      }


      #delete the unused threshold
      if(total_n_threshold < 6){
        diff_threshold <- 6-total_n_threshold
        position_unused <- (1:6)
        position_unused <- tail(position_unused,diff_threshold)

        for(q in min(position_unused):max(position_unused)){
          xml_data_use <- gsub(paste("<Threshold Field=\"my_threshold",q,"\""," Operator=\"my_operator",q,"\""," Value=\"1.00\" MinValue=\"min_val_threshold",q,"\""," MaxValue=\"max_val_threshold",q,"\" Step=\"step_threshold",q,"\" />",sep=""),"",unlist(xml_data_use))
        }
      }


      #if multiple thresholds used, then we need to get the thresholdLogic
      #threshold_logic = c("single_value","and")



      if(!missing(threshold_logic)){


        if(length(threshold_logic)!=2){
          stop("threshold_logic has to have two elements. The first has to be single_value or multiple_value, and the second and or or.")
        }

        if(threshold_logic[1]!="single_value" & threshold_logic[1]!="multiple_value"){
          stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
        }

        if(threshold_logic[2]!="and" & threshold_logic[2]!="or"){
          stop("threshold_logic has to be single_value or multiple_value followed by and or or.")
        }

        if(threshold_logic[2]=="and"){
          threshold_logic_val <- 0
        }

        if(threshold_logic[2]=="or"){
          threshold_logic_val <- 1
        }

        xml_data_use <- gsub("ThresholdLogic=\"0\"", paste("ThresholdLogic=\"",threshold_logic_val,"\"", sep=""),unlist(xml_data_use))


        if(threshold_logic[1]=="single_value"){
          threshold_logic_single <- 1
        }


        if(threshold_logic[1]=="multiple_value"){
          threshold_logic_single <- 0
        }

        xml_data_use <- gsub("ThresholdSingleValue=\"1\"", paste("ThresholdSingleValue=\"",threshold_logic_single,"\"", sep=""),unlist(xml_data_use))


      }
    }



  }









  #then plot the combination of what we can treat after the availability and the combination of all thresholds

}
