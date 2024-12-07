#' Evaluates if the stand shapefile fields are in the correct format to be used in ForSysXR. Optionally, this function can convert the indicated fields to the correct format.
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param area Field from input_shapefile containing the area of each stand
#' @param available Optional. Field from input_shapefile containing the available stands
#' @param exclude_field Optional. Field from input_shapefile identifying the stands to be excluded
#' @param subunit_field Optional. Field from input_shapefile identifying the pre-defined planning areas
#' @param master_subunit_field Optional. Field from input_shapefile identifying the master subunits.
#' @param make_valid Logical. If TRUE, then the input shapefile fields will be updated (if needed) to the correct format. This overwrites the existing shapefile. Default is TRUE
#'
#' @return
#' @export
#'
#' @examples
check_input_shapefile <- function(input_shapefile,
                                  stand_id,
                                  area,
                                  available,
                                  exclude_field,
                                  subunit_field,
                                  master_subunit_field,
                                  make_valid=TRUE){


  if(class(input_shapefile)[1]=="character"){
    input_shapefile_format <- substrRight(input_shapefile,4)
    if(input_shapefile_format!= ".shp"){
      stop("input_shapefile has to be a shapefile!")}


    # if(missing(stand_id)){
    #   stop("The mandatory stand id field is missing. Please specify it as stand_id = 'stand_id_field'")}



    cat("Loading stand shapefile\n")
    my_shp <- sf::st_read(input_shapefile,quiet = TRUE)



    fields_changed <- c()
    issues_found <- c()

    #stand_id
    stand_id_index <- which(colnames(my_shp)==stand_id)

    stand_id_data <- data.frame(my_shp[,stand_id_index])

    #my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])

    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }


    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == FALSE){
      issues_found <- c(issues_found,"The stand id field is not an integer")
    }


    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == FALSE){
      issues_found <- c(issues_found,"The stand id field requires an unique id for each stand, but non-unique ids were found")
    }

    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }



    #area
    if(!missing(area)){
      area_index <- which(colnames(my_shp)==area)

      area_data <- data.frame(my_shp[,area_index])

      if(is.numeric(area_data[,1])==FALSE & is.integer(area_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The area field is not numeric")
      }

      if(is.numeric(area_data[,1])==FALSE & is.integer(area_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,area_index]<-as.numeric(area_data[,1])
        fields_changed<-c(fields_changed,area)
      }}



    #exclude
    if(!missing(exclude_field)){
      exclude_field_index <- which(colnames(my_shp)==exclude_field)

      exclude_field_data <- data.frame(my_shp[,exclude_field_index])

      #my_shp[,exclude_field_index]<-as.integer(exclude_field_data[,1])

      if(is.integer(exclude_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The exclude field is not an integer")
      }

      if(is.integer(exclude_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,exclude_field_index]<-as.integer(exclude_field_data[,1])
        fields_changed<-c(fields_changed,exclude_field)
      }}



    #available
    if(!missing(available)){
      available_index <- which(colnames(my_shp)==available)

      available_data <- data.frame(my_shp[,available_index])

      #my_shp[,available_index]<-as.integer(available_data[,1])

      if(is.integer(available_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The available field is not an integer")
      }

      if(is.integer(available_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,available_index]<-as.integer(available_data[,1])
        fields_changed<-c(fields_changed,available)
      }
    }





    #subunit_field
    if(!missing(subunit_field)){
      subunit_field_index <- which(colnames(my_shp)==subunit_field)

      subunit_field_data <- data.frame(my_shp[,subunit_field_index])

      #my_shp[,subunit_field_index]<-as.integer(subunit_field_data[,1])

      if(is.integer(subunit_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The subunit field is not an integer")
      }

      if(is.integer(subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,subunit_field_index]<-as.integer(subunit_field_data[,1])
        fields_changed<-c(fields_changed,subunit_field)
      }}




    #master_subunit_field
    if(!missing(master_subunit_field)){
      master_subunit_field_index <- which(colnames(my_shp)==master_subunit_field)

      master_subunit_field_data <- data.frame(my_shp[,master_subunit_field_index])

      #my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The master subunit field is not an integer")
      }

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])
        fields_changed<-c(fields_changed,master_subunit_field)
      }
    }


    # if(make_valid == TRUE & length(fields_changed)>=1){
    #   cat("Writing stand shapefile \n")
    #   sf::st_write(my_shp,input_shapefile,append = FALSE,quiet = TRUE)
    #   cat("The input shapefile was updated. The following fields were updated to match ForSysX required formats: \n",noquote(fields_changed))
    # }



    #not allow long name values

    #check the length of characters attributes. Longer than 10 cause a problem
    charecters_in_table  <-  my_shp[, sapply(my_shp, class) == 'character']
    charecters_in_table <- charecters_in_table[,1:ncol(charecters_in_table)-1]

    if(make_valid == TRUE & ncol(charecters_in_table)>0){


      fields_characts<-noquote(colnames(charecters_in_table))

      my_shp_no_geo <- st_drop_geometry(my_shp)

      for(q in 1:(ncol(charecters_in_table)-1)){

        unique_vals_char <- unique(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])])

        if(max(nchar(unique_vals_char))>10){

          new_names <- abbreviate(unique_vals_char,10, dot = "FALSE", strict = "TRUE")
          test <- abbreviate(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])],10, dot = "FALSE", strict = "TRUE",named=FALSE)

          my_shp[,which( colnames(my_shp)==fields_characts[q])] <- test

          fields_changed_name<-paste("Field ",fields_characts[q]," had values with more than 10 characters. These values were abbreviated.",sep="")
          fields_changed<-c(fields_changed,fields_changed_name)


        }

      }

    }





    if(make_valid == FALSE & ncol(charecters_in_table)>0){


      fields_characts<-noquote(colnames(charecters_in_table))

      my_shp_no_geo <- st_drop_geometry(my_shp)

      for(q in 1:(ncol(charecters_in_table)-1)){

        unique_vals_char <- unique(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])])

        if(max(nchar(unique_vals_char))>10){

          issues_found <- c(issues_found,paste("Field ",fields_characts[q], " has values with more than 10 characters.", sep=""))

        }

      }

    }







    if(make_valid == TRUE & length(fields_changed)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("All fields match ForSysX required formats. The input shapefile was not updated.")
    }



    if(make_valid == FALSE & length(issues_found)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("All fields match ForSysX required formats. No changes are required.")
    }


    if(make_valid == FALSE & length(issues_found)>=1){
      all_issues_found<-c()
      for(e in 1:length(issues_found)){
        all_issues_found<-c(all_issues_found,(paste("•",noquote(issues_found)[e], "\n", sep=" ")))
      }
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("Changes in the input shapefiles are required. \nRe-run check_input_shapefile with make_valid = TRUE if you wish to correct and overwrite the current shapefile in the local machine (does not change the variable in global environment). \nThe problems found with the shapefile are listed below: \n",
          noquote(all_issues_found))

    }




    if(make_valid == TRUE & length(fields_changed)>=1){
      cat("Writing stand shapefile \n")
      sf::st_write(my_shp,input_shapefile,append = FALSE,quiet = TRUE)

      all_fields_changed<-c()
      for(e in 1:length(fields_changed)){
        all_fields_changed<-c(all_fields_changed,(paste("•",noquote(fields_changed)[e], "\n", sep=" ")))
      }
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("The input shapefile in the local machine was overwritten. The following fields were updated to match ForSysX required formats: \n",
          noquote(all_fields_changed))

      #cat("The input shapefile was updated. The following fields were updated to match ForSysX required formats: \n",noquote(fields_changed))
    }




  }




  if(class(input_shapefile)[1]=="sf"){


    my_list_input_variable<-get_input_names()
    result <- stringr::str_subset(my_list_input_variable$Last.command, "input_shapefile")
    class(result)

    result1 <- gsub(".*input_shapefile", "", result)
    result2 <- gsub(",.*", "", result1)
    result3 <- gsub("=", "", result2)
    result3 <- gsub("\\(", "", result3)
    result4 <- gsub(" ", "", result3)




    fields_changed <- c()
    issues_found <- c()

    my_shp <- input_shapefile

    #stand_id
    stand_id_index <- which(colnames(my_shp)==stand_id)

    stand_id_data <- data.frame(my_shp[,stand_id_index])

    #my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])

    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }


    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == FALSE){
      issues_found <- c(issues_found,"The stand id field is not an integer")
    }


    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == FALSE){
      issues_found <- c(issues_found,"The stand id field requires an unique id for each stand, but non-unique ids were found")
    }

    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }



    #area
    if(!missing(area)){
      area_index <- which(colnames(my_shp)==area)

      area_data <- data.frame(my_shp[,area_index])

      if(is.numeric(area_data[,1])==FALSE & is.integer(area_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The area field is not numeric")
      }

      if(is.numeric(area_data[,1])==FALSE & is.integer(area_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,area_index]<-as.numeric(area_data[,1])
        fields_changed<-c(fields_changed,area)
      }}



    #exclude
    if(!missing(exclude_field)){
      exclude_field_index <- which(colnames(my_shp)==exclude_field)

      exclude_field_data <- data.frame(my_shp[,exclude_field_index])

      #my_shp[,exclude_field_index]<-as.integer(exclude_field_data[,1])

      if(is.integer(exclude_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The exclude field is not an integer")
      }

      if(is.integer(exclude_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,exclude_field_index]<-as.integer(exclude_field_data[,1])
        fields_changed<-c(fields_changed,exclude_field)
      }}



    #available
    if(!missing(available)){
      available_index <- which(colnames(my_shp)==available)

      available_data <- data.frame(my_shp[,available_index])

      #my_shp[,available_index]<-as.integer(available_data[,1])

      if(is.integer(available_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The available field is not an integer")
      }

      if(is.integer(available_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,available_index]<-as.integer(available_data[,1])
        fields_changed<-c(fields_changed,available)
      }
    }





    #subunit_field
    if(!missing(subunit_field)){
      subunit_field_index <- which(colnames(my_shp)==subunit_field)

      subunit_field_data <- data.frame(my_shp[,subunit_field_index])

      #my_shp[,subunit_field_index]<-as.integer(subunit_field_data[,1])

      if(is.integer(subunit_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The subunit field is not an integer")
      }

      if(is.integer(subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,subunit_field_index]<-as.integer(subunit_field_data[,1])
        fields_changed<-c(fields_changed,subunit_field)
      }}




    #master_subunit_field
    if(!missing(master_subunit_field)){
      master_subunit_field_index <- which(colnames(my_shp)==master_subunit_field)

      master_subunit_field_data <- data.frame(my_shp[,master_subunit_field_index])

      #my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == FALSE){
        issues_found <- c(issues_found,"The master subunit field is not an integer")
      }

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])
        fields_changed<-c(fields_changed,master_subunit_field)
      }
    }





    #check if fields have less than 10 characters in the name
    all_names_shp <- names(my_shp)

    if(max(nchar(all_names_shp))>10){

      original_names_shapefile <- names(my_shp)
      names_needing_change <- which(nchar(all_names_shp)>10)


      for (b in names_needing_change){
        all_names_shp[[b]] <- abbreviate(all_names_shp[[b]],10, dot = "FALSE", strict = "TRUE")
      }




      #identify the difference
      old_names_replaced <- setdiff(original_names_shapefile,all_names_shp)
      new_names_to_replace <- setdiff(all_names_shp,original_names_shapefile)


      if(length(old_names_replaced)>=1 & make_valid == FALSE){
        issues_found <- c(issues_found,paste("The fields with the names ",paste0(old_names_replaced, collapse=", ")," have more than 10 characters.", sep=""))
      }


      if(length(old_names_replaced)>=1 & make_valid == TRUE){
        names(my_shp)<- all_names_shp
        fields_changed_name <- paste("Fields with a long name (more than 10 characters) were changed:",paste(old_names_replaced,new_names_to_replace,sep = " was changed to ", collapse = ", "))
        fields_changed<-c(fields_changed,fields_changed_name)
      }




      #change the input values given by the user to make sure we use the abbreviated names instead of the ones that will cause forsys to fail


    }



    #check if all fields are unique
    if(any(duplicated(names(my_shp)))==TRUE & make_valid == FALSE){
      issues_found <- c(issues_found,paste("Fields with the same name detected.", sep=""))
      }



    if(any(duplicated(names(my_shp)))==TRUE & make_valid == TRUE){
      names(my_shp)<- all_names_shp
      duplicated_names <- paste("Fields with the same name detected, but could not automatically change. Manual change is required", sep="")
      warning(duplicated_names)
    }





###set max number of characters in shapefile - not used at the moment####
    #check the length of characters attributes. Longer than 10 cause a problem
    # charecters_in_table  <-  my_shp[, sapply(my_shp, class) == 'character']
    # charecters_in_table <- charecters_in_table[,1:ncol(charecters_in_table)-1]
    #
    # if(make_valid == TRUE & ncol(charecters_in_table)>0){
    #
    #
    #     fields_characts<-noquote(colnames(charecters_in_table))
    #
    #     my_shp_no_geo <- st_drop_geometry(my_shp)
    #
    #     for(q in 1:(ncol(charecters_in_table)-1)){
    #
    #       unique_vals_char <- unique(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])])
    #
    #       if(max(nchar(unique_vals_char))>10){
    #
    #         new_names <- abbreviate(unique_vals_char,10, dot = "FALSE", strict = "TRUE")
    #         test <- abbreviate(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])],10, dot = "FALSE", strict = "TRUE",named=FALSE)
    #
    #         my_shp[,which( colnames(my_shp)==fields_characts[q])] <- test
    #
    #         fields_changed_name<-paste("Field ",fields_characts[q]," had values with more than 10 characters. These values were abbreviated.",sep="")
    #         fields_changed<-c(fields_changed,fields_changed_name)
    #
    #
    #     }
    #
    #   }
    #
    # }


#
#
#
#     if(make_valid == FALSE & ncol(charecters_in_table)>0){
#
#
#       fields_characts<-noquote(colnames(charecters_in_table))
#
#       my_shp_no_geo <- st_drop_geometry(my_shp)
#
#       for(q in 1:(ncol(charecters_in_table)-1)){
#
#         unique_vals_char <- unique(my_shp_no_geo[,which( colnames(my_shp_no_geo)==fields_characts[q])])
#
#         if(max(nchar(unique_vals_char))>10){
#
#           issues_found <- c(issues_found,paste("Field ",fields_characts[q], " has values with more than 10 characters.", sep=""))
#
#         }
#
#       }
#
#     }






    if(make_valid == TRUE & length(fields_changed)>=1){
      #input_shapefile <- my_shp

      #return(input_shapefile)
      suppressWarnings(assign(result4,my_shp,pos = 1))
      #list2env(input_shapefile, envir = .GlobalEnv)

      all_fields_changed<-c()
      for(e in 1:length(fields_changed)){
        all_fields_changed<-c(all_fields_changed,(paste("•",noquote(fields_changed)[e], "\n", sep=" ")))
      }
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("The input shapefile in the global environment was updated. The following fields were updated to match ForSysX required formats: \n",
          noquote(all_fields_changed))

      #cat("The input shapefile was updated. The following fields were updated to match ForSysX required formats: \n",noquote(fields_changed))
    }








    if(make_valid == TRUE & length(fields_changed)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("All fields match ForSysX required formats. The input shapefile was not updated.")
    }


    if(make_valid == FALSE & length(issues_found)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("All fields match ForSysX required formats. No changes are required.")
    }



    if(make_valid == FALSE & length(issues_found)>=1){
      all_issues_found<-c()
      for(e in 1:length(issues_found)){
        all_issues_found<-c(all_issues_found,(paste("•",noquote(issues_found)[e], "\n", sep=" ")))
      }
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("Changes in the input shapefiles are required. \nRe-run check_input_shapefile with make_valid = TRUE if you wish to correct and overwrite the current shapefile in the global environment (does not change the shapefile in local machine). \nThe problems found with the shapefile are listed below: \n",
          noquote(all_issues_found))

    }








  }


}
