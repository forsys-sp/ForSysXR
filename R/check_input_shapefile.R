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

    cat("Loading stand shapefile\n")
    my_shp <- sf::st_read(input_shapefile,quiet = TRUE)



    fields_changed <- c()

    #stand_id
    stand_id_index <- which(colnames(my_shp)==stand_id)

    stand_id_data <- data.frame(my_shp[,stand_id_index])

    #my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])

    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }


    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == FALSE){
      stop("The stand id field is not unique for each stand")
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
        stop("The area field is not numeric")
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
        stop("The exclude field is not an integer")
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
        stop("The available field is not an integer")
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
        stop("The subunit field is not an integer")
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
        stop("The master subunit field is not an integer")
      }

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])
        fields_changed<-c(fields_changed,master_subunit_field)
      }
    }


    if(make_valid == TRUE & length(fields_changed)>=1){
      cat("Writing stand shapefile \n")
      sf::st_write(my_shp,input_shapefile,append = FALSE,quiet = TRUE)
      cat("The input shapefile was updated. The following fields were updated to match ForSysX required formats: \n",noquote(fields_changed))
    }

    if(make_valid == TRUE & length(fields_changed)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("The input shapefile was not updated. All fields already match ForSysX required formats")
    }


  }




  if(class(input_shapefile)[1]=="sf"){


    my_list_input_variable<-get_input_names()
    result <- stringr::str_subset(my_list_input_variable$Last.command, "input_shapefile")
    class(result)

    result1 <- gsub(".*input_shapefile", "", result)
    result2 <- gsub(",.*", "", result1)
    result3 <- gsub("=", "", result2)
    result4 <- gsub(" ", "", result3)




    fields_changed <- c()

    my_shp <- input_shapefile

    #stand_id
    stand_id_index <- which(colnames(my_shp)==stand_id)

    stand_id_data <- data.frame(my_shp[,stand_id_index])

    #my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])

    if(is.integer(stand_id_data[,1])==FALSE  & make_valid == TRUE){
      my_shp[,stand_id_index]<-as.integer(stand_id_data[,1])
      fields_changed<-c(fields_changed,stand_id)
    }


    if(length(unique(stand_id_data[,1]))!=nrow(my_shp) & make_valid == FALSE){
      stop("The stand id field is not unique for each stand")
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
        stop("The area field is not numeric")
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
        stop("The exclude field is not an integer")
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
        stop("The available field is not an integer")
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
        stop("The subunit field is not an integer")
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
        stop("The master subunit field is not an integer")
      }

      if(is.integer(master_subunit_field_data[,1])==FALSE & make_valid == TRUE){
        my_shp[,master_subunit_field_index]<-as.integer(master_subunit_field_data[,1])
        fields_changed<-c(fields_changed,master_subunit_field)
      }
    }


    if(make_valid == TRUE & length(fields_changed)>=1){
      #input_shapefile <- my_shp

      #return(input_shapefile)
      suppressWarnings(assign(result4,my_shp,pos = 1))
      #list2env(input_shapefile, envir = .GlobalEnv)

      cat("The input shapefile was updated. The following fields were updated to match ForSysX required formats: \n",noquote(fields_changed))
    }


    if(make_valid == TRUE & length(fields_changed)==0){
      #sf::st_write(my_shp,input_shapefile,append = FALSE)
      cat("The input shapefile was not updated. All fields already match ForSysX required formats")
    }


  }


}
