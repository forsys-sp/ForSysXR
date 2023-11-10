#' Primary function for running the ForSysX treatment planner from R with an existent XML
#'
#' @param exe_path Path to the ForSysXConsole.exe
#' @param xml_path Path to the XML file that must be used in the ForSysX run
#' @param xml_folder Folder containing the XML files that must be used in the ForSysX run. Useful when running a large number of XML files
#' @param run_zones_interactive Logical. If TRUE, then zones will be run interactively. Default is FALSE. See https://github.com/bmaparicio/ForSysXR/tree/main for an example
#' @return
#' @export
#'
#' @examples
run_forsysx_xml <- function(exe_path,
                            xml_path,
                            xml_folder,
                            run_zones_interactive = FALSE) {

  if(missing(xml_path) & missing(xml_folder)){
    stop("Either xml_path or xml_folder must be specified")
  }

  if(!missing(xml_path) & !missing(xml_folder)){
    stop("Can't use both xml_path and xml_folder. Specify only one")
  }


  #run_forsysx_console(exe_path, output_xml)
  # Call a console application without showing the console window

  if(!missing(xml_folder)){
    xml_files <- list.files(xml_folder,pattern="\\.xml$")

    pt_1<-gsub(".xml","",xml_files)
    pt_2 <- stringr::str_split(pt_1, "_")

    xml_files_df <- data.frame((pt_1))
    xml_files_df <- stringr::str_split_fixed(xml_files_df[,1],"_",length(pt_2[[1]]))

    #order
    xml_files_df <- as.data.frame(xml_files_df)
    xml_files_df[,ncol(xml_files_df)] <- as.numeric(xml_files_df[,ncol(xml_files_df)])

    xml_files_df <- xml_files_df[order(xml_files_df[,ncol(xml_files_df)]),]


    xml_files <- do.call(paste, xml_files_df[1:ncol(xml_files_df)])
    xml_files <- gsub(" ","_",xml_files)
    xml_files <- paste0(xml_files,".xml")

    xml_path <- paste(xml_folder,xml_files,sep="/")

  }


  if(run_zones_interactive == FALSE | missing(run_zones_interactive)){

  if (length(xml_path)==1){

   command <- paste(exe_path, xml_path)
   #shell(command, invisible = TRUE)
   system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)}

  if (length(xml_path)>1){
    xml_path_total <- xml_path

    for(i in 1:length(xml_path_total)){
      cat(paste("Runnig XML ",i," of ",length(xml_path_total),sep=""),'\n')
      xml_path <- xml_path_total[i]
      command <- paste(exe_path, xml_path)
      #shell(command, invisible = TRUE)
      system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)}
    }
  }




  if(run_zones_interactive == TRUE){
    my_used_stands <- c()

    if (length(xml_path)==1){

      command <- paste(exe_path, xml_path)
      #shell(command, invisible = TRUE)
      system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)}

    if (length(xml_path)>1){
      xml_path_total <- xml_path

      for(i in 1:length(xml_path_total)){

        if(i == 1){
          command <- paste(exe_path, xml_path_total[i])
          #shell(command, invisible = TRUE)
          system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)}else{

        cat(paste("Runnig XML ",i," of ",length(xml_path_total),sep=""),'\n')
        xml_path <- xml_path_total[i-1]
        # command <- paste(exe_path, xml_path)
        # #shell(command, invisible = TRUE)
        # system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)


        #load the ouput shapefile with the stands treated


        xml_path_file <- read.table(xml_path,sep="\t") #,quote = ""
        shapefile_output1 <- sub(" StandIDField.*", "", xml_path_file[2,])
        shapefile_output <- sub(".*OutputBaseName=", "", shapefile_output1)
        shapefile_output <- gsub("\\\\","/",shapefile_output)

        #break the path and discard the last part

        shapefile_output_splitted <- stringr::str_split(shapefile_output,pattern = "/")
        total_elements <- length(shapefile_output_splitted[[1]])

        path_to_shp_use <- shapefile_output_splitted[[1]][1:(total_elements-1)]
        path_to_shp_use <- paste(path_to_shp_use, collapse = '/')




        shapefile_output <- intersect(list.files(path_to_shp_use,pattern = "\\.shp$"), list.files(path_to_shp_use,pattern = shapefile_output_splitted[[1]][total_elements]))


        shapefile_output <- paste(path_to_shp_use,shapefile_output,sep="/")

        shapefile_output_use <- spsUtil::quiet(sf::st_read(shapefile_output))


        ####get the id of the stands selected for the project.######


        stand_id1 <- sub(" XCoordField.*", "", xml_path_file[2,])
        stand_id <- sub(".*StandIDField=", "", stand_id1)
        stand_id <- gsub("\\\\","/",stand_id)


        shapefile_output_use

        stand_id_position <- grep(stand_id, colnames(shapefile_output_use))

        names(shapefile_output_use)[stand_id_position] <- "V1"


        #put the used stands in a dataframe

        my_used_stands <- c(my_used_stands,shapefile_output_use$V1)



        ###load the input shapefile of i####


        xml_path_i <- xml_path_total[i]

        #load the ouput shapefile with the stands treated


        xml_path_file_i <- read.table(xml_path_i,sep="\t") #,quote = ""
        shapefile_input1 <- sub(" OutputBaseName.*", "", xml_path_file_i[2,])
        shapefile_input <- sub(".*ShapeFileName=", "", shapefile_input1)
        shapefile_input <- gsub("\\\\","/",shapefile_input)



        shapefile_input_use <- spsUtil::quiet(sf::st_read(shapefile_input))


        ###update the exclude field - zero become ones #####

        #get the name of exclude
        excl_field1 <- sub(" NumberProjects.*", "", xml_path_file_i[2,])
        excl_field <- sub(".*ExclusionField=", "", excl_field1)
        excl_field <- gsub("\\\\","/",excl_field)


        excl_field_position <- grep(excl_field, colnames(shapefile_input_use))
        names(shapefile_input_use)[excl_field_position] <- "exclude_field"

        stand_id_position_2 <- grep(stand_id, colnames(shapefile_input_use))
        names(shapefile_input_use)[stand_id_position_2] <- "V1"



        shapefile_input_use$exclude_field[shapefile_input_use$V1 %in% my_used_stands] <- 1

        shapefile_input_use$exclude_field <- as.integer(shapefile_input_use$exclude_field)

        ###put the names back#####

        excl_field_position <- grep("exclude_field", colnames(shapefile_input_use))
        names(shapefile_input_use)[excl_field_position] <- excl_field

        stand_id_position_2 <- grep("V1", colnames(shapefile_input_use))
        names(shapefile_input_use)[stand_id_position_2] <- stand_id

        shapefile_input_use


        ###save the shapefile and replace the previous one####

        spsUtil::quiet(sf::st_write(shapefile_input_use,shapefile_input,append = FALSE))



        ###run forsys with the updated fields####


        command <- paste(exe_path, xml_path_total[i])
        #shell(command, invisible = TRUE)
        system2("cmd.exe", c("/c", command), invisible = TRUE, wait = TRUE)


        }






        }
    }
  }





}
