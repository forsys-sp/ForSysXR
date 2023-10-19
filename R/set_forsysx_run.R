#' Primary function for running the ForSysX treatment planner from R without an
#' existing XML file. If a XML file defining the run already exists, consider using run_forsys_xml
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param outputs_base_name Path and name where the outputs should be stored
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param area Field from input_shapefile containing the area of each stand
#' @param available_for_management If the input_shapefile contains information on available and unavailable stands. <\emph{logical}>
#' @param available Field from input_shapefile containing the available stands
#' @param exclude_stands If the input_shapefile contains information on stands that should be excluded from the prioritization. <\emph{logical}>
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param x_coordinate Field from input_shapefile identifying the x coordinate of each stand
#' @param y_coordinate Field from input_shapefile identifying the y coordinate of each stand
#' @param seed_stand_percent Percentage of stands that will be used to seed potential projects
#' @param max_project_diameter The maximum diameter of any treatment project. The value of -1 means there is no limit
#' @param max_number_projects The maximum number of treatment projects
#' @param inverse_distance_power When this value is not zero, inverse distance weighting of the objective value is used in the stand selection process. A higher value will produce more tightly clumped stands in a project area, but sacrifices the overall objective. A value of zero means that the objective value is not modified by distance
#' @param adjacency_matrix A csv file containing the adjacency file. Can be generated using the parameter output_adjacency_matrix.
#' @param output_adjacency_matrix Path where the adjacency matrix should be stored. Ignore if adjacency file was generated previous, and use the parameter adjacency_matrix
#' @param constraints_name Field from input_shapefile that will be used as a constraint. Typical area or cost.
#' @param constraints_value Constraint value
#' @param constraints_slack Constraint slack.
#' @param effect_fields Field(s) from input_shapefile that sould be stored in the output to measure the effect of the treatments
#' @param objectives Field(s) from input_shapefile identifying the treatment priorities
#' @param output_xml Path and name (with xml extention) where the xml file for the run should be stored.
#' @param run_forsysx Binary. If 1 will run ForSysX. If 0 will end after saving the XML file
#' @param exe_path Path to the ForSysXConsole.exe
#'
#' @return
#' @export
#'
#' @examples
set_forsysx_run <- function(input_shapefile,
                            outputs_base_name,
                            stand_id,area,
                            available_for_management, #default is to not have stands with availability info
                            available,
                            #seed_stands_only_available_stands,
                            #exclude_stands=0, #the default is not to have info on exclude in stands
                            exclude_field,
                            #load_objective_steps,
                            #step_file,
                            #objective_direction,
                            #spatial_optimization,
                            x_coordinate,
                            y_coordinate,
                            seed_stand_percent,
                            max_project_diameter,
                            max_number_projects,
                            #patch_buster,
                            #weight,
                            #patch_identifier,
                            inverse_distance_power,
                            #maximize_distance,
                            adjacency_matrix,
                            output_adjacency_matrix,
                            constraints_name,
                            constraints_value,
                            constraints_slack,
                            effect_fields,
                            objectives,
                            output_xml,
                            run_forsysx,
                            exe_path #,xml_path
                            ) {

  input_shapefile_format <- substrRight(input_shapefile,4)

  if(input_shapefile_format!= ".shp")
    stop("input_shapefile has to be a shapefile!")



  #warning if default definitions are used for some parameters. And say that the
  #user should see the manual to understand if it is doing what they want

  #input_shapefile <- "C:/Users/almeidbr/Desktop/umatilla/stands_with_distance_to_FBN_projects/changed_availability_after_implementing_projects_stands_vs2/Umatilla_Restoration_08282023_with_distance_to_FBN_linear_Rev_proj_1_vs2.shp"
  input_shapefile_use <- gsub("/","\\\\\\\\",input_shapefile)

  outputs_base_name_use <- gsub("/","\\\\\\\\",outputs_base_name)



  if (!missing(output_adjacency_matrix)){
    cat("Loading shapefile")
    my_shp <- sf::read_sf(input_shapefile)

    #keep only the stand id data
    my_shp <- my_shp[,stand_id]
    colnames(my_shp) <- c("V1","geometry")
    #order by the stand id
    my_shp <- my_shp[order(my_shp$V1),]


    cat("Generating adjacency matrix")
    adj_raw <- expp::neighborsDataFrame(spdep::poly2nb(my_shp))
    adj_final <- adj_raw[,c(2,1)]

    colnames(adj_final) <- c(stand_id,paste(" Adjacent_",stand_id,sep=""))

    write.csv(adj_final,paste(output_adjacency_matrix,"/adjacency_matrix_forsys.csv",sep=""),row.names = FALSE)

    adjacency_matrix <- paste(output_adjacency_matrix,"/adjacency_matrix_forsys.csv",sep="")
  }


  adjacency_matrix_use <- gsub("/","\\\\\\\\",adjacency_matrix)


  data("xml_data")
  xml_data_use <- gsub("my_input_shapefile.shp",input_shapefile_use,unlist(xml_data)) #input_shapefile_use
  xml_data_use <- gsub("my_output_name",outputs_base_name_use,unlist(xml_data_use))
  xml_data_use <- gsub("my_adjacency_file.csv",adjacency_matrix_use,unlist(xml_data_use)) #input_shapefile_use
  xml_data_use <- gsub("my_stand_id",stand_id,unlist(xml_data_use))
  xml_data_use <- gsub("my_area_field",area,unlist(xml_data_use))



  #available_for_management -falta fazer este
  if (available_for_management==1){
    rlang::is_missing(available)
  xml_data_use <- gsub("my_availability_field",available,unlist(xml_data_use))}

  #seed_stands_only_available_stands - falta fazer
  #exclude_stands - falta fazer este

  xml_data_use <- gsub("my_exclude_field",exclude_field,unlist(xml_data_use))


  #load_objective_steps - falta fazer
  #step_file - falta fazer
  #objective_direction - falta fazer
  #spatial_optimization - falta fazer

  xml_data_use <- gsub("Point_X",x_coordinate,unlist(xml_data_use))
  xml_data_use <- gsub("Point_Y",y_coordinate,unlist(xml_data_use))


  xml_data_use <- gsub("my_seed_stand",seed_stand_percent,unlist(xml_data_use))
  xml_data_use <- gsub("my_max_diameter",max_project_diameter,unlist(xml_data_use))
  xml_data_use <- gsub("my_project_number",max_number_projects,unlist(xml_data_use))

  xml_data_use <- gsub("Field=\"my_constraint\"",paste("Field=\"",constraints_name,"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub("MinValue=\"my_constraint_val\"",paste("MinValue=\"",constraints_value,"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub("MaxValue=\"my_constraint_val\"",paste("MaxValue=\"",constraints_value,"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub("my_slack",constraints_slack,unlist(xml_data_use))


  #effect_fields vai ser um vector com um máximo de 10 efeitos para começar
  #vou ter de detectar quantos elementos há
  #depois vou apagar os efeitos que nao vao ser usados
  #e vou utilizar os nomes da lista para substituir a lista default.



  all_effects <- length(effect_fields)


  for (i in 1:all_effects){
    xml_data_use <- gsub(paste("<Effect Field=\"my_effect_field_",i,"\" />",sep=""),paste("<Effect Field=\"",effect_fields[i],"\" />",sep=""),unlist(xml_data_use)) #input_shapefile_use
  }


  #delete the unused effect_fields

  for (j in (all_effects+1):20){
    xml_data_use <- gsub(paste("<Effect Field=\"my_effect_field_",j,"\" />",sep=""),"",unlist(xml_data_use))
    #xml_data_use <- gsub("\\\\r\\\\n","",unlist(xml_data_use))
  }


  #objectives

  total_n_objective <- base::length(objectives)
  total_n_objective <- total_n_objective/5

  #integer_val <- total_n_objective%%1==0

  integer_val <- decimalplaces(total_n_objective)

  print(length(objectives))
  print(objectives)
  print(paste("number of objectives: ",total_n_objective,sep=""))
  print(integer_val)

  if(integer_val != 0)
    stop("Wrong number of arguments when defining the objectives")

  if(total_n_objective > 5)
    stop("Maximum number of objectives reached. Maximum number allowd is 5")

  for (k in 1:total_n_objective){

  position_obj <- (k-1)*5
  xml_data_use <- gsub(paste("<Objective Field=\"my_objective_",k,"\"",sep=""),paste("<Objective Field=\"",objectives[position_obj+1],"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub(paste("type_my_objective_",k,sep=""),objectives[position_obj+2],unlist(xml_data_use))
  xml_data_use <- gsub(paste("minweight_my_objective_",k,sep=""),objectives[position_obj+3],unlist(xml_data_use))
  xml_data_use <- gsub(paste("maxweight_my_objective_",k,sep=""),objectives[position_obj+4],unlist(xml_data_use))
  xml_data_use <- gsub(paste("step_my_objective_",k,sep=""),objectives[position_obj+5],unlist(xml_data_use))

  }


  #delete the unused objectives
  if(total_n_objective < 5){
    diff_objectives <- 5-total_n_objective
    position_unused <- (1:5)
    position_unused <- tail(position_unused,diff_objectives)

    for(q in min(position_unused):max(position_unused)){
      xml_data_use <- gsub(paste("<Objective Field=\"my_objective_",q,"\""," Weight=\"1.00\" Type=\"type_my_objective_",q,"\""," MinWeight=\"minweight_my_objective_",q,"\""," MaxWeight=\"maxweight_my_objective_",q,"\""," Step=\"step_my_objective_",q,"\" />",sep=""),"",unlist(xml_data_use))
      }

  }


  write.table(xml_data_use,output_xml,row.names = F,col.names = F,quote = FALSE)

  #falta os seguintes também
  #patch_buster
  #weight
  #patch_identifier
  #inverse_distance_power
  #maximize_distance



  #run forsysX

  if (run_forsysx==1){
    cat("XML file saved")
    run_forsysx_console(exe_path, output_xml)

  } else {cat("XML file saved")}


  }



