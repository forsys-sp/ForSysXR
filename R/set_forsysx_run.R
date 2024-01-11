#' Primary function for running the ForSysX treatment planner from R without an existent XML
#' existing XML file. If a XML file defining the run already exists, consider using run_forsys_xml
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param outputs_base_name Path and name where the outputs should be stored
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param area Field from input_shapefile containing the area of each stand
#' @param available Field from input_shapefile containing the available stands
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
#' @param constraints_slack Constraint slack. Default is blank (i.e. will not use slack to tag valid and invalid projects)
#' @param threshold Vector containing the threshold field, the symbol of inequality or equality (">","<","==",">=","<="), and the threshold value
#' @param threshold_logic Vector with two elements containing the threshold logic. The first element refers to if a single value should be used for the threshold ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple thresholds must be combined, either selecting stands where all thresholds are met ("and") or select stands where any of the thresholds are met ("or"). Default is c("single_value","and")
#' @param patchbuster_identifier Optional. Field containing the patch identifier. Only use when setting a patchbuster run
#' @param patchbuster_weight Optional. Weight for the patchbuster function. Only use when setting a patchbuster run.
#' @param effect_fields Field(s) from input_shapefile that sould be stored in the output to measure the effect of the treatments
#' @param objectives Field(s) from input_shapefile identifying the treatment priorities
#' @param subunit_field Field from input_shapefile identifying the pre-defined planning areas
#' @param master_subunit If master subunits should be used
#' @param save_outputs Vector with the outputs that must be saved. Options are "shapefile", "stand_csv" and/or "image". At least one output must be saved.
#' @param output_xml Path and name (with xml extention) where the xml file for the run should be stored.
#' @param run_forsysx Binary. If 1 will run ForSysX. If 0 will end after saving the XML file
#' @param overwrite_data Logical. If TRUE, then any existing data with the same name and in the same directory will be replaced. Default is FALSE, which returns an error if the layer exists.
#' @param load_results Logical. If TRUE, then the csv stands and projects results from running ForSysX will be automatically loaded in R with the variable name as the outputs_base_name set by the user followed by prj_results and stnd_results. Default is FALSE
#' @param exe_path Path to the ForSysXConsole.exe
#' @param seed_stands_only_available_stands. Binary. If 1, then only the stands available for management are used as seed stands for potential projects. If 0 then all stands can be used as seed stands for potential projects. Default is 0.
#' @param MaximizeDistanceOpt Binary. If 1, then Maximize Distance will be used and the effect of inverse distance weighting is reversed. Useful when creating fuelbreak networks. Default is 0.
#' @param plot_results Optional. If TRUE, the projects created by ForSys will be plotted. Requires that "shapefile" is saved as an output using the save_outputs parameter. Default is FALSE
#'
#' @import dplyr sf ggplot2 shiny
#' @return
#' @export
#'
#' @examples
set_forsysx_run <- function(input_shapefile,
                            outputs_base_name,
                            stand_id,area,
                            #available_for_management, #default is to not have stands with availability info
                            available,
                            seed_stands_only_available_stands=0,
                            #exclude_stands, #the default is not to have info on exclude in stands
                            exclude_field,
                            #load_objective_steps,
                            #step_file,
                            #objective_direction,
                            #spatial_optimization,
                            x_coordinate,
                            y_coordinate,
                            seed_stand_percent=100,
                            max_project_diameter= -1,
                            max_number_projects,
                            #patch_buster,
                            #weight,
                            #patch_identifier,
                            inverse_distance_power=0, #default is 0
                            #maximize_distance = 0, #default is 0 (not use)
                            MaximizeDistanceOpt =0, #0 has to be the default. This is only useful for FBN
                            patchbuster_identifier,
                            patchbuster_weight,
                            adjacency_matrix,
                            output_adjacency_matrix,
                            constraints_name,
                            constraints_value,
                            constraints_slack ="",
                            effect_fields,
                            objectives,
                            threshold,
                            threshold_logic = c("single_value","and"),
                            subunit_field,
                            master_subunit,
                            output_xml,
                            run_forsysx,
                            save_outputs,
                            overwrite_data=FALSE,
                            plot_results=FALSE,
                            load_results=FALSE,
                            exe_path #,xml_path
                            ) {


  if (file.exists(output_xml)) {
    #Delete file if it exists
    file.remove(output_xml)
  }





  # all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)
  #
  # all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
  # all_elements_use <- as.character(all_elements_use)
  #
  # path_with_results <- paste(all_elements_use, collapse = '/')
  #
  # #list patterns
  # #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))
  #
  # last_name <- all_elements[,ncol(all_elements)]
  # #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))
  #
  # output_shp_run = intersect(list.files(path_with_results, paste(as.numeric(constraints_value),".shp$",sep="")), list.files(path_with_results,pattern = last_name))
  #
  #




  if(class(input_shapefile)[1]=="character"){
  input_shapefile_format <- substrRight(input_shapefile,4)
  if(input_shapefile_format!= ".shp")
    stop("input_shapefile has to be a shapefile!")}

  if (missing(adjacency_matrix) & missing(output_adjacency_matrix)) {
    stop("User must specify an existing adjacency matrix or generate one (output_adjacency_matrix parameter)")
  }

  # if(exclude_stands !=0) {
  #   if (exclude_stands !=1){
  #     stop("exclude_stands must be 0 or 1")
  #   }}
  #
  # if(exclude_stands ==1 & missing(exclude_field)) {
  #   stop("User is specifying that some stands must be excluded without indicating the field with this information")
  # }

  #warning if default definitions are used for some parameters. And say that the
  #user should see the manual to understand if it is doing what they want

  #input_shapefile <- "C:/Users/almeidbr/Desktop/umatilla/stands_with_distance_to_FBN_projects/changed_availability_after_implementing_projects_stands_vs2/Umatilla_Restoration_08282023_with_distance_to_FBN_linear_Rev_proj_1_vs2.shp"




  if (file.exists(paste0(outputs_base_name,"_Results.csv")) & overwrite_data==FALSE){
    stop("Output file already exists. If you want to replace the file, use overwrite_data=TRUE")
  }



  if (file.exists(paste0(outputs_base_name,"_stand_data.shp")) & overwrite_data==FALSE){
    stop("Input shapefile already exists. If you want to replace the file, use overwrite_data=TRUE")
  }



#this is ONLY if we are either plotting (plot=TRUE) or generating the adjacency matrix
  if(class(input_shapefile)[1]=="sf"){
    my_shp <- (input_shapefile)


    if(overwrite_data==TRUE){
    sf::st_write(input_shapefile,paste(outputs_base_name,"_stand_data.shp",sep=""),append=FALSE)}else{
      sf::st_write(input_shapefile,paste(outputs_base_name,"_stand_data.shp",sep=""))
    }
    input_shapefile <- paste(outputs_base_name,"_stand_data.shp",sep="")
  }

  if(class(input_shapefile)[1]=="character"){
    if(plot_results==TRUE | !missing(output_adjacency_matrix)){
    my_shp <- sf::st_read(input_shapefile)
  }}



  input_shapefile_use <- gsub("/","\\\\\\\\",input_shapefile)

  outputs_base_name_use <- gsub("/","\\\\\\\\",outputs_base_name)



  if (!missing(output_adjacency_matrix)){
    cat("Loading shapefile",'\n')


    #keep only the stand id data
    my_shp <- my_shp[,stand_id]
    colnames(my_shp) <- c("V1","geometry")
    #order by the stand id
    my_shp <- my_shp[order(my_shp$V1),]


    my_shp_df_nei <- as.data.frame(my_shp)


    cat("Generating adjacency matrix",'\n')
    adj_raw <- neighborsDataFrame_expp(spdep::poly2nb(my_shp, queen=FALSE))
    adj_raw$id_correct <- my_shp_df_nei[adj_raw$id,1]
    adj_raw$id_neigh_correct <- my_shp_df_nei[adj_raw$id_neigh,1]

    adj_final <- adj_raw[,c(4,3)]

    colnames(adj_final) <- c(stand_id,paste(" Adjacent_",stand_id,sep=""))

    #write.csv(adj_final,paste(output_adjacency_matrix,"/adjacency_matrix_forsys.csv",sep=""),row.names = FALSE)

    write.table(adj_final, file=paste(output_adjacency_matrix,"/adjacency_matrix_forsys.csv",sep=""), sep=",", row.names = FALSE, quote=FALSE)

    adjacency_matrix <- paste(output_adjacency_matrix,"/adjacency_matrix_forsys.csv",sep="")
  }


  adjacency_matrix_use <- gsub("/","\\\\\\\\",adjacency_matrix)



  if(missing(threshold)){
    data("xml_data_threshold")
    xml_data <- xml_data_threshold
  }


  if(!missing(threshold)){
    data("xml_data")
    }


  xml_data_use <- gsub("my_input_shapefile.shp",input_shapefile_use,unlist(xml_data)) #input_shapefile_use
  xml_data_use <- gsub("my_output_name",outputs_base_name_use,unlist(xml_data_use))
  xml_data_use <- gsub("my_adjacency_file.csv",adjacency_matrix_use,unlist(xml_data_use)) #input_shapefile_use
  xml_data_use <- gsub("my_stand_id",stand_id,unlist(xml_data_use))
  xml_data_use <- gsub("my_area_field",area,unlist(xml_data_use))




  #available_for_management -falta fazer este
  #if (available_for_management==1){
    if (missing(available)) {
      xml_data_use <- gsub("my_availability_field","",unlist(xml_data_use))
      xml_data_use <- gsub("Availability=\"1\"","Availability=\"0\"",unlist(xml_data_use))
      xml_data_use <- gsub("SeedOnlyAvail=\"1\"",paste("SeedOnlyAvail=\"",0,"\"",sep=""),unlist(xml_data_use))
    }

  if (!missing(available)) {
  if (length(available)==1) {
    xml_data_use <- gsub("my_availability_field",available,unlist(xml_data_use))
    xml_data_use <- gsub("Availability=\"1\"","Availability=\"1\"",unlist(xml_data_use))
    xml_data_use <- gsub("SeedOnlyAvail=\"1\"",paste("SeedOnlyAvail=\"",seed_stands_only_available_stands,"\"",sep=""),unlist(xml_data_use))
  } else{
    stop("One and only one field must be used to specify the available stands")
  }}


  #available_for_management -falta fazer este
  # if (available_for_management==1){
  #   if (missing(available)) {
  #     stop("User is specifying that some stands cannot be treated without indicating the field with this information")
  #     }
  # xml_data_use <- gsub("my_availability_field",available,unlist(xml_data_use))
  # xml_data_use <- gsub("SeedOnlyAvail=\"1\"",paste("SeedOnlyAvail=\"",seed_stands_only_available_stands,"\"",sep=""),unlist(xml_data_use))
  # }


  #Exclude stands

  if (missing(exclude_field)) {
    xml_data_use <- gsub(paste("Exclusions=\"1\""),paste("Exclusions=\"",0,"\"",sep=""),unlist(xml_data_use))
    xml_data_use <- gsub("my_exclude_field","",unlist(xml_data_use))
    }


  if (!missing(exclude_field)) {
    if(length(exclude_field)==1){
      xml_data_use <- gsub(paste("Exclusions=\"1\""),paste("Exclusions=\"",1,"\"",sep=""),unlist(xml_data_use))
      xml_data_use <- gsub("my_exclude_field",exclude_field,unlist(xml_data_use))
    } else{
    stop("One and only one field must be used to specify the excluded stands")
  }}


  # xml_data_use <- gsub(paste("Exclusions=\"1\""),paste("Exclusions=\"",exclude_stands,"\"",sep=""),unlist(xml_data_use))
  #
  # if (exclude_stands == 0) {
  #   xml_data_use <- gsub("my_exclude_field","",unlist(xml_data_use))
  # } else {
  #   xml_data_use <- gsub("my_exclude_field",exclude_field,unlist(xml_data_use))
  # }




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




  #repeat for threshold
#
#   if(missing(threshold)){
#     for(q in 1:6){
#       xml_data_use <- gsub(paste("<Threshold Field=\"my_threshold",q,"\""," Operator=\"my_operator",q,"\""," Value=\"1.00\" MinValue=\"min_val_threshold",q,"\""," MaxValue=\"10.00\" Step=\"1.00\" />",sep=""),"",unlist(xml_data_use))
#     }
#   }


  if(!missing(threshold)){
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
      xml_data_use <- gsub(paste("<Threshold Field=\"my_threshold",q,"\""," Operator=\"my_operator",q,"\""," Value=\"1.00\" MinValue=\"min_val_threshold",q,"\""," MaxValue=\"10.00\" Step=\"step_threshold",q,"\" />",sep=""),"",unlist(xml_data_use))
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





  #MaximizeDistanceOpt

  xml_data_use <- gsub(paste("MaximizeDistanceOpt=\"0\"",sep=""),paste("MaximizeDistanceOpt=\"",MaximizeDistanceOpt,"\"",sep=""),unlist(xml_data_use))

  #IdwPower
  xml_data_use <- gsub("my_idw_value",inverse_distance_power,unlist(xml_data_use))


  #export shapefile
  if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
    xml_data_use <- gsub(paste("DisableShapeOutput=\"0\"",sep=""),"DisableShapeOutput=\"0\"",unlist(xml_data_use))
  } else {
    xml_data_use <- gsub(paste("DisableShapeOutput=\"0\"",sep=""),"DisableShapeOutput=\"1\"",unlist(xml_data_use))
  }


  #export image
  if(any(grepl("image", save_outputs, fixed = TRUE))==TRUE){
    xml_data_use <- gsub(paste("ImageOutput=\"0\"",sep=""),"ImageOutput=\"1\"",unlist(xml_data_use))
  } else {
    xml_data_use <- gsub(paste("ImageOutput=\"0\"",sep=""),"ImageOutput=\"0\"",unlist(xml_data_use))
  }


  #export csv
  if(any(grepl("stand_csv", save_outputs, fixed = TRUE))==TRUE){
    xml_data_use <- gsub(paste("DisablePointsOutput=\"0\"",sep=""),"DisablePointsOutput=\"0\"",unlist(xml_data_use))
  } else {
    xml_data_use <- gsub(paste("DisablePointsOutput=\"0\"",sep=""),"DisablePointsOutput=\"1\"",unlist(xml_data_use))
  }




  #subunit

  if (!missing(subunit_field)){
    xml_data_use <- gsub(paste("SubunitsEnable=\"0\"",sep=""),"SubunitsEnable=\"1\"",unlist(xml_data_use))
    xml_data_use <- gsub(paste("SubunitsField=\"\"",sep=""),paste("SubunitsField=\"",subunit_field,"\"",sep=""),unlist(xml_data_use))
  }


  if (missing(master_subunit)){
    xml_data_use <- gsub(paste("NestedSubunitsEnable=\"1\"",sep=""),"NestedSubunitsEnable=\"0\"",unlist(xml_data_use))

  }


  #falta os seguintes também
  #patch_buster
  #weight
  #patch_identifier

  if (!missing(patchbuster_identifier)){
    xml_data_use <- gsub("my_patchbuster_id",patchbuster_identifier,unlist(xml_data_use))
    xml_data_use <- gsub("RecursiveOpt=\"0\"","RecursiveOpt=\"1\"",unlist(xml_data_use))
    xml_data_use <- gsub("my_patchbuster_weight",patchbuster_weight,unlist(xml_data_use))
  } else {
    xml_data_use <- gsub("my_patchbuster_id","",unlist(xml_data_use))
    xml_data_use <- gsub("my_patchbuster_weight","0",unlist(xml_data_use))
    xml_data_use <- gsub("RecursiveOpt=\"0\"","RecursiveOpt=\"0\"",unlist(xml_data_use))
    }

  # if (!missing(patchbuster_weight)){
  #   xml_data_use <- gsub("my_patchbuster_weight",patchbuster_weight,unlist(xml_data_use))
  # } else {
  #   xml_data_use <- gsub("my_patchbuster_weight","",unlist(xml_data_use))
  # }






  #run forsysX

  #wait a bit for the xml to be written and then run forsysX
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<5){
    #print("waiting")
  }

  write.table(xml_data_use,output_xml,row.names = F,col.names = F,quote = FALSE)

  if (run_forsysx==1){
    cat("XML file saved",'\n')
    run_forsysx_console(exe_path, output_xml)

  } else {cat("XML file saved",'\n')}








  # if(missing(load_results)){
  #   load_results<- FALSE
  #
  # }


  if(load_results==TRUE){
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

    #exclude this file from the output_csv_run
    exclude_this_file_csv <- paste(last_name,"_Results.csv",sep="")


    if(any(grepl("stand_csv", save_outputs, fixed = TRUE))==TRUE){
    output_csv_run_use <- output_csv_run[!output_csv_run %in% exclude_this_file_csv]

    if(length(output_csv_run_use)!=1){
      stop("Could not load ForSysX results in R. Multiple files match the name given. Please use a unique name in outputs_base_name or load the csv results manually.")
    } else {
      stnd_results <- read.csv(paste(path_with_results,output_csv_run_use,sep="/"))
    }

    suppressWarnings(assign("stnd_results",stnd_results,pos = 1)) #,pos = 1
    }


    suppressWarnings(assign("prj_results",prj_results,pos = 1)) #,pos = 1


  }






  #plot the results if the shapefile is saved

  #get the number of shapefiles exported#

  all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

  all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
  all_elements_use <- as.character(all_elements_use)

  path_with_results <- paste(all_elements_use, collapse = '/')


  last_name <- all_elements[,ncol(all_elements)]

  number_scenarios_created <- intersect(list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep="")), list.files(path_with_results,pattern = last_name))



  if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
    if(plot_results==TRUE & length(number_scenarios_created) == 1){
      all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

      all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
      all_elements_use <- as.character(all_elements_use)

      path_with_results <- paste(all_elements_use, collapse = '/')

      #list patterns
      #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

      last_name <- all_elements[,ncol(all_elements)]
      #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

      output_shp_run = intersect(list.files(path_with_results, paste(as.numeric(constraints_value),".shp$",sep="")), list.files(path_with_results,pattern = last_name))


      output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"))

      if (missing(my_shp)){
        my_shp <- sf::st_read(input_shapefile)
      }


      my_shp$diss <- 1

      ttt <- my_shp  %>%
        #mutate_at(c('diss'), ~na_if(., 0)) %>%
        #st_combine() %>%
        ggplot() +
        geom_sf(aes(fill=diss),fill="grey",color=NA) +
        #ggtitle("Projects ranking") +
        theme_void()+
        theme(plot.title=element_text(hjust=0.5))+
        #guides(fill="none")+
        geom_sf(data=output_shp_run,aes(fill=ProjectNum),color=NA)+
        scale_fill_viridis_c(option = "turbo",direction=-1)+
        labs(fill='Project number')

      return(ttt)
    }

  }



  if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
    if(plot_results==TRUE & length(number_scenarios_created) > 1){

      output_shp_run = intersect(list.files(path_with_results, paste(as.numeric(constraints_value),".shp$",sep="")), list.files(path_with_results,pattern = last_name))


      #output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"))

      plot_data <- list()
      all_name_output_shiny <- data.frame()

      for(e in 1:length(number_scenarios_created)){
        output_shp_run_plot <- sf::st_read(paste(path_with_results,output_shp_run[[e]],sep="/"))

        name_output_shiny <- output_shp_run[[e]]

        plot_data[[paste("ttt_", e, sep = "")]] <- my_shp  %>%
          #mutate_at(c('diss'), ~na_if(., 0)) %>%
          #st_combine() %>%
          ggplot() +
          geom_sf(aes(fill=diss),fill="grey",color=NA) +
          #ggtitle("Projects ranking") +
          theme_void()+
          theme(plot.title=element_text(hjust=0.5))+
          #guides(fill="none")+
          geom_sf(data=output_shp_run_plot,aes(fill=ProjectNum),color=NA)+
          scale_fill_viridis_c(option = "turbo",direction=-1)+
          labs(fill='Project number')

        all_name_output_shiny <- rbind(all_name_output_shiny,name_output_shiny)

      }


      colnames(all_name_output_shiny)<-"names"

      ui <- fluidPage(
        titlePanel("ForSysX outputs"),
        mainPanel(
          plotOutput("current_plot"),
          fluidRow(
            column(12, textOutput("plot_number")),
            column(12, textOutput("plot_title"))
          ),
          actionButton("next_button", "Next Plot")
        )
      )

      # Define the server
      server <- function(input, output) {
        # Initialize reactive values
        current_plot_index <- reactiveVal(1)
        plot_titles <- reactiveVal(paste("File: ",all_name_output_shiny$names,sep = ""))

        # Function to render the current plot
        output$current_plot <- renderPlot({
          print(plot_data[[current_plot_index()]])
        })

        # Function to render the current plot number text
        output$plot_number <- renderText({
          paste("Plot", current_plot_index(), "of", length(plot_data))
        })

        # Function to render the current plot title
        output$plot_title <- renderText({
          plot_titles()[current_plot_index()]
        })

        # Observe the click event on the "Next Plot" button
        observeEvent(input$next_button, {
          # Increment the current plot index
          current_plot_index(current_plot_index() + 1)

          # Reset to the first plot if we reached the end
          if (current_plot_index() > length(plot_data)) {
            current_plot_index(1)
          }
        })
      }

      # Create the shiny app
      return(shinyApp(ui, server))

    }
  }








  if(any(grepl("shapefile", save_outputs, fixed = TRUE))==FALSE){
    if(plot_results==TRUE){
      warning("Unable to plot results as the output shapefile was not exported.")
    }
  }


  }



