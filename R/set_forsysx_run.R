#' Primary function for running the ForSysX planning system from R without an
#' existing XML file. If an XML file defining the run already exists, consider using run_forsys_xml
#'
#' @param input_shapefile Shapefile containing the stands that will be used in the prioritization process
#' @param outputs_base_name Path and name where the outputs should be stored
#' @param stand_id Field from input_shapefile containing the unique identifier for individual stands
#' @param area Field from input_shapefile containing the area of each stand
#' @param available Field from input_shapefile containing the available stands
#' @param exclude_field Field from input_shapefile identifying the stands to be excluded
#' @param spatial_optimization Logical. If TRUE, then the treatment areas are spatially aggregated into patches (projects) of adjacent stands. If FALSE, then projects are not created and stands are sorted based on their relevance for the objective(s). Default is TRUE.
#' @param x_coordinate Field from input_shapefile identifying the x coordinate of each stand
#' @param y_coordinate Field from input_shapefile identifying the y coordinate of each stand
#' @param seed_stand_percent Percentage of stands that will be used to seed potential projects
#' @param max_project_diameter The maximum diameter of any treatment project. The value of -1 means there is no limit
#' @param max_number_projects The maximum number of treatment projects
#' @param inverse_distance_power When this value is not zero, inverse distance weighting of the objective value is used in the stand selection process. A higher value will produce more tightly clumped stands in a project area, but sacrifices the overall objective. A value of zero means that the objective value is not modified by distance
#' @param adjacency_matrix A csv file containing the adjacency file. Can be generated using the parameter output_adjacency_matrix.
#' @param output_adjacency_matrix Path where the adjacency matrix should be stored. Ignore if adjacency file was generated previous, and use the parameter adjacency_matrix
#' @param constraints Vector containing the constraint(s) field, the constraint value (or minimum, maximum and step to be used), and the slack value to be used. The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param constraints_logic Vector with two elements containing the constraint logic. The first element refers to if a single value should be used for the constraint ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple constraints must be combined, either selecting stands where all constraints are met ("and") or select stands where any of the constraints are met ("or"). Default is c("single_value","and")
#' @param threshold Vector containing the threshold(s) field, the symbol of inequality or equality (">","<","==",">=","<="), and the threshold value (or minimum, maximum and step to be used). The vector can have a length of 3 or 5 elements, depending if using single_value or multiple_value in the constraints_logic
#' @param threshold_logic Vector with two elements containing the threshold logic. The first element refers to if a single value should be used for the threshold ("single_value") or if multiple values with stepping should be used ("multiple_value"). The second element refers to how multiple thresholds must be combined, either selecting stands where all thresholds are met ("and") or select stands where any of the thresholds are met ("or"). Default is c("single_value","and")
#' @param patchbuster_identifier Optional. Field containing the patch identifier. Only use when setting a patchbuster run
#' @param patchbuster_weight Optional. Weight for the patchbuster function. Only use when setting a patchbuster run.
#' @param effect_fields Field(s) from input_shapefile that sould be stored in the output to measure the effect of the treatments
#' @param objectives Vector containing the field(s) from input_shapefile identifying the treatment priorities, type, minimum weight, maximum weight, and step. Five elements in the vector characterize each objective, following the order c("objective","treatment type","min weight","max weight", "step").
#' @param objective_direction Direction ("minimize" or "maximize") for the objectives. If maximize, stands with highest values will be selected first. If minimize, stands with lowest values will be selected first. Default is "maximize"
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
#' @param build_report Optional. If TRUE, a html report showing ForSys results and projects' attainment is created. Requires run_forsysx = 1. Default is FALSE
#' @param build_interac_report Optional. If TRUE, an interactive html report showing ForSys results and projects' attainment is created. Requires run_forsysx = 1. Default is FALSE
#' @param report_variables Optional. A vector containing the names of categorical field(s) that should be analyzed in the report. Only available when only one scenario is created
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
                            #constraints_name,
                            #constraints_value,
                            #constraints_slack ="",
                            constraints,
                            constraints_logic = c("single_value","and"),
                            effect_fields,
                            objective_direction="maximize",
                            objectives,
                            threshold,
                            threshold_logic = c("single_value","and"),
                            subunit_field,
                            master_subunit,
                            output_xml,
                            run_forsysx,
                            save_outputs,
                            spatial_optimization=TRUE,
                            overwrite_data=FALSE,
                            plot_results=FALSE,
                            load_results=FALSE,
                            build_report=FALSE,
                            build_interac_report=FALSE,
                            report_variables,
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

  if (missing(adjacency_matrix) & missing(output_adjacency_matrix) & spatial_optimization==TRUE) {
    stop("User must specify an existing adjacency matrix or generate one (output_adjacency_matrix parameter)")
  }

  if(spatial_optimization==FALSE){
    adjacency_matrix<-""
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

    if(max(nchar(names(my_shp)))>10){
      stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
    }



    if(overwrite_data==TRUE){
    sf::st_write(input_shapefile,paste(outputs_base_name,"_stand_data.shp",sep=""),append=FALSE)}else{
      sf::st_write(input_shapefile,paste(outputs_base_name,"_stand_data.shp",sep=""))
    }
    input_shapefile <- paste(outputs_base_name,"_stand_data.shp",sep="")
  }



  if(class(input_shapefile)[1]=="character"){
    if(plot_results==TRUE | !missing(output_adjacency_matrix)){
    my_shp <- sf::st_read(input_shapefile,quiet=TRUE)

    if(max(nchar(names(my_shp)))>10){
      stop("The shapefile contains at least one field named with more than 10 characters. Please modify it manually or by using the function check_input_shapefile")
    }

  }}





  input_shapefile_use <- gsub("/","\\\\\\\\",input_shapefile)

  outputs_base_name_use <- gsub("/","\\\\\\\\",outputs_base_name)

  suppressMessages(sf::sf_use_s2(FALSE))
  #my_shp<-st_make_valid(my_shp)



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

  if (spatial_optimization==TRUE){


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
    if(objective_direction=="minimize"){
  xml_data_use <- gsub("ObjectiveDirection=\"1\"","ObjectiveDirection=\"0\"",unlist(xml_data_use))
    }

  xml_data_use <- gsub("Point_X",x_coordinate,unlist(xml_data_use))
  xml_data_use <- gsub("Point_Y",y_coordinate,unlist(xml_data_use))


  xml_data_use <- gsub("my_seed_stand",seed_stand_percent,unlist(xml_data_use))
  xml_data_use <- gsub("my_max_diameter",max_project_diameter,unlist(xml_data_use))
  xml_data_use <- gsub("my_project_number",max_number_projects,unlist(xml_data_use))



  #new constraints - works with vector now

  #constraints <- c("area_ha", 50, "")
}




  if (spatial_optimization==FALSE){

      xml_data_use <- gsub(paste("Exclusions=\"1\""),paste("Exclusions=\"",0,"\"",sep=""),unlist(xml_data_use))
      xml_data_use <- gsub("my_exclude_field","",unlist(xml_data_use))



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

    xml_data_use <- gsub("Point_X","",unlist(xml_data_use))
    xml_data_use <- gsub("Point_Y","",unlist(xml_data_use))


    xml_data_use <- gsub("my_seed_stand","100",unlist(xml_data_use))
    xml_data_use <- gsub("my_max_diameter","-1",unlist(xml_data_use))
    xml_data_use <- gsub("my_project_number","1",unlist(xml_data_use))



    #new constraints - works with vector now

    #constraints <- c("area_ha", 50, "")

    xml_data_use <- gsub("Aggregate=\"1\"","Aggregate=\"0\"",unlist(xml_data_use))

  }





  if(missing(constraints_logic)){
  #if missing constraints_logic, then it is always single_value

    if(!(length(constraints) %in% c(3,6,9,12,15,18))){
      stop("Wrong number of constraints. Expected three arguments per constraint - name, value and slack")
    }

    #length(constraints)


  total_n_constraints <- base::length(constraints)
  total_n_constraints <- total_n_constraints/3

  #integer_val <- total_n_constraints%%1==0

  integer_val <- decimalplaces(total_n_constraints)

  if(integer_val != 0)
    stop("Wrong number of arguments when defining the constraints")

  if(total_n_constraints > 6)
    stop("Maximum number of constraints reached. Maximum number allowd is 6")

  for (k in 1:total_n_constraints){


    position_constraints <- (k-1)*3

    constraints_name <- constraints[position_constraints+1]
    constraints_value <- constraints[position_constraints+2]
    constraints_slack <- constraints[position_constraints+3]

    xml_data_use <- gsub(paste("my_constraint",k,"\"",sep=""),paste(constraints_name,"\"",sep=""),unlist(xml_data_use))
    xml_data_use <- gsub(paste("my_min_constraint_val",k,"\"",sep=""),paste(constraints_value,"\"",sep=""),unlist(xml_data_use))
    xml_data_use <- gsub(paste("my_max_constraint_val",k,"\"",sep=""),paste(constraints_value,"\"",sep=""),unlist(xml_data_use))
    xml_data_use <- gsub(paste("step_constraint",k,"\"",sep=""),paste("0",'"',sep=""),unlist(xml_data_use))
    xml_data_use <- gsub(paste("my_slack",k,sep=""),constraints_slack,unlist(xml_data_use))


  }


  #delete the unused constraints
  if(total_n_constraints < 6){
    diff_constraints <- 6-total_n_constraints
    position_unused <- (1:6)
    position_unused <- tail(position_unused,diff_constraints)

    for(q in min(position_unused):max(position_unused)){
      xml_data_use <- gsub(paste("<Constraint Field=\"my_constraint",q,"\""," MinValue=\"my_min_constraint_val",q,"\""," MaxValue=\"my_max_constraint_val",q,"\""," MinField=\"\" MaxField=\"\" Step=\"step_constraint",q,"\""," Slack=\"my_slack",q,"\" />",sep=""),"",unlist(xml_data_use))
    }
  }





  }



  # xml_data_use <- gsub("Field=\"my_constraint\"",paste("Field=\"",constraints_name,"\"",sep=""),unlist(xml_data_use))
  # xml_data_use <- gsub("MinValue=\"my_constraint_val\"",paste("MinValue=\"",constraints_value,"\"",sep=""),unlist(xml_data_use))
  # xml_data_use <- gsub("MaxValue=\"my_constraint_val\"",paste("MaxValue=\"",constraints_value,"\"",sep=""),unlist(xml_data_use))
  # xml_data_use <- gsub("my_slack",constraints_slack,unlist(xml_data_use))

  #constraints <- c("area_ha", 50, "")

  #constraints_logic <- c("single_value","and")

  if(!missing(constraints_logic)){

    if(constraints_logic[1] == "single_value"){

      if(!(length(constraints) %in% c(3,6,9,12,15,18))){
        stop("Wrong number of constraints. Expected three arguments per constraint - name, value and slack")
      }

      #length(constraints)


      total_n_constraints <- base::length(constraints)
      total_n_constraints <- total_n_constraints/3

      #integer_val <- total_n_constraints%%1==0

      integer_val <- decimalplaces(total_n_constraints)

      if(integer_val != 0)
        stop("Wrong number of arguments when defining the constraints")

      if(total_n_constraints > 6)
        stop("Maximum number of constraints reached. Maximum number allowd is 6")

      for (k in 1:total_n_constraints){


        position_constraints <- (k-1)*3

        constraints_name <- constraints[position_constraints+1]
        constraints_value <- constraints[position_constraints+2]
        constraints_slack <- constraints[position_constraints+3]

        xml_data_use <- gsub(paste("my_constraint",k,"\"",sep=""),paste(constraints_name,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_min_constraint_val",k,"\"",sep=""),paste(constraints_value,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_max_constraint_val",k,"\"",sep=""),paste(constraints_value,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("step_constraint",k,sep=""),"0",unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_slack",k,sep=""),constraints_slack,unlist(xml_data_use))


      }


      #delete the unused constraints
      if(total_n_constraints < 6){
        diff_constraints <- 6-total_n_constraints
        position_unused <- (1:6)
        position_unused <- tail(position_unused,diff_constraints)

        for(q in min(position_unused):max(position_unused)){
          xml_data_use <- gsub(paste("<Constraint Field=\"my_constraint",q,"\""," MinValue=\"my_min_constraint_val",q,"\""," MaxValue=\"my_max_constraint_val",q,"\""," MinField=\"\" MaxField=\"\" Step=\"step_constraint",q,"\""," Slack=\"my_slack",q,"\" />",sep=""),"",unlist(xml_data_use))
        }
      }



      #and or or



      if(length(constraints_logic)!=2){
        stop("constraints_logic has to have two elements. The first has to be single_value or multiple_value, and the second and or or.")
      }

      if(constraints_logic[1]!="single_value" & constraints_logic[1]!="multiple_value"){
        stop("constraints_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(constraints_logic[2]!="and" & constraints_logic[2]!="or"){
        stop("constraints_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(constraints_logic[2]=="and"){
        constraints_logic_val <- 0
      }

      if(constraints_logic[2]=="or"){
        constraints_logic_val <- 1
      }

      xml_data_use <- gsub("ConstraintLogic=\"0\"", paste("ConstraintLogic=\"",constraints_logic_val,"\"", sep=""),unlist(xml_data_use))


      if(constraints_logic[1]=="single_value"){
        constraints_logic_single <- 1
      }


      if(constraints_logic[1]=="multiple_value"){
        constraints_logic_single <- 0
      }

      xml_data_use <- gsub("ConstraintSingleValue=\"1\"", paste("ConstraintSingleValue=\"",constraints_logic_single,"\"", sep=""),unlist(xml_data_use))




    }
  }



  #constraints <- c("area_ha", 50, 150, 50, "")

  #constraints_logic <- c("multiple_value","and")

  if(!missing(constraints_logic)){

    if(constraints_logic[1] == "multiple_value"){

      if(!(length(constraints) %in% c(5,10,15,20,25,30))){
        stop("Wrong number of constraints. Expected five arguments per constraint - name, minumum value, maximum value, step and slack")
      }

      #length(constraints)


      total_n_constraints <- base::length(constraints)
      total_n_constraints <- total_n_constraints/5

      #integer_val <- total_n_constraints%%1==0

      integer_val <- decimalplaces(total_n_constraints)

      if(integer_val != 0)
        stop("Wrong number of arguments when defining the constraints")

      if(total_n_constraints > 6)
        stop("Maximum number of constraints reached. Maximum number allowd is 6")

      for (k in 1:total_n_constraints){


        position_constraints <- (k-1)*5

        constraints_name <- constraints[position_constraints+1]
        min_constraints_value <- constraints[position_constraints+2]
        max_constraints_value <- constraints[position_constraints+3]
        step_constraints_value <- constraints[position_constraints+4]
        constraints_slack <- constraints[position_constraints+5]

        xml_data_use <- gsub(paste("my_constraint",k,"\"",sep=""),paste(constraints_name,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_min_constraint_val",k,"\"",sep=""),paste(min_constraints_value,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_max_constraint_val",k,"\"",sep=""),paste(max_constraints_value,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("step_constraint",k,"\"",sep=""),paste(step_constraints_value,"\"",sep=""),unlist(xml_data_use))
        xml_data_use <- gsub(paste("my_slack",k,sep=""),constraints_slack,unlist(xml_data_use))


      }


      #delete the unused constraints
      if(total_n_constraints < 6){
        diff_constraints <- 6-total_n_constraints
        position_unused <- (1:6)
        position_unused <- tail(position_unused,diff_constraints)

        for(q in min(position_unused):max(position_unused)){
          xml_data_use <- gsub(paste("<Constraint Field=\"my_constraint",q,"\""," MinValue=\"my_min_constraint_val",q,"\""," MaxValue=\"my_max_constraint_val",q,"\""," MinField=\"\" MaxField=\"\" Step=\"step_constraint",q,"\""," Slack=\"my_slack",q,"\" />",sep=""),"",unlist(xml_data_use))
        }
      }



      #and or or



      if(length(constraints_logic)!=2){
        stop("constraints_logic has to have two elements. The first has to be single_value or multiple_value, and the second and or or.")
      }

      if(constraints_logic[1]!="single_value" & constraints_logic[1]!="multiple_value"){
        stop("constraints_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(constraints_logic[2]!="and" & constraints_logic[2]!="or"){
        stop("constraints_logic has to be single_value or multiple_value followed by and or or.")
      }

      if(constraints_logic[2]=="and"){
        constraints_logic_val <- 0
      }

      if(constraints_logic[2]=="or"){
        constraints_logic_val <- 1
      }

      xml_data_use <- gsub("ConstraintLogic=\"0\"", paste("ConstraintLogic=\"",constraints_logic_val,"\"", sep=""),unlist(xml_data_use))


      if(constraints_logic[1]=="single_value"){
        constraints_logic_single <- 1
      }


      if(constraints_logic[1]=="multiple_value"){
        constraints_logic_single <- 0
      }

      xml_data_use <- gsub("ConstraintSingleValue=\"1\"", paste("ConstraintSingleValue=\"",constraints_logic_single,"\"", sep=""),unlist(xml_data_use))



      constraints_value <- seq(from=as.numeric(min_constraints_value),to=as.numeric(max_constraints_value),by=as.numeric(step_constraints_value))


    }
  }








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

  list_objectives <- data.frame()
  for (k in 1:total_n_objective){

  position_obj <- (k-1)*5
  xml_data_use <- gsub(paste("<Objective Field=\"my_objective_",k,"\"",sep=""),paste("<Objective Field=\"",objectives[position_obj+1],"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub(paste("type_my_objective_",k,sep=""),objectives[position_obj+2],unlist(xml_data_use))
  xml_data_use <- gsub(paste("minweight_my_objective_",k,sep=""),objectives[position_obj+3],unlist(xml_data_use))
  xml_data_use <- gsub(paste("maxweight_my_objective_",k,sep=""),objectives[position_obj+4],unlist(xml_data_use))
  xml_data_use <- gsub(paste("step_my_objective_",k,sep=""),objectives[position_obj+5],unlist(xml_data_use))

  list_objectives <- rbind(list_objectives,objectives[position_obj+1])

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


  if(!missing(threshold) & missing(threshold_logic)){
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
  subunit_field_exists <- FALSE

  if (!missing(subunit_field)){
    xml_data_use <- gsub(paste("SubunitsEnable=\"0\"",sep=""),"SubunitsEnable=\"1\"",unlist(xml_data_use))
    xml_data_use <- gsub(paste(" SubunitsField=\"\"",sep=""),paste("SubunitsField=\"",subunit_field,"\"",sep=""),unlist(xml_data_use))
    subunit_field_exists <- TRUE
  }


  if (missing(master_subunit)){
    xml_data_use <- gsub(paste("NestedSubunitsEnable=\"1\"",sep=""),"NestedSubunitsEnable=\"0\"",unlist(xml_data_use))
    #xml_data_use <- gsub(paste("NestedSubunitsEnable=\"1\"",sep=""),"NestedSubunitsEnable=\"0\"",unlist(xml_data_use))
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
  # date_time<-Sys.time()
  # while((as.numeric(Sys.time()) - as.numeric(date_time))<5){
  #   #print("waiting")
  # }

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

    if(length(output_csv_run_use)==1){
      stnd_results <- read.csv(paste(path_with_results,output_csv_run_use,sep="/"))
    }

    if(length(output_csv_run_use)>1){
      stnd_results <- data.frame()
      for(e in 1:length(output_csv_run_use)){
        stnd_results_e <- read.csv(paste(path_with_results,output_csv_run_use[e],sep="/"))
        get_name_stnd <- output_csv_run_use[e]
        stnd_results_e$file_name <-get_name_stnd
        stnd_results<-rbind(stnd_results,stnd_results_e)
      }

    }

    #if(length(output_csv_run_use)!=1){
    #  stop("Could not load ForSysX results in R. Multiple files match the name given. Please use a unique name in outputs_base_name or load the csv results manually.")
    #} else {
    #  stnd_results <- read.csv(paste(path_with_results,output_csv_run_use,sep="/"))
    #}

    suppressWarnings(assign(paste(last_name,"_stnd_results",sep=""),stnd_results,pos = 1)) #,pos = 1
    }


    suppressWarnings(assign(paste(last_name,"_prj_results",sep=""),prj_results,pos = 1)) #,pos = 1


  }






  #plot the results if the shapefile is saved

  #get the number of shapefiles exported#

  all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

  all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
  all_elements_use <- as.character(all_elements_use)

  path_with_results <- paste(all_elements_use, collapse = '/')


  last_name <- all_elements[,ncol(all_elements)]


  number_scenarios_created <- intersect(list.files(path_with_results,pattern = paste(as.numeric(constraints_value),collapse='|',sep="")), list.files(path_with_results,pattern = last_name)) #".shp$"

  number_scenarios_created <- number_scenarios_created[grepl(pattern = ".shp$", x = number_scenarios_created)]

  number_scenarios_created <- number_scenarios_created[!grepl(pattern = "_stand_data.shp$", x = number_scenarios_created)]





  if(build_report==TRUE | build_interac_report==TRUE){



    #plot the attainment for report


    #get table with inputs for markdown from the summary file written

    output_summary_run = intersect(list.files(path_with_results, "Summary.txt$"), list.files(path_with_results,pattern = last_name))



    all_text_summary<-readtext::readtext(paste(path_with_results,output_summary_run,sep="/"), text_field = "texts")

    all_text_summary<-all_text_summary$text

    all_text_summary <- gsub("\n","",all_text_summary)
    all_text_summary <- gsub("\t"," ",all_text_summary)



    if(length(number_scenarios_created) == 1){
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




        plot_attainment_per_project <- (suppressWarnings(suppressMessages(ggplot2::ggplot(df_loop_effects_final,aes(x=ProjectNumber,y=effect,color=effect_name))+
                                                                            geom_point()+
                                                                            geom_line(linewidth=1)+
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
                                                                                #scale_x_continuous(breaks = 1:max(df_loop_effects_final$ProjectNumber))+
                                                                                scale_color_manual(values = as.vector(pals::brewer.dark2(length(unique(df_loop_effects_per_proj_final$effect_name)))),
                                                                                                  name = "Effect name") +
                                                                                xlab("Area treated")+
                                                                                ylab("Cumulative objective attainment")+
                                                                                labs(color = "Effect name",tag="b)")+
                                                                                theme_classic())))

        suppressWarnings(assign("plot_attainment_per_project_cum",plot_attainment_per_project_cum,pos = 1))





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
    if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){


      all_elements <- stringr::str_split(outputs_base_name, "/", simplify=T)

      all_elements_use <- all_elements[,1:(ncol(all_elements)-1)]
      all_elements_use <- as.character(all_elements_use)

      path_with_results <- paste(all_elements_use, collapse = '/')

      #list patterns
      #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

      last_name <- all_elements[,ncol(all_elements)]
      #output_shp_run <- list.files(path_with_results,pattern = paste(as.numeric(constraints_value),".shp$",sep=""))

      output_shp_run = intersect(list.files(path_with_results,pattern = paste(as.numeric(constraints_value),collapse='|',".shp$",sep="")), list.files(path_with_results,pattern = last_name))


      output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"),quiet=TRUE)



      if(class(input_shapefile)[1]=="character"){
        my_shp <- sf::st_read(input_shapefile,quiet=TRUE)
      }



      my_shp$diss <- 1


      my_shp$sub_id_package <- 0

      if(subunit_field_exists == TRUE){
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




    }
  }



  if(build_report==TRUE){


    #plot projects in report

    if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
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







    if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
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
    my_list_input_variable<-get_input_names_long()


    all_matching_set_forsysx_run <- grep("set_forsysx_run", my_list_input_variable$Last.command)
    #select the highest - most recent command

    most_recent_set_forsysx_run <- max(all_matching_set_forsysx_run)

    commands_given_print <- my_list_input_variable$Last.command[most_recent_set_forsysx_run:length(my_list_input_variable$Last.command)]

    suppressWarnings(assign("commands_given_print",commands_given_print,pos = 1))

    suppressWarnings(assign("last_name",last_name,pos = 1)) #,pos = 1

    suppressWarnings(assign("build_interac_report",build_interac_report,pos = 1)) #,pos = 1

    path_for_rmd <- system.file("rmd_template", package = "ForSysXR")
    #setwd(system.file("rmd_template", package = "ForSysXR"))
    save.image (file = paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))

    #rmarkdown::render("testing_2.Rmd")





    cat("Generating htlm report",'\n')
    setwd(path_with_results)
    #suppressWarnings(suppressMessages(generate_report()))
    capture.output(suppressWarnings(suppressMessages(generate_report(output_file=paste("report_",last_name,".html",sep="")))))


  }



  if(build_interac_report==TRUE){



    #plot projects in report

    if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
      if(length(number_scenarios_created) == 1){


        output_shp_run_wgs84 <-st_transform(output_shp_run_diss,crs = 4326)
        output_shp_run_treat_wgs84 <-st_transform(output_shp_run_diss_treat,crs = 4326)
        my_shp_diss_line_wgs84 <-st_transform(my_shp_diss_line,crs = 4326)




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







    if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
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
    my_list_input_variable<-get_input_names_long()


    all_matching_set_forsysx_run <- grep("set_forsysx_run", my_list_input_variable$Last.command)
    #select the highest - most recent command

    most_recent_set_forsysx_run <- max(all_matching_set_forsysx_run)

    commands_given_print <- my_list_input_variable$Last.command[most_recent_set_forsysx_run:length(my_list_input_variable$Last.command)]

    suppressWarnings(assign("commands_given_print",commands_given_print,pos = 1))

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
    if(load_results==FALSE){
      suppressWarnings(rm(list = ls()[grep("prj_results", ls())], envir = globalenv()))
      #rm(list = ls()[grep("last_name", ls())], envir = globalenv())

    }
  }





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

      output_shp_run = intersect(list.files(path_with_results,pattern = paste(as.numeric(constraints_value),collapse='|',".shp$",sep="")), list.files(path_with_results,pattern = last_name))


      output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"),quiet=TRUE)

      if (missing(my_shp)){
        my_shp <- sf::st_read(input_shapefile,quiet=TRUE)
      }


      my_shp$diss <- 1


      my_shp$sub_id_package <- 0

      if(subunit_field_exists == TRUE){
        my_shp$sub_id_package <- my_shp[,paste(subunit_field)][[1]]
      }



      my_shp_subunits <- my_shp %>%
        group_by(sub_id_package) %>%
        summarise(m = mean(sub_id_package)) %>%
        st_cast()

      my_breaks<-max(output_shp_run$ProjectNum)/4

      my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


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
        scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
        geom_sf(data=my_shp_subunits,color="black",fill=NA)+
        labs(fill='Project number')

      return(ttt)
    }

  }



  if(any(grepl("shapefile", save_outputs, fixed = TRUE))==TRUE){
    if(plot_results==TRUE & length(number_scenarios_created) > 1){

      output_shp_run = intersect(list.files(path_with_results,pattern = paste(as.numeric(constraints_value),collapse='|',".shp$",sep="")), list.files(path_with_results,pattern = last_name))


      my_shp$sub_id_package <- 0

      if(subunit_field_exists == TRUE){
        my_shp$sub_id_package <- my_shp[,paste(subunit_field)][[1]]
      }



      my_shp_subunits <- my_shp %>%
        group_by(sub_id_package) %>%
        summarise(m = mean(sub_id_package)) %>%
        st_cast()


      #output_shp_run <- sf::st_read(paste(path_with_results,output_shp_run,sep="/"))

      plot_data <- list()
      all_name_output_shiny <- data.frame()


      for(e in 1:length(number_scenarios_created)){
        output_shp_run_plot <- sf::st_read(paste(path_with_results,output_shp_run[[e]],sep="/"),quiet=TRUE)

        name_output_shiny <- output_shp_run[[e]]


        my_breaks<-max(output_shp_run$ProjectNum)/4

        my_breaks<-round(c(my_breaks,my_breaks*2,my_breaks*3,my_breaks*4),0)


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
          scale_fill_viridis_c(option = "turbo",direction=-1,breaks=my_breaks)+
          geom_sf(data=my_shp_subunits,color="black",fill=NA)+
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
        output$current_plot <- shiny::renderPlot({
          print(plot_data[[current_plot_index()]])
        })

        # Function to render the current plot number text
        output$plot_number <- shiny::renderText({
          paste("Plot", current_plot_index(), "of", length(plot_data))
        })

        # Function to render the current plot title
        output$plot_title <- shiny::renderText({
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




  ####clean-up unecessary files#####
  file.remove(paste(path_for_rmd,"my_work_space_vs2.RData",sep="/"))
  suppressWarnings(rm(list = ls()[grep("commands_given_print", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("last_name", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("plot_attainment_per_project", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("plot_attainment_per_project_cum", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("plot_perc_treated_constraint_in_proj", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("plot_treated_constraint", ls())], envir = globalenv()))

  suppressWarnings(rm(list = ls()[grep("introduction_table", ls())], envir = globalenv()))
  suppressWarnings(rm(list = ls()[grep("my_plot_projects_report", ls())], envir = globalenv()))

  (rm(list = ls()[grep("attainment_fig_constraint1", ls())], envir = globalenv()))
  (rm(list = ls()[grep("caption_fig_constraint1", ls())], envir = globalenv()))
  (rm(list = ls()[grep("caption_fig1", ls())], envir = globalenv()))




  #rm(commands_given_print)


  #AQUI REMOVER OS STANDS E PROJS!
  if(load_results==FALSE){
    suppressWarnings(rm(list = ls()[grep("prj_results", ls())], envir = globalenv()))
    #rm(list = ls()[grep("last_name", ls())], envir = globalenv())

  }



  }


