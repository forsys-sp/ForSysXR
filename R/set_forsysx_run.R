set_forsysx_run <- function(input_shapefile,
                            outputs_base_name,
                            stand_id,area,
                            #available_for_management,
                            available,
                            #seed_stands_only_available_stands,
                            #exclude_stands,
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
                            constraints_name,
                            constraints_value,
                            constraints_slack,
                            effect_fields,
                            objectives,
                            output_xml,
                            run_forsysx,
                            exe_path #,xml_path
                            ) {

  l <- list(base::missing_arg())

  input_shapefile_format <- substrRight(input_shapefile,4)

  if(input_shapefile_format!= ".shp")
    stop("input_shapefile has to be a shapefile!")


  stopifnot

  #warning if default definitions are used for some parameters. And say that the
  #user should see the manual to understand if it is doing what they want

  #input_shapefile <- "C:/Users/almeidbr/Desktop/umatilla/stands_with_distance_to_FBN_projects/changed_availability_after_implementing_projects_stands_vs2/Umatilla_Restoration_08282023_with_distance_to_FBN_linear_Rev_proj_1_vs2.shp"
  input_shapefile_use <- gsub("/","\\\\\\\\",input_shapefile)

  outputs_base_name_use <- gsub("/","\\\\\\\\",outputs_base_name)

  adjacency_matrix_use <- gsub("/","\\\\\\\\",adjacency_matrix)


  data("xml_data")
  xml_data_use <- gsub("my_input_shapefile.shp",input_shapefile_use,unlist(xml_data)) #input_shapefile_use
  xml_data_use <- gsub("my_output_name",outputs_base_name_use,unlist(xml_data_use))
  xml_data_use <- gsub("my_adjacency_file.csv",adjacency_matrix_use,unlist(xml_data_use)) #input_shapefile_use
  xml_data_use <- gsub("my_stand_id",stand_id,unlist(xml_data_use))
  xml_data_use <- gsub("my_area_field",area,unlist(xml_data_use))

  #available_for_management -falta fazer este
  xml_data_use <- gsub("my_availability_field",available,unlist(xml_data_use))

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



  xml_data_use <- gsub("<Objective Field=\"my_objective_1\"",paste("<Objective Field=\"",objectives[1],"\"",sep=""),unlist(xml_data_use))
  xml_data_use <- gsub("type_my_objective_1",objectives[2],unlist(xml_data_use))
  xml_data_use <- gsub("minweight_my_objective_1",objectives[3],unlist(xml_data_use))
  xml_data_use <- gsub("maxweight_my_objective_1",objectives[4],unlist(xml_data_use))
  xml_data_use <- gsub("step_my_objective_1",objectives[5],unlist(xml_data_use))


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




input_shapefile <- "D:/umatilla/stands/stands_with_distance_to_FBN_projects/Umatilla_Restoration_08282023_with_distance_to_FBN_linear_Rev_proj_1.shp"
outputs_base_name <- "D:/umatilla/stands/stands_with_distance_to_FBN_projects/dummy_run/dummy_run_forsys"
stand_id <- "ID_forsys"
area <- "area_ha"
#available_for_management <- "avail_fin"
available <- "avail_fin"
#seed_stands_only_available_stands,
#exclude_stands,
exclude_field <- "Exclude"
#load_objective_steps,
#step_file,
#objective_direction,
#spatial_optimization,
x_coordinate <- "Point_X"
y_coordinate <- "Point_Y"
seed_stand_percent <- 20
max_project_diameter <- -1
max_number_projects <- 1
#patch_buster,
#weight,
#patch_identifier,
inverse_distance_power <-0
#maximize_distance,
adjacency_matrix <- "D:/umatilla/stands/stands_with_distance_to_FBN_projects/adj_umatilla_FBN_and_restoration_ID_forsys.csv"
constraints_name <- "area_ha"
constraints_value <- "4,000.00"
constraints_slack <- "5.00"
effect_fields <- c("NrevA_SPM","NrevA_PCP", "Nrev_nor_a", "NetR_norm","pSDIa_SPM","pSDIa_PCP","pmaxSDI_a","permaxSDI")
objectives <- c("NrevA_SPM","Treat","1","1","1")
output_xml <- "C:/Users/aparicio/Desktop/ForSysXR/test_700.xml"
run_forsysx <- 1
exe_path <- "C:/Users/aparicio/Desktop/forsys/ForSysXConsole.exe"
