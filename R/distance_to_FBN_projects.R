
distance_to_FBN_projects <- function(my_stands,FBN_projects,output_folder){

  my_stands_use <- sf::st_read(my_stands)

  #select only the restoration and not the FBN
  #head(my_stands_use)

  #get the position of the variable with FBN projects
  FBN_porj_position <- grep(FBN_projects, colnames(my_stands_use))




  names(my_stands_use)[FBN_porj_position] <- "FBN_proj"


  my_linear_projs <-  subset(my_stands_use,FBN_proj!=0)


  #get all projs
  for(i in 1:max(my_stands_use$FBN_proj,na.rm = TRUE)){
    my_linear_projs_loop <- subset(my_stands_use,FBN_proj==i)
    #plot(my_linear_projs_loop[,1])



    # create an index of the nearest feature
    index <- st_nearest_feature(x = my_stands_use, y = my_linear_projs_loop)

    # slice based on the index
    my_FBN_2 <- my_linear_projs_loop %>% slice(index)

    # calculate distance between polygons
    poly_dist <- as.numeric(st_distance(x = my_stands_use, y= my_FBN_2, by_element = TRUE))

    #length(poly_dist)

    # add the distance calculations to the fire polygons
    #nrow(my_stands_restoration)
    my_stands_use$distance <- poly_dist

    #nrow(my_stands_restoration)
    #length(unique(my_stands_restoration$ID_forsys))


    #my_FBN$distance <- 0


    my_stands_final <- my_stands_use
    #my_stands_final$distance
    #head(my_stands_final)


    #create a temp folder
    dir.create(file.path(output_folder, "temp_folder_shp"), showWarnings = FALSE)


    st_write(my_stands_final,paste(output_folder, "/temp_folder_shp/","interactive_zones_",i,".shp",sep=""))


  }
}
