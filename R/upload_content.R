upload_content<-function(output_folder_web,
                         last_name_web,
                         explore=FALSE,
                         static=TRUE) {

  Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAX7YB5OT7NZW5OLO7",
             "AWS_SECRET_ACCESS_KEY" = "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",
             "AWS_DEFAULT_REGION" = "us-west-2")

  aws.s3::bucket_exists("forsysxr")

  #date_and_time <- Sys.time()
  #date_and_time <-gsub(":","",date_and_time)
  #date_and_time <-gsub("\\..*","",date_and_time)
  #date_and_time <-gsub(" ","_",date_and_time)

  # aws.s3::put_object(file = paste(output_folder_web,"/report_static_run_",last_name_web,"_",date_and_time,".html",sep=""),
  #            bucket = "forsysxr")

  #file_path <- paste(output_folder_web, "/report_static_run_", last_name_web, "_", date_and_time, ".html", sep = "")

  if(explore==TRUE){
    aws.s3::put_object(file = paste(output_folder_web,"/explore_",last_name_web,".html",sep=""),
                       bucket = "forsysxr")


    file_path <- paste(output_folder_web, "/explore_", last_name_web,".html", sep = "")
    full_url <- paste("https://forsysxr.s3-us-west-2.amazonaws.com/", basename(file_path), sep = "")

    cat(paste0("The automatic report was uploaded and can be viewd at ",full_url),'\n')
  }


  if(static==TRUE & explore==FALSE){
    aws.s3::put_object(file = paste(output_folder_web,"/report_",last_name_web,".html",sep=""),
                       bucket = "forsysxr")


    file_path <- paste(output_folder_web, "/report_", last_name_web,".html", sep = "")
    full_url <- paste("https://forsysxr.s3-us-west-2.amazonaws.com/", basename(file_path), sep = "")

    cat(paste0("The automatic report was uploaded and can be viewd at ",full_url),'\n')
  }


  if(static==FALSE & explore==FALSE){
    aws.s3::put_object(file = paste(output_folder_web,"/report_interactive_",last_name_web,".html",sep=""),
                       bucket = "forsysxr")


    file_path <- paste(output_folder_web, "/report_interactive_", last_name_web,".html", sep = "")
    full_url <- paste("https://forsysxr.s3-us-west-2.amazonaws.com/", basename(file_path), sep = "")

    cat(paste0("The automatic report was uploaded and can be viewd at ",full_url),'\n')
  }



}



