# A method to handle downloading files through s3
# Expects to be given 
# - An AWS profile
# - An AWS bucket string
# - An AWS credentials file path (optional if default)
# - An s3_read function, to define what to do with the s3 file

expected_params = c("profile", "bucket")

loader_s3 = function(..., filepath_credentials = aws.signature::default_credentials_file()) {
  
  params = as.list(...)
  
  # Check the correct parameters have been provided in the config > extra section
  if(!all(expected_params %in% names(params)))
    stop("Please pass", paste0(expected_params, collapse = ", "), "to s3_loader")

  # Overwrite the default credentials path if a custom path is provided
  if ("filepath_credentials" %in% names(params))
    filepath_credentials = params$filepath_credentials
  
  # Make sure an s3 reader function is provided in the init_data_helper
  if (!("s3_read_function" %in% names(params)))
    stop("Please pass an 's3_read_function' to loader_generic in your init_data_helper")
  
  # If using the default credential file, we should tell people. It could be an oversight
  if (filepath_credentials == aws.signature::default_credentials_file())
    cat("INFO: Using credentials file at", filepath_credentials, "\n")
  
  profile = params$profile
  bucket = params$bucket
  s3_read_function = params$s3_read_function
  
  # Regsiter AWS credentials in the environment
  aws.signature::use_credentials(profile = profile, file = file.path(root, filepath_credentials))
  
  # Pull out a list of all objects in the bucket
  # They will all be processed
  dt_objects = aws.s3::get_bucket_df(bucket)
  
  # Read all files from S3 using the user defined s3_read_function, provided in the init_data_helper
  cat("INFO: Reading from S3, this could take some time...\n")
  
  list_result = lapply(dt_objects$Key, function(key) .s3read_using(s3_read_function, object = key, bucket = bucket))
  
  # Unset the AWS credentials from the environment and return
  unset_credentials()
  list_result

}

unset_credentials = function() {
  cat("INFO: Unsetting AWS credentials from system environmental variables\n")
  # Remove aws.signature authorisation to use ec2.metadata
  Sys.unsetenv('AWS_ACCESS_KEY_ID')
  Sys.unsetenv('AWS_SECRET_ACCESS_KEY')
}

# This is stupid, but I didn't want the random hex file name
.s3read_using = function (FUN, ..., object, bucket, opts = NULL) {
  
  if (missing(bucket)) {
    bucket = aws.s3:::get_bucketname(object)
  }
  
  object = aws.s3:::get_objectkey.character(object)
  tmp = tempfile(pattern = object, fileext = paste0(".", tools::file_ext(object)))
  
  if (is.null(opts)) {
    r = aws.s3::save_object(bucket = bucket, object = object, file = tmp)
  }
  
  else {
    r <- do.call("aws.s3::save_object", c(list(bucket = bucket, object = object, file = tmp), opts))
  }
  
  new_tmp = file.path(tempdir(), object)
  file.rename(tmp, new_tmp)
  
  return(FUN(new_tmp, ...))
}

