
# function to find latest date in a LID
latest_date = function(dir_path) {
    subdir_dates = list.dirs(dir_path, full.names = TRUE, recursive = FALSE)
    if (length(subdir_dates) > 0) {
        # Extract the date part and convert it to Date objects
        subdir_dates = as.Date(gsub(".*/(\\d{4}-\\d{2}-\\d{2}).*", "\\1", subdir_dates))
        latest_date_subdir = subdir_dates[which.max(subdir_dates)]
        return(file.path(dir_path, format(latest_date_subdir, "%Y-%m-%d")))
    } else {
        return(NULL)
    }
}


# Function to assign hexile to each invoice
hexile = function(amount, hexiles) {
    
    fcase(
        amount <= hexiles[2], "H1",
        amount > hexiles[2] & amount <= hexiles[3], "H2",
        amount > hexiles[3] & amount <= hexiles[4], "H3",
        amount > hexiles[4] & amount <= hexiles[5], "H4",
        amount > hexiles[5] & amount <= hexiles[6], "H5",
        default = "H6"  
    )
    
}

### Query Italian companies DB Functions -----------

assembleQuery = function(conn, base, search_parameters){
    
    parameter_names = names(search_parameters)
    partial_queries = ""
    
    # Iterate over all the parameters to assemble the query
    for(k in 1:length(parameter_names)){
        # Check if the values are numeric or not
        if(is.numeric(search_parameters[[parameter_names[k]]])){
            values = paste(search_parameters[[parameter_names[k]]], collapse = ", ")
        } else {
            # Surround each value with single straight quotes if it's not numeric
            values = paste("'", search_parameters[[parameter_names[k]]], "'", sep = "", collapse = ", ")
        }
        
        filter_k = paste(parameter_names[k], "IN (", values, ")")
        
        # If there is more than 1 parameter, add an AND statement before the parameter
        if(k > 1){
            filter_k = paste("AND", filter_k)
        }
        
        partial_queries = paste(partial_queries, filter_k)
    }
    
    # Paste all together into a single query using a WHERE statement
    final_query = paste(base, " WHERE", partial_queries)
    
    # Print the assembled query to show how it looks like
    #print(final_query)
    
    # Run the final query
    result = DBI::dbGetQuery(conn, final_query)
    
    # return the executed query
    return(result)
}