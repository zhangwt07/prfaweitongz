## QUESTION ----

#' Offence counts in Adelaide for different offence level in postcode/suburb
#'
#' \code{crime_analysis_no_titles} <The function shows the trend of offence counts in adelaide different
#'        postcode/suburb areas of a kind of offence description in level 1 or 3>
#' @param crime_data A data.table object with the following columns with the exact same name:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of offence description in level 1 or 3.
#' @param args is either postcode or suburb.
#'        postcodes A two-element character vector. Each element is an SA postcode, eg c(5000,5022).
#'        suburb A two-element character vector, each is a suburb name
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes/suburbs.
#'          WITHOUT TITLES
#' @examples
#' crime_analysis(some_data,"OFFENCES AGAINST PROPERTY",c(5173,5114))
#' shows the offence counts in postcode 5173 and 5114 for level 3 "OFFENCES AGAINST PROPERTY"
#' crime_analysis(some_data,"ACTS INTENDED TO CAUSE INJURY",c("ABERFOYLE PARK","ALDINGA BEACH"))
#' shows the offence counts in suburb aberfoyle park and aldinga beach for level 1 "ACTS INTENDED TO CAUSE INJURY"

crime_analysis_no_titles <- function(crime_data, offence_description, args) {
  require(data.table)
  require(ggplot2)

  # if the args elements are integer, they are postcodes
  if (is.numeric(args[1])){
    flag <- "postcode"
    postcodes <- args
    # Error catching
    # check if entered 2-element vector postcodes, like c(5000,5001)
    if (length(postcodes)!=2) {
      stop("Please enter two postcodes")
    }
  }else{
    # else they are suburb
    flag <- "suburb"
    suburbs <- args
    if (length(suburbs)!=2) {
      stop("Please enter two suburbs")
    }
  }




  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  if (!all.equal(colnames(crime_data),expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  #record the year
  year <- substr(crime_data$date[1],1,4)

  # check offence description exist in crime_data, find level 1 or 3
  if ( !offence_description %in% crime_data$offence_level_1){
    if ( !offence_description %in% crime_data$offence_level_3){
      stop("Please enter correct offence level 1 or 3 description")
    }else{
      level <- 3
    }
  }else{
    level <- 1
  }


  # Check that the input postcodes/suburbs exist in crime_data
  if (flag=="postcode"){
    if (any(!postcodes %in% crime_data$postcode)) {
      stop("Please enter correct postcodes")
    }
  }else if(flag=="suburb"){
    if (any(!suburbs %in% crime_data$suburb)) {
      stop("Please enter correct suburbs")
    }
  }


  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "postcode", "total_offence_count"
  if(flag=="postcode"){
    if(level==1){
      plot_data <- crime_data[((postcode %in% args)&offence_description %in% offence_level_1),
                              list(total_offence_count = sum(offence_count)),by=list(postcode,month(date))]
    }else{
      plot_data <- crime_data[((postcode==args[2]|postcode==args[1])&offence_level_3==offence_description),
                              .(total_offence_count = sum(offence_count)),by=list(postcode,month(date))]
    }
  }else{
    if(level==1){
      plot_data <- crime_data[((suburb==args[2]|suburb==args[1])&(offence_level_1==offence_description)),
                              .(total_offence_count = sum(offence_count)),by=list(suburb,month(date))]
    }else{
      plot_data <- crime_data[((suburb==args[2]|suburb==args[1])&offence_level_3==offence_description),
                              .(total_offence_count = sum(offence_count)),by=list(suburb,month(date))]
    }
  }

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  if (flag=="postcode"){
    #if is postcode
    plot_data[, postcode := plyr::mapvalues(postcode, args, c("x", "y"))]
    plot_data <- dcast(plot_data, month ~ postcode, fun = sum,
                       fill = 0, value.var = "total_offence_count")
    plot_data$month = factor(plot_data$month,level=c(7:12,1:6))

    # Generate the plot
    Postcode <- factor(args[1])
    Postcode_1<-factor(args[2])
    res<-ggplot(plot_data,aes(x=month,group=1))+
      geom_line(aes(y=plot_data$x,colour = Postcode))+
      geom_line(aes(y=plot_data$y,colour = Postcode_1))+
      labs(x = paste("Month in",year,"-",(as.numeric(year)+1),sep=" "),
           y = "Offence Count",
           title = paste("Offence Counts of",offence_description,"in Adelaide",sep=" "),
           subtitle = paste("Offence counts level",level,"between",flag,args[1],
                            "and",args[2],"from year",year,"-",(as.numeric(year)+1),sep=" "))
  }else{
    #if is suburb
    plot_data[, suburb := plyr::mapvalues(suburb, args, c("x", "y"))]
    plot_data <- dcast(plot_data, month ~ suburb, fun = sum,
                       fill = 0, value.var = "total_offence_count")
    plot_data$month = factor(plot_data$month,level=c(7:12,1:6))

    # Generate the plot
    Suburb <- factor(args[1])
    Suburb_1<-factor(args[2])
    res<-ggplot(plot_data,aes(x=month,group=1))+
      geom_line(aes(y=plot_data$x,colour = Suburb))+
      geom_line(aes(y=plot_data$y,colour = Suburb_1))+
      labs(x = paste("Month in",year,"-",(as.numeric(year)+1),sep=" "),
           y = "Offence Count")
  }
  return(res)
}
