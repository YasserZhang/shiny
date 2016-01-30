library(shiny)
library(ggmap)
library(leaflet)
library(FNN)
library(ggplot2)
library(rdrop2)
library(DT)
token <- drop_auth()
allSales <- drop_read_csv("remote_storage/alldataforshinyapp.csv",header = T, stringsAsFactors = FALSE)

outputDir <- "remote_storage"
saveData <- function(data) {
    # Create a unique file name
    fileName <- sprintf("alldataforshinyapp.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, dest = outputDir)
}
##loading the data
#allSales <- read.csv("alldataforshinyapp.csv",header=T,stringsAsFactors = F)
#create the dataset for linear model training
features <- c("zip_code", "log_land", "log_gross_sq")
output <- 'log_price'

#create new subset of data according to the borough users choosed
data_filter <- function(borough){
    allSales[allSales$BOROUGH == as.character(borough),]
}

#set several categories for ui side, namely, neighborhood and zip code.
neighborhood <- function(borough){
    newdata <- data_filter(borough)
    neighborhood_list <- as.list(unique(as.character(newdata$NEIGHBORHOOD)))
    return(neighborhood_list)
}
#neighborhood_list <- as.list(unique(as.character(allSales$NEIGHBORHOOD)))
zipcode <- function(borough){
    newdata <- data_filter(borough)
    zipcode_list <- as.list(unique(as.character(newdata$zip_code))[order(unique(newdata$zip_code))])
}
create_dataframe <- function(borough, neighborhood,address,zipcode,res, com, land, gross){
    if(class(zipcode) == "NULL"){
        zipcode = "11377"
    } 
    if (!is.character(neighborhood)){
        neighborhood = "WOODSIDE"
    }
    input_array <- c(borough, neighborhood,address,zipcode,res, com, land, gross)
    input_array <- matrix(input_array, nrow=1, ncol=8)
    t <- data.frame(input_array)
    names(t) <- c("Borough","Neighborhood", "Address","Zipcode","Red_Units","Comm_Units", "Land(SqFt)", "Gross(SqFt)")
    return(t)
}

#fit the linear model
#predict the property of interest
lm_prediction <- function(borough, zipcode, land, gross){
    lm_data <- data_filter(borough)
    if (class(zipcode) == "NULL"){
        zipcode = as.character(unique(lm_data$zip_code)[1])
    }
    lm_data <- lm_data[,c(features,output)]
    lm_data$zip_code <- as.character(lm_data$zip_code)
    test_data <- lm_data[,features][1,]
    test_data$zip_code <- as.character(zipcode)
    test_data$log_land <-  log(land)
    test_data$log_gross_sq <- log(gross)^2
    #list(str(test_data), str(new_data))
    lmFit <- lm(log_price ~ ., data = lm_data)
    predictions <- exp(predict(lmFit, test_data, type = "response", interval = "prediction"))
    predictions <- as.data.frame(predictions)
    names(predictions) = c("Estimated Price in US Dollars","Lower Bound", "Upper Bound")
    predictions
}


##KNN model
#choose the feature collumns used in the knn model
features_knn <- c("RESIDENTIAL.UNITS", "COMMERCIAL.UNITS", "zip_code", "log_land", "log_gross",  "log_gross_sq", "log_land_sq")
#fit the k nearest neighbors model
knn_prediction <- function(borough, input_res, input_com, input_zipcode,input_land, input_gross, input_k){
    #create customized dataset
    newdata = data_filter(borough)
    #check the class of zip code value
    if (class(input_zipcode) == "NULL"){
        input_zipcode = unique(newdata$zip_code)[1]
    }
    #prepare test data and labels
    labels <- newdata$log_price
    knn_data = newdata[,features_knn]
    knn_data$zip_code <- as.numeric(knn_data$zip_code)
    test_knn <- knn_data[1,]
    test_knn$RESIDENTIAL.UNITS = input_res
    test_knn$COMMERCIAL.UNITS = input_com
    test_knn$zip_code = as.numeric(input_zipcode)
    test_knn$log_land = log(input_land)
    test_knn$log_gross = log(input_gross)
    test_knn$log_gross_sq = test_knn$log_land^2
    test_knn$log_gross_sq = test_knn$log_gross^2
    #fit the knn model
    kFit <- knn(knn_data,test_knn[1,], labels, k = input_k, algorithm = "brute")
    indices = attr(kFit, "nn.index")
    newdata[indices,]
}

#choose the feature columns to show up in the UI
show_features <- c("BOROUGH","NEIGHBORHOOD",
                   "ADDRESS", "ZIP.CODE", "RESIDENTIAL.UNITS", 
                   "COMMERCIAL.UNITS", "LAND.SQUARE.FEET",
                   "GROSS.SQUARE.FEET", "YEAR.BUILT",
                   "SALE.PRICE", "SALE.DATE")
#generate the k nearest neighbors of the property of interest to show up in the UI in the form of table
knn_show_data <- function(knn_result){
    show_data <- knn_result
    show_data <- show_data[,show_features]
    names(show_data) <- c("Borough","Neighborhood",
                          "Address", "Zipcode", "Resd_Units",
                          "Comm_Units", "Land(SqFt)",
                          "Gross(SqFt)", "Year.Built",
                          "Sale", "Date")
    return(show_data)
}

#generate geolocation information for the k nearest neighbors and output an updated data frame
map_data <- function(knn_result){
    map_data <- knn_result[,c("X", "ADDRESS","sale_price", "comp_add", "longitude","latitude")]
    map_data$labels <- as.character(knn_result$SALE.PRICE)
    if (sum(is.na(map_data$longitude)) > 0){
        sq <- is.na(map_data$longitude)
        m1 <- map_data[!sq,]
        m2 <- map_data[sq,]
        loc <- sapply(m2$comp_add, geocode)
        m2$longitude <- as.numeric(loc[1,])
        m2$latitude <- as.numeric(loc[2,])
        map_data <- rbind(m2,m1)
        map_data
    }
    else{
        map_data
    }
}

#generate the data of the property of insterest for marking in the map
map_my_property <- function(borough, address, neighborhood){
    comp_add <- paste(address,", ", neighborhood, ", ", borough, ", NY, United States", sep = "")
    loc <- geocode(comp_add, messaging = FALSE)
    loc <- as.numeric(loc)
    my_map <- data.frame(ADDRESS = address, longitude =loc[1], latitude= loc[2])
}
#update geolocation information in the original data, when some entries are requested as nearest neighbors,
#their geolocation are generated in the function of map_data and recorded into the original dataset allSales in the function belwo.
write_in_data <- function(knn_result){
    #check if geocloation in the original dataset already exist, and pass the function if yes.
    geo_data <- map_data(knn_result)
    s <- sum(is.na(allSales[allSales$X %in% geo_data$X, ]$longitude))
    if (s>0){
        ord1 <- order(geo_data$X)
        ord2 <- order(allSales[allSales$X %in% geo_data$X, ]$X)
        q <- is.na(allSales[allSales$X %in% geo_data$X, ][ord2,]$longitude)
        allSales[allSales$X %in% geo_data$X, ][ord2,][q,]$longitude = geo_data[ord1,][q,]$longitude
        allSales[allSales$X %in% geo_data$X, ][ord2,][q,]$latitude = geo_data[ord1,][q,]$latitude
        #save to remote storage
        saveData(allSales)
        #print out the updated rows to check out the correstness of the long and lat data
        f <- c("X","ADDRESS","longitude","latitude")
        rbind(allSales[allSales$X %in% geo_data$X,f], geo_data[,f])
    }
}


icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue',
                                  iconColor = 'black', library = 'glyphicon')
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'lightblue', library='fa',
                           iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green',
                            library='ion')


shinyServer(
    function(input,output,session){
        #(ADDRESS HERE)
        #BOROUGH
        output$choose_borough <- renderText({input$borough})
        #NEIGHBORHOOD
        output$choose_neighborhood <- renderUI({
            neighborhood_list <- neighborhood(input$borough)
            selectInput("neighborhood", "Choose A Neighborhood", choices = neighborhood_list, selected = "WOODSIDE")
        })
        output$place <- renderText({input$neighborhood})
        #ZIP CODE
        output$choose_zipcode <- renderUI({
            zipcode_list <- zipcode(input$borough)
            selectInput("zip_code", "Select the Zip Code. This Is Important!", choices = zipcode_list, selected="11377")
        })
        output$zipcode <- renderText({input$zip_code})
        #LAND SQUARE FEET
        output$land <- renderPrint({input$land_sqft})
        #GROSS LAND SQUARE FEET
        output$gross <- renderPrint({input$gross_sqft})
        #RESIDENTIAL UNITS
        output$residential <- renderPrint({input$resident})
        #COMMERCIAL UNITS
        output$commercial <- renderPrint({input$commerce})
        #(ACTION BUTTON HERE)
        #GENERAL VIEW OF ALL SALES ON THE NYC MAP
        #output$general_view_map <- renderLeaflet({
         #   leaflet(Brooklyn) %>% addTiles %>% addCircleMarkers(radius = ~sale_price/1000000,popup = ~paste(ADDRESS,"\n", ", PRICE: ", labels, sep=""))
        #})
        #SHOW THE PROPERTY OF INTEREST IN THE FORM OF TABLE
        output$test_input <- DT::renderDataTable({
            input$loadIn
            dt <- isolate(create_dataframe(input$borough,input$neighborhood,
                                 input$address,input$zip_code,
                                 input$resident, input$commerce,
                                 input$land_sqft, input$gross_sqft))
            action <- DT::dataTableAjax(session,dt)
            DT::datatable(dt, options = list(ajax = list(url = action), dom='t'),
                          escape = FALSE)
            
        })
        output$text_isThis <- renderText({
            input$loadIn
            isolate("Please check out if the property displayed in the table and the map below is the one you want to evaluate. If everything is Correct, click the Estimate Button.")
        })
#         output$test_input <- renderPrint({
#             class(input$neighborhood)
#         })
        #OUTPUT LINEAR MODEL PREDICTION
        output$lm_prediction <- renderTable({
            input$goButton
            isolate(lm_prediction(borough = input$borough, zipcode = input$zip_code, land = input$land_sqft,gross = input$gross_sqft))
            })
        #SHOW THE K NEAREST NEIGHBORS IN THE FORM OF TABLE
        output$knn_prediction <- DT::renderDataTable({
            input$goButton
            input$showButton
            knn_data <- isolate(knn_prediction(borough = input$borough,
                                               input_res = input$resident,
                                               input_com = input$commerce,
                                               input_zipcode = input$zip_code,
                                               input_land = input$land_sqft,
                                               input_gross = input$gross_sqft,
                                               input_k = input$knumber))
            df <- knn_show_data(knn_data)
            action <- DT::dataTableAjax(session,df)
            DT::datatable(df, options = list(ajax = list(url = action)),
                          escape = FALSE)
        })
        #SHOW THE DATA OF NEAREST NEIGHBORS AND MARK THE PROPERTY OF INTEREST
        output$knn_map <- renderLeaflet({
            input$goButton
            input$showButton
            show_data <- isolate(knn_prediction(borough = input$borough,
                                                input_res = input$resident,
                                                input_com = input$commerce,
                                                input_zipcode = input$zip_code,
                                                input_land = input$land_sqft,
                                                input_gross = input$gross_sqft,
                                                input_k = input$knumber))
            show_data <- map_data(show_data)
            my_property <- isolate(map_my_property(input$borough,input$address, input$neighborhood))
            leaflet(show_data) %>%
                addTiles %>%
                addAwesomeMarkers(popup = ~paste(comp_add,"\n", ", PRICE: ", labels, sep=""),icon = icon.fa) %>%
                addAwesomeMarkers(lng=my_property$longitude, lat=my_property$latitude, popup=my_property$ADDRESS, icon = icon.ion)
                               
        })
        #WRITE IN THE GEOLOCATION INFORMATION INTO THE ORIGINAL DATABASE IN THE BACKGROUND
        output$writeindata <- renderPrint({
            #write in new location data into the original file, danger!!!
            input$goButton
            input$showButton
            show_data <- isolate(knn_prediction(borough = input$borough,
                                        input_res = input$resident,
                                        input_com = input$commerce,
                                        input_zipcode = input$zip_code,
                                        input_land = input$land_sqft,
                                        input_gross = input$gross_sqft, input_k = input$knumber))
            write_in_data(show_data)
        })
        
    }
)

