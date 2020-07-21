library(shiny)
library(tidyverse)
library(ggplot2) 
library(readxl)

######## loading state_info ##########
state_info <- read_csv("./data/state_info.csv")
# tidy
state_info <- state_info %>%
    select(-medals) %>%
    bind_rows(list(state = c("WV", "SD"), 
                   location = c("West Virginia", "South Dakota"), 
                   lat = c(39.000000, 44.500000), 
                   lon = c(-80.500000, -100.000000)))
############# function 1 #############
# function1 - tidy High School data
tidy_hs <- function(x) {
    x %>%
        slice(which(row_number() %% 2 == 0)) %>%
        mutate(state = str_extract(`ATHLETE/TEAM`, "(^(?i)[a-z][a-z])")) %>%
        separate(`ATHLETE/TEAM`, into = c("Empty", "Team"), sep = "(^(?i)[a-z][a-z])") %>% 
        select(8, 5) %>%
        mutate(state = str_to_upper(.$state)) %>%
        # column bind all the second row of data  
        cbind(
            x %>%
                slice(which(row_number() %% 2 == 1)) %>%
                select(-3) %>%
                set_names(c("Rank", "Time", "Athlete", "Grade", "Meet/Place")) %>%
                mutate(Place = str_extract(`Meet/Place`, "(\\d[a-z][a-z])$")) %>%
                mutate(`Meet/Place` = str_replace(`Meet/Place`, "(\\d[a-z][a-z])$", ""))
        ) %>%
        set_names(c("state", "Team", "Rank", "Time", 
                    "Athlete", "Grade", "Meet", "Place")) %>%
        select(3, 5, 4, 1, 2, 6, 7, 8) %>%
        # join the state info
        left_join(state_info, by = "state")
}

############# function 2 #############
# function2 - convert time to second 
count_sec <- function(x) {
    x <- x %>%
        separate(Time, into = c("minute", "second"), sep = ":")
    # set the time variable as numeric
    x$minute <- as.numeric(x$minute)
    x$second <- as.numeric(x$second)
    # calculate the time variable
    x <- x %>%
        mutate(Time = (minute * 60) + second) %>%
        select(1, 2, 13, 5, 10:12, 6:9)
    return(x)
}

######### loading hs indoor ##########
# indoor
hs_indoor18 <- "./data/HS_indoor18.xlsx"
# read excel
hs_indoor18 <- hs_indoor18 %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel, path = hs_indoor18)
########## tiding hs indoor ##########
# tidy & convert the time format
hs_indoor18 <- map(hs_indoor18, tidy_hs)
hs_indoor18 <- map(hs_indoor18, count_sec)
# fix hs indoor 1600m Rank
hs_indoor18$boys_1600m$Rank <- as.numeric(hs_indoor18$boys_1600m$Rank)

######## loading hs outdoor ##########
# outdoor
hs_outdoor18 <- "./data/HS_outdoor18.xlsx"
# read excel
hs_outdoor18 <- hs_outdoor18 %>%
    excel_sheets() %>%
    purrr::set_names() %>%
    map(read_excel, path = hs_outdoor18)
########## tiding hs indoor ##########
# tidy & convert the time format
hs_outdoor18 <- map(hs_outdoor18, tidy_hs)
hs_outdoor18 <- map(hs_outdoor18, count_sec)

############# function 3 #############
# function3 - subset the data, get outstanding players 
sub_data <- function(x) {
    x %>%
        # group by state
        group_by(state) %>%
        # aggregate the amount of athletes
        count() %>%
        left_join(x, by = "state") %>%
        group_by(state) %>%
        # get the greatest rank from each state
        filter(Rank == min(Rank))
}
###### subset hs indoor/outdoor ######
sub_otf <- map(hs_outdoor18, sub_data)
sub_itf <- map(hs_indoor18, sub_data)

############ loading usmap ############
library(usmap) 
library(maps)
# library(ggmap)
# library(mapdata)
# usmap
usmap <- map_data("state") %>%
    select(1, 2, 3, 5)





ui <- fluidPage(
    
    # Main title
    titlePanel("The High School Track & Field Performance Results in 2018"),
    helpText("A statewide distribution of top 500 athletes"),
    
    # Arrange slider, textInput, selectInput x & y, actionButton, and plot in a sidebarLayout
    sidebarLayout(
        
        sidebarPanel(
            radioButtons(inputId = "type", 
                         label = "Select a type",
                         choices = c(Indoor = "indoor",
                                     Outdoor = "outdoor")
            ),
            radioButtons(inputId = "gender",
                         label = "Select a gender",
                         choices = c(Men = "boys",
                                     Women = "girls")
            ),
            radioButtons(inputId = "event",
                         label = "Select an event",
                         choices = c(`800m` = "800m",
                                     `1600m` = "1600m",
                                     `3200m` = "3200m")
            ),
            sliderInput(inputId = "rank", 
                        label = "Range of ranking", 
                        value = 500, min = 0, max = 500, step = 500), 
            # an extra spacing
            br(),
            # actionButtom
            actionButton(inputId = "go", label = "Create a plot")
        ),
        
        # plot & table output (using tabs)
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Table", tableOutput("table"))
            )
        )
    )
)

# ----------------------------------------------------------------------------------

server <- function(input, output){
    
    data <- reactive({
        filename <- str_c(input$gender, "_", input$event)
        if(input$type == "indoor") {
            sub_itf[[filename]]
        } else {
            sub_otf[[filename]]
        }
    })
    
    myplot <- eventReactive(input$go, {
        ggplot(data(), aes(x = lon, y = lat, color = Rank)) +
            geom_polygon(data = usmap, aes(x = long, y = lat, group = group),
                         color = "white", fill = "grey92") +
            # label states
            ggrepel::geom_label_repel(aes(label = state), data = data(),
                                      size = 3, label.size = 0, segment.color = "orange") +
            # point size & ranking
            geom_point(aes(size = n, color = Rank, alpha = 0.8)) +
            scale_color_continuous("Ranking", low = "#0099FF", high = "red") +
            scale_size_continuous("Count", range = c(1, 12))
        
    })
    
    mytable <- eventReactive(input$go, { data() })
    
    output$plot <- renderPlot({
        myplot()
    })
    
    output$table <- renderTable({
        mytable()
    })
    
}

# ----------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
