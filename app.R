#importing libraries
library(shiny)
library(tidyverse)
library(bslib)
library(maps)

#reading the datasets
data <- read_csv("bwf_historic_rankings.csv") |>
  janitor::clean_names()

world <- map_data("world")

#subsetting the dataset to get Asia
asia <- subset(world, region %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
                                    "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
                                    "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
                                    "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", "Myanmar", 
                                    "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", 
                                    "Russia", "Saudi Arabia", "Singapore", "South Korea", "Sri Lanka", "Syria", 
                                    "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", "Turkmenistan", 
                                    "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"))
asia$region <- toupper(asia$region)

# Define UI for application
ui <- page_navbar(
  # app title
  title = "Analyzing BWF Badminton World Rankings",
  
  inverse = TRUE,
  #first page (with the map)
  nav_panel(title = "Map & Timeline", 
            layout_column_wrap(
              width = 1/2,
              card( full_screen = FALSE,
                    # adding the map and slider input UI
                    card_header("Number of Top 100 Players Around in Asia in the Past 10 Years"),
                    card_body(
                      plotOutput("map"),
                      sliderInput(
                        inputId = "date_input",
                        label = "Date:",
                        max = as.Date("2023-10-23","%Y-%m-%d"),
                        min = as.Date("2013-01-07","%Y-%m-%d"),
                        value=as.Date("2013-01-07"),
                        step = 7,
                        width = "95%",
                        timeFormat="%Y-%m-%d")),
                    card_footer(
                      "*Missing data from April 2020-February 2021"
                    )),
              card(card_header("Summary of Map Analysis"),
                   # text to explain key takeways from the visualization
                   card_body(
                     card_image("www/bwfbadminton.svg", height = "10px"),
                     "Some key takeways from this map is that in the past decade, a majority of the top 100 players in 
                     Asia come from East or Southeast Asia. In particular, Indonesia is a country which consistently has 
                     a lot of (over 40) top 100 players across the five disciplines. Similar, nations including China, 
                    Japan, Korea, Malaysia, and Thailand all also seem to have at least 30 top 100 players consistently. A
                       nation whose number of top 100 players seem to have the most change the past 10 years is India. The
                       number of players have increased over the 10 years, suggesting that their team is improving quite a lot."
                   ),
                   # link to BWF website
                   card_footer(markdown("For more information about international badminton, you can visit the [BWF Website](https://bwfbadminton.com/)")))
            )
  ),
  nav_panel(title = "Player Comparison",
            titlePanel("2022-23 Rankings of Top Athletes"),
            layout_sidebar(
              # selectInput UI in sidebar
              sidebar = sidebar(
                selectInput(
                  "discipline",
                  label = "Discipline",
                  choices = c("MEN'S SINGLES" = "MS",
                              "WOMEN'S SINGLES" = "WS",
                              "MEN'S DOUBLES" = "MD",
                              "WOMEN'S DOUBLES" = "WD",
                              "MIXED DOUBLES" = "XD"),
                  selected = "MS"),
                selectInput(
                  inputId = "player1",
                  label = "First Player",
                  choices = NULL),
                selectInput(
                  inputId = "player2",
                  label = "Second Player",
                  choices = NULL),
                markdown("The lineplot to the right shows the ranking of the two players selected from 2022-2023. To view players from a different discipline, you can select from the first dropdown.")),
              card(
                # putting in the lineplot UI
                full_screen = TRUE,
                card_body(
                  plotOutput("lineplot")
                )))),
  nav_panel(title = "Country Stats", 
            titlePanel("Proportions of Top 50 Players of Each Discipline in Dominating Countries by Year"),
            layout_sidebar(
              # selectInput UI in sidebar
              sidebar = sidebar(
                selectizeInput(
                  "country",
                  label = "Country",
                  choices = c("CHINA",
                              "INDIA",
                              "INDONESIA",
                              "MALAYSIA",
                              "JAPAN",
                              "THAILAND",
                              "KOREA",
                              "DENMARD",
                              "CHINESE TAIPEI")),
                markdown("The plot to the left shows the proportion of top 50 players from the selected country who play each discipline (2018-2023). The dropdown above allows you to select the country whose data you'd like to explore.")),
                card(
                  full_screen = TRUE,
                  # outputting the barplot
                  card_body(
                    plotOutput("barplot")
                  )
                )
            )),
  nav_panel(title = "Additional Info",
            titlePanel("Additional Information"),
            markdown("**Created by**: Nalin (Fai) Poungpeth"),
            # linking the data source
            markdown("**Data Source**: [Badminton World Ranking (historic Top 100) Dataset](https://www.kaggle.com/datasets/valentindefour/badminton-world-rankings-updated-august-2020)")),
            # markdown("**What is the core concept(s) or insight(s) into the data that you believe the visualization communicates? Explain your choice of widgets and what about the data it help users understand**"),
            # markdown("The core concept that the visualization communicates is the change in badminton ranking statistics over time. The app starts off on a more general note in the first page, as it shows which countries in Asia the top 100 are from over the past ten years. 
            #           Thus, it shows which countries have been performing well in general, as well as how these trends have changed as badminton becomes more globalized in recent years. In the second page, the lineplot shows changes in rankings of specific players over the past two years, allowing the users to see players who have recently risen in the ranks, as well as which players have maintained good performance. 
            #           This could be helpful since the Paris Olympics are coming up, meaning that these lineplots could provide insight into which players would be fun to keep an eye out for. In addition, players who are curious about how two different players perform in comparison to each other can easily compare their data by choosing the names they're curious about. Finally, the third page contains a collection of countries selected based on the darker densities of the map, as well as the nationalities of the top ten players in the second page, to determine which countries are dominating the badminton scene.
            #           From then, it shows the proportion of top 50 players from that country in each of the five disciplines, which allows users to analyze whether that country can be considered 'well-rounded' or if they only have one or two disciplines in which they're strong at. For instance, China's data suggests that they have an even proportion of top players in all disciplines, whereas Malaysia has more players in Men's doubles than Women's singles.
            #           Thus, this could be used if coaches or staff wanted to figure out which discipline to work on, as it shows which one they're weaker at in comparison to the other disciplines"),
            # 
            # markdown("The core concepts I used to create the datasets included numerous components of `ggplot`, such as using `geom_polygon` to create a map to show the geographical disparity between badminton quality in different regions of Asia, and lineplots to show changes in ranking of specific players over time. 
            #          In addition, I used a stacked barplot to show which discipline specific countries are better at at each year, and standardized that by plotting the proportions rather than absolute values. 
            #          In order to make these visualizations even more useful, I combined these static components with dynamic/reactive features such as the `selectInput` and `sliderInput` so that users can also do their own exploration with the data.
            #          With both the shiny-specific components and that of `ggplot`, I also made sure to adjust the aesthetics so that it is easier to view. For instance, I decided to create a multi-page app so that each plot is big enough for the user to view with ease, and included a sidePanel for the dropdown in some pages to help with organization and make the page more intuitive to navigate.
            #          In regards to the specific types of widgets I chose to integrate, I decided to use the dropdowns for player, country, and discipline selections since there are quite a lot of options to choose from. If I were to use something else (eg: radio buttons), it would take up a lot more space and become confusing to use. Although the discipline dropdown specifically only has five options, I believe that keeping it consistent with the other two widgets on the page makes more intuitive to use.
            #          As for the `sliderInput` in the first page, it is meant to be representitive of a timeline, which is something that is likely to be familiar to users and thus easy to use. I believe that this comes together very well with the static components of the visualization, as it allows for users to make specific comparisons or analyses based on their personal preference in a very intuitive manner.\n")),

  tags$head(
    tags$style(HTML("
    # adjusting the selectInput UI aesthetics
        .selectize-input {
        height: auto;
        line-height: 1;
        display: inline-block;
        white-space: wrap;
        vertical-align: middle;
      }
      .selectize-dropdown .option {
        padding: 5px;
        line-height: 1.2;
        font-size: 9pt;
      }
      .selectize-control.single .selectize-input {
        align-items: center;
        height: 42px;
        font-size: 9pt;
        vertical-align: middle;
      }
      .card-header { 
        color:black;
        background: lightgrey;
        margin-top: 0px;
        padding-top: 10px;
        padding-bottom: 0px;
        height: 50px;}
       .card {
         padding: 0;
         width: 97.5%;
         margin-left: 1.25%;
      }"))
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(
    input$discipline,
    {
      #preparing the initial input data
      topten <- data |>
        filter(rank <= 10 & date == as.Date("2023-10-23","%Y-%m-%d") & draw == input$discipline) |>
        pull(name)
      
      two_to_nine <- data |>
        filter(rank <= 9 & rank > 1 & date == as.Date("2023-10-23","%Y-%m-%d") & draw == input$discipline) |>
        pull(name)
      
      # mapping the filtered dataset into the appropriate selectInput UI
      updateSelectInput(session, input = "player1", choices = topten)
      updateSelectInput(session, input = "player2", choices = two_to_nine)
      
    }
  )
  
  observe({
    # preparing initial player options for lineplot
    topten <- data |>
      filter(rank <= 10 & date == as.Date("2023-10-23","%Y-%m-%d") & draw == input$discipline) |>
      pull(name)
    # setting initial player options for the lineplot dropdown
    if(!is.null(input$player2))
      updateSelectInput(session, "player2",
                        choices = topten[!(topten %in% input$player1)],
                        selected = isolate(input$player2))
  })
  
  output$map <- renderPlot({
    # preparing map data
    map <- data |>
      filter(rank <= 100, date == input$date_input)
    
    map$country[map$country == "CHINESE-TAIPEI"] <- "CHINESE TAIPEI"
    map$country[map$country == "HONG-KONG"] <- "HONG KONG"
    
    country_density <- map |>
      count(country)
    
    plot_data <- asia |> 
      left_join(country_density, by = c("region" = "country")) |> 
      mutate(
        n = if_else(is.na(n), 0, n)
      ) 
    
    # building the plot
    ggplot() +
      geom_polygon(data = plot_data, aes(group = group, x = long, y = lat, fill = n), 
                   color = "black", size = 0.3) +
      scale_fill_gradient(
        low = "white",
        high = "maroon"
      ) +
      theme_void()
  })
  
  output$lineplot <- renderPlot({
    # preparing the lineplot data
    player_data <- data |>
      filter((name == input$player1 | name == input$player2)  & date >= as.Date("2022-01-03"))
    
    # building the lineplot
    ggplot() +
      geom_line(data = player_data, aes(x = date, y = rank, color = name)) +
      scale_y_continuous(limits = c(1, 10, 1)) +
      scale_x_date(date_breaks = "2 month", date_labels = "%Y, %m") +
      scale_y_reverse() +
      labs(
        x = "Date since January 3rd, 2022",
        y = "World Ranking",
        title = str_wrap(paste(input$player1, "&", input$player2, "WORLD RANKING 2022-23"),
                         width = 70)
      ) +
      scale_color_discrete(
        name = NULL, labels = c(str_wrap(input$player2, width = 20), str_wrap(input$player1, width = 20)),
        guide = guide_legend(reverse=TRUE)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)
      )
  })
  
  output$barplot <- renderPlot({
    # preparing the barplot data
    dominant_countries <- data |> 
      filter(
        (date == as.Date("2018-01-01","%Y-%m-%d") |
        date == as.Date("2019-01-07","%Y-%m-%d") |
        date == as.Date("2020-01-06","%Y-%m-%d") |
        date == as.Date("2021-02-01","%Y-%m-%d") |
        date == as.Date("2022-01-03","%Y-%m-%d") |
        date == as.Date("2023-01-02","%Y-%m-%d")) & 
        rank <= 50 &
        (country == "CHINA" | country == "INDIA" | country == "INDONESIA" | 
           country == "MALAYSIA" | country == "JAPAN" | country == "THAILAND" |
           country == "KOREA" | country == "DENMARK" | country == "CHINESE TAIPEI"))  

    c <- dominant_countries |>
      group_by(country, date) |> 
      count(draw)
    
    total <- dominant_countries |>
      group_by(date) |> 
      count(country)
    
    b <- dominant_countries |> 
      left_join(c, by = c("country", "date", "draw")) |> 
      mutate(
        n = if_else(is.na(n), 0, n)
      )
    
    colnames(b)[8] <- "d"
    
    barplot_data <- b |> 
      left_join(total, by = c("country", "date")) |> 
      mutate(
        n = if_else(is.na(n), 0, n)
      )
    
    final_data <- barplot_data |> 
      filter(country == input$country)
    
    final_data$draw <- factor(final_data$draw, levels=c('MS', 'WS', 'MD', "WD", "XD"))
    
    # building the plot
    ggplot(final_data) +
      geom_col(aes(x = date, y = n, fill = draw), position = "fill") +
      labs(
        x = "Date (First Month of Each Year)",
        y = "Proportion of Top 50 Ranked Players in Each Discipline",
        title = input$country
      ) +
      scale_fill_manual(name = "Discipline",
                          labels = c("Men's Singles", 
                                     "Women's Singles", 
                                     "Men's Doubles", 
                                     "Women's Doubles",
                                     "Mixed Doubles"),
                          breaks = c("MS", "WS", "MD", "WD", "XD"),
                          values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
                          ) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        axis.text = element_text(size = 10)
      )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
