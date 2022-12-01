library(shiny)
library(readxl)
library(stringr)
library(glue)

pore_clogging <- read_excel("/Users/emilysharp/Desktop/pore_clogging.xlsx")

find_bad_ingredients <- function(ingredients){
  ingredients_split <- unlist(str_split(ingredients, ",")) %>%
    str_trim()
  
  ingredients_cleaned <- str_remove_all(ingredients_split,"[+#&]|-") %>%
    str_remove_all(" ") %>%
    str_to_lower()
  
  ingredients_df <- data.frame(ingredientName = ingredients_split, cleanedName = ingredients_cleaned)
  
  bad_ingredients_identified <- character()
  bad_count <- 0
  
  for(i in 1:nrow(ingredients_df)){
    for(j in 1:nrow(pore_clogging)){
      if(adist(ingredients_df$cleanedName[i], pore_clogging$cleanedName[j]) <= pore_clogging$maxDist[j]){
        bad_count <- bad_count+1
        bad_ingredients_identified <- c(bad_ingredients_identified, ingredients_df$ingredientName[i])
      }
    }
  }
  
  if(length(bad_ingredients_identified) > 0){
    return(data.frame(1:length(bad_ingredients_identified), bad_ingredients_identified))
  } else{
    return(data.frame(c("None")))
  }
}

# Define UI ----
ui <- fluidPage(
  titlePanel("Pore Clogging Ingredients in Skincare"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId = "ingInput", 
        label = NULL, 
        value = "", 
        placeholder = "Paste the product's ingredients here (separated by commas)", 
        width = '100%',
        height = '450px'
      ),
      actionButton(inputId = "goButton", label = "Check Ingredients"),
      width = 5
    ),
    mainPanel(
      textOutput("commentOutput"),
      tableOutput("ingOutput"),
      width = 7
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  output$commentOutput <- renderText({"Pore clogging ingredients found: "})
  
  bad_ingredients <- eventReactive(input$goButton, {
    find_bad_ingredients(input$ingInput)
  })
  
  # Not necessary, but I didn't figure out how to display a message that changes 
  # depending on whether bad ingredients were found
  
  output$ingOutput <- renderTable({
    bad_ingredients()
  }, colnames = FALSE)
  
  session$onSessionEnded(stopApp)
}

# Run the app ----
app <- shinyApp(ui = ui, server = server, options = list(port = 6003, launch.browser = T))
runApp(app)
