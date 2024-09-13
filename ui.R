# for questions about this app, please contact
# Murphy Zhang: yz4732@columbia.edu

ui = fluidPage(
  
  title = 'Azzuri', # what appears on browser tab
  theme = shinytheme('lumen'), # one of 16 themes from https://rstudio.github.io/shinythemes/
  setBackgroundImage(src = 'background.jpg'), # background image
  
  # header----
  div(
    style = 'font-family: "Times New Roman", Times, serif;',
    fluidRow(
      column(
        width = 3,
        fluidRow(
          column(
            width = 4,
            tags$button(
              id = 'homeBtn',
              class = 'btn action-button',
              style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
              img(
                src = 'Italy_logo_small.png',
                width = '100%'
              )
            )
          ),
          lapply(
            1:4,
            function(i) {
              column(
                width = 2,
                align = 'center',
                style = 'padding:20px 0 0 0;',
                img(
                  src = 'trophy3.png',
                  width = '50%'
                ),
                h6(style = 'color:white;', champ[i])
              )
            }
          )
        )
      ),
      column(
        width = 7,
        h3(style = 'color:gold;', 'FIFA WORLD CUP 2026'),
        h1(style = 'color:white; margin:0; font-size:50px; font-style:italic;', 'Italy')
      ),
      column(
        width = 2,
        align = 'right',
        style = 'padding:20px 5px 0 0;',
        actionBttn(
          inputId = 'aboutBtn',
          label = 'About Us',
          style = 'minimal',
          color = 'default',
          size = 'sm'
        )
      )
    )
  ),
  
  # tabs----
  div(
    radioGroupButtons(
      inputId = 'tabs',
      label = NULL,
      choices = c(
        'Home' = 0,
        'Schedule' = 1,
        'Azzurri' = 2,
        'History' = 3,
        'Travel' = 4
      ),
      selected = character(0),
      justified = TRUE,
      size = 'normal'
    )
  ),
  
  # body----
  div(
    style = 'height:75vh; overflow-y:scroll; overflow-x:hidden;',
    uiOutput('uiBody')
  )
  
)
