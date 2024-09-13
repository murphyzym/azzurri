# for questions about this app, please contact
# Murphy Zhang: yz4732@columbia.edu

server = function(input, output, session) {
 
  # >>>>>>>>>>>>>>>>>>>>>----
  # reactives----
  wc = reactiveValues(
    tb = 0, # selected tab (0 to 6)
    ds = 0, # selected desc section (1 to 4)
    gm = 0, # selected game (1 to 6)
    gr = 8, # selected group (1 to 12; Italy = 8)
    hi = 0, # selected hist ui (1 or 2)
    mr = 0, # selected merch category (1 to 3)
    pl = 0, # selected player (1 to 11)
    yr = 0, # selected wc yr (1 to 18)
    yi = NULL, # all wc years italy
    yw = NULL, # all wc years world
    tc = 0, # selected travel city (1 to 3)
    to = 0, # selected travel option (1 to 3)
    pr = 0, # selected product (1 to 14)
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # events page buttons----
  # _home button----
  observeEvent(
    input$homeBtn, 
    {
      updateRadioGroupButtons(
        inputId = 'tabs',
        selected = character(0)
      )
      wc$tb = 0
    }
  )
  # _tab buttons----
  observeEvent(
    input$tabs, 
    if (!is.null(input$tabs)) {
      wc$tb = as.numeric(input$tabs)
    }
  )
  # _about button----
  observeEvent(
    input$aboutBtn, 
    {
      updateRadioGroupButtons(
        inputId = 'tabs',
        selected = character(0)
      )
      wc$tb = 5
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui body----
  output$uiBody = renderUI(
    if (wc$tb == 0) {
      uiOutput('uiHome')
    } else if (wc$tb <= 4) {
      uiOutput(paste0('uiPage', wc$tb))
    } else {
      uiOutput('uiAbout')
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui home----
  output$uiHome = renderUI(
    {
      dd = ymd('20260611') - Sys.Date()
      div(
        style = 'padding:50px 10% 50px 10%;',
        fluidRow(
          # _description----
          column(
            width = 8,
            align = 'left',
            div(
              style = paste0(
                'overflow-y:auto; ',
                'height:480px; ',
                'background-color:rgba(255,255,255,0.8); ',
                'padding:30px 30px 20px 30px; ',
                'border-radius:10px;'
              ),
              h3(
                style = 'color:blue;',
                'Welcome to the Italy World Cup 2026 Fan Hub!'
              ),
              h4(
                style = 'color:green;',
                paste0(
                  'This Shiny app is your ultimate companion for following Italy\'s ',
                  'journey in the upcoming FIFA World Cup 2026. Whether you’re a ',
                  'die-hard fan, a history buff, or a traveler planning to support ',
                  'Italy on the ground, this app has something for you.'
                )
              ),
              h4(
                style = 'color:green;',
                'The app is organized into several sections to simplify your experience:'
              ),
              div(
                style = 'padding:0 5% 0 5%;',
                fluidRow(
                  lapply(
                    1:4,
                    function(i) {
                      column(
                        width = 3,
                        align = 'center',
                        div(
                          style = paste0(
                            'padding:10px 0 10px 0; ',
                            'background-color:rgba(0,0,255,0.7); ',
                            'border-radius:10px;'
                          ),
                          actionBttn(
                            inputId = paste0('desc', i),
                            label = i,
                            style = 'stretch',
                            color = ifelse(wc$ds == i, 'warning', 'default'),
                            size = 'lg',
                            icon = icon(
                              ifelse(wc$ds == i, 'caret-down', 'caret-right')
                            )
                          ),
                          h4(
                            style = 'color:white;',
                            c(
                              'Schedule',
                              'Azzurri',
                              'History',
                              'Travel'
                            )[i]
                          )
                        )
                      )
                    }
                  )
                ),
                uiOutput('uiSec')
              ),
              h4(
                style = 'color:red;',
                paste0(
                  'Stay tuned as we update the app with the official World Cup ',
                  'schedule and more information to ensure you have the most ',
                  'accurate and up-to-date resources at your fingertips. ',
                  'Forza Italia!'
                )
              )
            )
          ),
          # _countdown----
          column(
            width = 4,
            align = 'center',
            div(
              style = paste0(
                'height:480px; ',
                'background-color:rgba(255,255,255,0.8); ',
                'padding:50px 30px 0 30px; ',
                'border-radius:10px;'
              ),
              h1(
                style = 'margin:0;',
                'FIFA',
                img(
                  src = 'worldcup_ball.png',
                  width = '50px'
                )
              ),
              h2(
                style = 'margin:15px 0 25px 0;',
                'WORLD CUP 2026'
              ),
              h3(
                style = 'margin:15px 0 15px 0;',
                'Begins In'
              ),
              # hr(),
              div(
                style = paste0(
                  'background-color:green; ',
                  'border-radius:10px; ',
                  'padding:10px 10px 20px 10px;'
                ),
                h1(
                  style = 'color:white; font-size:100px;',
                  dd
                ),
                h1('Days')
              )
            )
          )
        )
      )
    }
  )
 
  # _event desc section----
  observeEvent(input$desc1, {wc$ds = 1 * (wc$ds != 1)})
  observeEvent(input$desc2, {wc$ds = 2 * (wc$ds != 2)})
  observeEvent(input$desc3, {wc$ds = 3 * (wc$ds != 3)})
  observeEvent(input$desc4, {wc$ds = 4 * (wc$ds != 4)})
  
  # _ui section desc----
  output$uiSec = renderUI(
    if (wc$ds > 0) {
      div(
        hr(style = 'border-color:green;'),
        h4(
          style = 'color:black;',
          switch(
            wc$ds,
            paste0(
              'This section provides a detailed look at Italy\'s ',
              'matches and the overall World Cup game schedule. Although ',
              'the current data is simulated to reflect potential scenarios, ',
              'it includes match dates, venues, and key information to help ',
              'you plan your viewing. We will update this section with real ',
              'data once the official World Cup schedule is released.'
            ),
            paste0(
              'Get to know the Italian squad inside out. This section ',
              'features real-time information about Italy’s national team, ',
              'allowing you to keep up with the players representing Italy ',
              'on the world stage.'
            ),
            paste0(
              'Dive deep into Italy’s rich World Cup history. ',
              'This section offers a comprehensive overview of Italy\'s ',
              'past performances in every World Cup, complete with dates, ',
              'match stages, involved teams, and results. Additionally, ',
              'explore the broader history of the World Cup itself, ',
              'including details on host countries, winning teams, ',
              'goals scored, qualified teams, and total attendance.'
            ),
            paste0(
              'Planning to support Italy live? This section is ',
              'designed for you. Based on the match schedule, it offers ',
              'curated options for hotels, restaurants, and attractions ',
              'in the cities where Italy might play. These travel guidelines ',
              'are tailored for fans who want to experience the World Cup ',
              'in person.'
            )
          )
        ),
        hr(style = 'border-color:red;')
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui page 1 schedule----
  output$uiPage1 = renderUI(
    {
      z = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM games ',
          'JOIN venues USING (venue_id) ',
          'JOIN locations USING (locn_id) ',
          'WHERE group_id = \'', LETTERS[wc$gr], '\';'
        )
      )
      div(
        style = 'padding:10px 5% 30px 5%;',
        # _group buttons----
        fluidRow(
          lapply(
            1:12, 
            function(i) {
              column(
                width = 1,
                style = 'padding:0 0 10px 0;',
                actionBttn(
                  inputId = paste0('group', LETTERS[i]),
                  label = paste0('GROUP ', LETTERS[i]),
                  style = 'simple',
                  color = ifelse(i != wc$gr, 'default', 'primary'),
                  size = 'sm',
                  block = TRUE
                )
              )
            }
          )
        ),
        div(
          align = 'center',
          h3(
            style = 'color:white;', 
            'The Group Stage Games'
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 3,
            style = 'padding:20px 0 25px 0;',
            # _game buttons a----
            lapply(
              1:3,
              function(i) {
                j = 2 * i - 1
                tags$button(
                  id = paste0('game', j),
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
                  div(
                    style = 'margin:20px 0 20px 0;',
                    img(
                      src = 'arrow.png',
                      width = '15%'
                    ),
                    img(
                      src = ifelse(
                        wc$gr == 8,
                        paste0('ball_', z$team_a[j], '.png'),
                        paste0('flags/circles/', z$team_a[j], '.png')
                      ),
                      width = '20%'
                    ),
                    img(
                      src = 'vs_white.png',
                      width = '10%',
                      style = 'margin:0 20px 0 20px;'
                    ),
                    img(
                      src = ifelse(
                        wc$gr == 8,
                        paste0('ball_', z$team_b[j], '.png'),
                        paste0('flags/circles/', z$team_b[j], '.png')
                      ),
                      width = '20%'
                    )
                  )
                )
              }
            )
          ),
          column(
            width = 3,
            style = 'padding:20px 0 25px 0;',
            # _game buttons b----
            lapply(
              1:3,
              function(i) {
                j = 2 * i
                tags$button(
                  id = paste0('game', j),
                  class = 'btn action-button',
                  style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
                  div(
                    style = 'margin:20px 0 20px 0;',
                    img(
                      src = 'arrow.png',
                      width = '15%'
                    ),
                    img(
                      src = ifelse(
                        wc$gr == 8,
                        paste0('ball_', z$team_a[j], '.png'),
                        paste0('flags/circles/', z$team_a[j], '.png')
                      ),
                      width = '20%'
                    ),
                    img(
                      src = 'vs_white.png',
                      width = '10%',
                      style = 'margin:0 20px 0 20px;'
                    ),
                    img(
                      src = ifelse(
                        wc$gr == 8,
                        paste0('ball_', z$team_b[j], '.png'),
                        paste0('flags/circles/', z$team_b[j], '.png')
                      ),
                      width = '20%'
                    )
                  )
                )
              }
            )
          ),
          column(
            width = 6,
            leafletOutput('schedMap', height = '50vh')
          )
        ),
        uiOutput('uiGameDetail')
      )
    }
  )
        
  # _events game buttons----
  observeEvent(input$game1, {wc$gm = 1; gameDet()})
  observeEvent(input$game2, {wc$gm = 2; gameDet()})
  observeEvent(input$game3, {wc$gm = 3; gameDet()})
  observeEvent(input$game4, {wc$gm = 4; gameDet()})
  observeEvent(input$game5, {wc$gm = 5; gameDet()})
  observeEvent(input$game6, {wc$gm = 6; gameDet()})
  
  # _sched map----
  output$schedMap = renderLeaflet(
    {
      z = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM games ',
          'JOIN venues USING (venue_id) ',
          'JOIN locations USING (locn_id) ',
          'WHERE group_id = \'', LETTERS[wc$gr], '\';'
        )
      )
      z |> 
        leaflet() |> 
        addTiles() |> 
        addAwesomeMarkers(
          lat = ~lat,
          lng = ~lng,
          popup = ~venue_name,
          icon = awesomeIcons(
            markerColor = c('red', 'green', 'blue', 'orange', 'pink', 'purple'),
            icon = 'star'
          )
        )
    }
  )
  
  # event group buttons----
  observeEvent(input$groupA, {wc$gr = 1; showGrp()})
  observeEvent(input$groupB, {wc$gr = 2; showGrp()})
  observeEvent(input$groupC, {wc$gr = 3; showGrp()})
  observeEvent(input$groupD, {wc$gr = 4; showGrp()})
  observeEvent(input$groupE, {wc$gr = 5; showGrp()})
  observeEvent(input$groupF, {wc$gr = 6; showGrp()})
  observeEvent(input$groupG, {wc$gr = 7; showGrp()})
  observeEvent(input$groupH, {wc$gr = 8; showGrp()})
  observeEvent(input$groupI, {wc$gr = 9; showGrp()})
  observeEvent(input$groupJ, {wc$gr = 10; showGrp()})
  observeEvent(input$groupK, {wc$gr = 11; showGrp()})
  observeEvent(input$groupL, {wc$gr = 12; showGrp()})
 
  # function show grp----
  showGrp = function() {
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM games ',
        'JOIN venues USING (venue_id) ',
        'JOIN locations USING (locn_id) ',
        'WHERE group_id = \'', LETTERS[wc$gr], '\';'
      )
    )
  }
  
  # function game detail----
  gameDet = function() {
    # the data
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM games g ',
        'JOIN venues USING (venue_id) ',
        'JOIN locations USING (locn_id) ',
        'WHERE group_id = \'', LETTERS[wc$gr], '\';'
      )
    )
    ca = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM games g ',
        'JOIN countries c ',
        'ON g.team_a = c.ctry_id ',
        'WHERE group_id = \'', LETTERS[wc$gr], '\';'
      )
    )
    cb = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM games g ',
        'JOIN countries c ',
        'ON g.team_b = c.ctry_id ',
        'WHERE group_id = \'', LETTERS[wc$gr], '\';'
      )
    )
    # shiny alert popup
    shinyalert(
      title = paste0('GROUP ', LETTERS[wc$gr], ' - Game ', wc$gm),
      text = tagList(
        fluidRow(
          column(
            width = 6,
            div(
              style = 'margin:20px 0 20px 0;',
              h2(z$game_dt[wc$gm]),
              img(
                src = paste0('flags/rectangles/', z$team_a[wc$gm], '.jpg'),
                width = '20%',
                style = 'border:solid black 1px;'
              ),
              img(
                src = 'vs_black.png',
                width = '10%',
                style = 'margin:0 20px 0 20px;'
              ),
              img(
                src = paste0('flags/rectangles/', z$team_b[wc$gm], '.jpg'),
                width = '20%',
                style = 'border:solid black 1px;'
              )
            ),
            div(
              h3(
                paste0(
                  ca$ctry_name[wc$gm], ' vs ',
                  cb$ctry_name[wc$gm]
                )
              )
            ),
            hr(),
            tags$button(
              id = 'regBtn',
              class = 'btn action-button',
              style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
              img(
                src = 'register.png',
                width = '50%'
              )
            )
          ),
          column(
            width = 6,
            leafletOutput('gameMap')
          )
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'l',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
    # game map
    output$gameMap = renderLeaflet(
      {
        z |> 
          leaflet() |> 
          addTiles() |> 
          addAwesomeMarkers(
            lat = z$lat[wc$gm],
            lng = z$lng[wc$gm],
            popup = z$venue_name[wc$gm],
            icon = awesomeIcons(
              markerColor = c('red', 'green', 'blue')[wc$gm],
              text = wc$gm
            )
          ) |> 
          addPopups(
            lat = z$lat[wc$gm] + 0.0007,
            lng = z$lng[wc$gm],
            popup = z$venue_name[wc$gm]
          )
      }
    )
    
  }
  
  # _event register button----
  observeEvent(
    input$regBtn,
    {
      z = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM games g ',
          'JOIN venues USING (venue_id) ',
          'JOIN locations USING (locn_id) ',
          'WHERE group_id = \'', LETTERS[wc$gr], '\';'
        )
      )
      ca = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM games g ',
          'JOIN countries c ',
          'ON g.team_a = c.ctry_id ',
          'WHERE group_id = \'', LETTERS[wc$gr], '\';'
        )
      )
      cb = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM games g ',
          'JOIN countries c ',
          'ON g.team_b = c.ctry_id ',
          'WHERE group_id = \'', LETTERS[wc$gr], '\';'
        )
      )
      shinyalert(
        title = paste0('Register for Tickets: Game ', wc$gm),
        text = tagList(
          h3(
            ca$ctry_name[wc$gm],
            'vs',
            cb$ctry_name[wc$gm]
          ),
          hr(),
          fluidRow(
            column(
              width = 7,
              lapply(
                1:5,
                function(i) {
                  fluidRow(
                    column(
                      width = 5,
                      align = 'right',
                      h4(
                        style = 'padding-top:10px;',
                        c(
                          'Numer of Tickets:',
                          'First Name:',
                          'Last Name:',
                          'Email:',
                          'Phone Number:'
                        )[i]
                      )
                    ),
                    column(
                      width = 7,
                      textInput(
                        inputId = paste0('tixQ', i),
                        label = NULL,
                        width = '100%'
                      )
                    )
                  )
                }
              )
            ),
            column(
              width = 5,
              leafletOutput('tixMap')
            )
          ),
          actionBttn(
            inputId = 'confTix',
            label = 'Confirm Registration',
            style = 'gradient',
            color = c('danger', 'success', 'primary')[wc$gm],
            size = 'md',
            block = FALSE
          )
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
      output$tixMap = renderLeaflet(
        {
          z |> 
            leaflet() |> 
            addTiles() |> 
            addAwesomeMarkers(
              lat = z$lat[wc$gm],
              lng = z$lng[wc$gm],
              popup = z$venue_name[wc$gm],
              icon = awesomeIcons(
                markerColor = c('red', 'green', 'blue')[wc$gm],
                text = wc$gm
              )
            ) |> 
            addPopups(
              lat = z$lat[wc$gm] + 0.0007,
              lng = z$lng[wc$gm],
              popup = z$venue_name[wc$gm]
            )
        }
      )
    }
  )
  
  # _event confirm button----
  observeEvent(
    input$confTix,
    {
      shinyalert(
        title = paste0('Congratulations ', input$tixQ2, ' ', input$tixQ3, '!'),
        text = tagList(
          h3(
            'You have successfully registered to receive',
            'updates about Official Hospitality seating.'
          )
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 's',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui page 2 azzurri----
  output$uiPage2 = renderUI(
    {
      z = dbGetQuery(
        con,
        paste0(
          'SELECT * FROM players ',
          'WHERE plyr_id IN (1,2,9,13,14,16,17,18,21,22,23) ',
          'ORDER BY pos;'
        )
      )
      div(
        style = 'padding:0 5% 0 5%;',
        fluidRow(
          column(
            width = 4,
            style = 'padding-top:80px;',
            div(
              style = paste0(
                'padding:20px; ',
                'background-color:#01a242; ',
                'border-radius:10px 10px 0 0; '
              ),
              align = 'center',
              h2(style = 'color:white;', 'Key Team Stats')
            ),
            div(
              style = paste0(
                'padding:15px; ',
                'background-color:white; ',
                'border-radius:0; '
              ),
              align = 'center',
              h1(style = 'color:blue;', 1.95),
              h4(style = 'color:blue;', 'xG per match')
            ),
            div(
              style = paste0(
                'padding:15px; ',
                'background-color:#ed0021; ',
                'border-radius:0 0 10px 10px; '
              ),
              align = 'center',
              h1(style = 'color:white;', '90.23%'),
              h4(style = 'color:white;', 'Passing Accuracy')
            )
          ),
          column(
            width = 8,
            div(
              style = 'padding:10px 0 40px 0;',
              div(
                align = 'center',
                h4(
                  style = 'color:white;',
                  'The Starting 11'
                ),
                hr(),
                # _forwards----
                div(
                  style = 'padding:0 10% 0 10%;',
                  div(
                    lapply(
                      5:7,
                      function(i) {
                        tags$button(
                          id = paste0('plyr', i),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:0; margin:0 20px 0 20px; border-radius:50px;',
                          img(
                            src = paste0('plyrs/', z$plyr_id[i], '.png'),
                            width = '100px',
                            style = 'border-radius:50px;'
                          )
                        )
                      }
                    )
                  ),
                  hr(style = 'margin:10px 0 10px 0;'),
                  # _midfielders----
                  div(
                    lapply(
                      9:11,
                      function(i) {
                        tags$button(
                          id = paste0('plyr', i),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:0; margin:0 20px 0 20px; border-radius:50px;',
                          img(
                            src = paste0('plyrs/', z$plyr_id[i], '.png'),
                            width = '100px',
                            style = 'border-radius:50px;'
                          )
                        )
                      }
                    )
                  ),
                  hr(style = 'margin:10px 0 10px 0;'),
                  # _defenders----
                  div(
                    lapply(
                      1:4,
                      function(i) {
                        tags$button(
                          id = paste0('plyr', i),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:0; margin:0 20px 0 20px; border-radius:50px;',
                          img(
                            src = paste0('plyrs/', z$plyr_id[i], '.png'),
                            width = '100px',
                            style = 'border-radius:50px;'
                          )
                        )
                      }
                    )
                  ),
                  hr(style = 'margin:10px 0 10px 0;'),
                  # _goalkeepers----
                  div(
                    lapply(
                      8:8,
                      function(i) {
                        tags$button(
                          id = paste0('plyr', i),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:0; margin:0 20px 0 20px; border-radius:50px;',
                          img(
                            src = paste0('plyrs/', z$plyr_id[i], '.png'),
                            width = '100px',
                            style = 'border-radius:50px;'
                          )
                        )
                      }
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  )
  
  # _events plyr buttons----
  observeEvent(input$plyr1, {wc$pl = 1; plProf()})
  observeEvent(input$plyr2, {wc$pl = 2; plProf()})
  observeEvent(input$plyr3, {wc$pl = 3; plProf()})
  observeEvent(input$plyr4, {wc$pl = 4; plProf()})
  observeEvent(input$plyr5, {wc$pl = 5; plProf()})
  observeEvent(input$plyr6, {wc$pl = 6; plProf()})
  observeEvent(input$plyr7, {wc$pl = 7; plProf()})
  observeEvent(input$plyr8, {wc$pl = 8; plProf()})
  observeEvent(input$plyr9, {wc$pl = 9; plProf()})
  observeEvent(input$plyr10, {wc$pl = 10; plProf()})
  observeEvent(input$plyr11, {wc$pl = 11; plProf()})
  
  # function player profile----
  plProf = function() {
    # the data
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM players ',
        'WHERE plyr_id IN (1,2,9,13,14,16,17,18,21,22,23) ',
        'ORDER BY pos;'
      )
    )
    # shiny alert popup
    shinyalert(
      title = NULL,
      text = tagList(
        fluidRow(
          column(
            width = 5,
            img(
              src = paste0('plyrs/', z$plyr_id[wc$pl], '.png'),
              width = '100%',
              style = 'margin-top:20px;'
            )
          ),
          column(
            width = 7,
            h3(z$plyr_name[wc$pl]),
            hr(),
            h4('Position: ', z$pos[wc$pl]),
            h4('Height: ', z$ht[wc$pl]),
            h4('Weight: ', z$wt[wc$pl]),
            h4('Age: ', z$age[wc$pl])
          )
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 's',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
  }
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui page 3 history----
  output$uiPage3 = renderUI(
    {
      div(
        align = 'center',
        tags$button(
          id = 'wcItalyBtn',
          class = 'btn action-button',
          style = paste0(
            'background-color:rgba(0,0,0,0); ',
            'padding:0; margin:0 10px 0 10px; ',
            'border-radius:50px;'
          ),
          img(
            src = 'button_italy.png',
            width = '150px'
          )
        ),
        tags$button(
          id = 'wcWorldBtn',
          class = 'btn action-button',
          style = paste0(
            'background-color:rgba(0,0,0,0); ',
            'padding:0; margin:0 10px 0 10px; ',
            'border-radius:50px;'
          ),
          img(
            src = 'button_world.png',
            width = '150px'
          )
        ),
        uiOutput('uiHist')
      )
    }
  )
  
  # _events hist buttons----
  observeEvent(input$wcItalyBtn, {wc$hi = 1})
  observeEvent(input$wcWorldBtn, {wc$hi = 2})
  
  # _ui hist----
  output$uiHist = renderUI(
    # _italy history----
    if (wc$hi == 1) {
      z = dbGetQuery(
        con,
        paste0(
          'SELECT DISTINCT date_part(\'year\', date) yr ',
          'FROM italy_past ORDER BY 1;'
        )
      )
      wc$yi = pull(z, yr)
      div(
        style = 'padding:20px 5% 10px 5%;',
        align = 'center',
        lapply(
          1:4, 
          function(j) {
            fluidRow(
              column(width = 1),
              lapply(
                1:(3 + 2 * (j < 4)),
                function(i) {
                  k = (j - 1) * 5 + i
                  column(
                    width = 2,
                    align = 'center',
                    style = paste0(
                      'height:80px; ',
                      'background-color:white; ',
                      'padding:15px; margin:5px; ',
                      'border-radius:10px;'
                    ),
                    tags$button(
                      id = paste0('wcYr', k),
                      class = 'btn action-button',
                      style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
                      div(
                        align = 'center',
                        h2(
                          style = paste0(
                            'padding:10px 0 10px 0; ',
                            'margin:0; ',
                            'color:',
                            ifelse(z$yr[k] %in% champ, 'green', 'black'), ';'
                          ),
                          if (z$yr[k] %in% champ) {
                            img(
                              src = 'trophy2.png',
                              height = '40px'
                            )
                          },
                          z$yr[k]
                        )
                      )
                    )
                  )
                }
              )
            )
          }
        )
      )
    } 
    # _world history----
    else if (wc$hi == 2) {
      w = dbGetQuery(
        con,
        paste0(
          'SELECT * ',
          'FROM worldcup ',
          'ORDER BY 1;'
        )
      )
      c = dbGetQuery(
        con,
        paste0(
          'SELECT * ',
          'FROM countries;'
        )
      )
      host1 = left_join(w, c, by = c('ctry_a' = 'ctry_id'))
      host2 = left_join(w, c, by = c('ctry_b' = 'ctry_id'))
      host3 = left_join(w, c, by = c('ctry_c' = 'ctry_id'))
      place1 = left_join(w, c, by = c('winner' = 'ctry_id'))
      place2 = left_join(w, c, by = c('second' = 'ctry_id'))
      place3 = left_join(w, c, by = c('third' = 'ctry_id'))
      div(
        style = 'padding:20px 5% 10px 5%; ',
        div(
          # __header----
          fluidRow(
            style = 'margin-bottom:10px;',
            # year----
            column(
              width = 1,
              h4(style = 'color:yellow; padding-top:20px;', 'Year')
            ),
            # host----
            column(
              width = 2,
              h4(style = 'color:yellow; padding-top:20px;', 'Host(s)')
            ),
            # 1st place----
            column(
              width = 1,
              img(
                src = 'place1.png',
                width = '60px'
              )
            ),
            # 2nd place----
            column(
              width = 1,
              img(
                src = 'place2.png',
                width = '60px'
              )
            ),
            # 3rd place----
            column(
              width = 1,
              img(
                src = 'place3.png',
                width = '60px'
              )
            ),
            # goals----
            column(
              width = 2,
              h4(style = 'color:yellow; padding-top:20px;', 'Goals Scored')
            ),
            # teams----
            column(
              width = 2,
              h4(style = 'color:yellow; padding-top:20px;', 'Qualified Teams')
            ),
            # atten----
            column(
              width = 2,
              h4(style = 'color:yellow; padding-top:20px;', 'Total Attendance')
            )
          ),
          hr()
        ),
        # __body----
        div(
          style = paste0(
            'height:60vh; overflow-y:auto; ',
            'margin-top:20px;'
          ),
          lapply(
            1:23,
            function(i) {
              div(
                style = paste0(
                  'background-color:rgba(0,0,0,0.5); ',
                  'border-radius:10px; ',
                  'padding:0 0 10px 0; margin-bottom:10px;'
                ),
                fluidRow(
                  style = 'margin-bottom:10px;',
                  # year----
                  column(
                    width = 1,
                    tags$a(
                      href = w$url[i],
                      target = '_blank',
                      h5(
                        style = paste0(
                          'color:lightblue; padding-top:20px;', 
                          'text-decoration:underline;'
                        ),
                        w$year[i]
                      )
                    )
                  ),
                  # host----
                  column(
                    width = 2,
                    h5(
                      style = paste0(
                        'color:lime; padding-top:',
                        ifelse(is.na(host2$ctry_name[i]), '20px;',
                        ifelse(is.na(host3$ctry_name[i]), '10px;', '7px;'))
                      ), 
                      host1$ctry_name[i]
                    ),
                    if (!is.na(host2$ctry_name[i])) {
                      h5(style = 'color:lime;', host2$ctry_name[i])
                    },
                    if (!is.na(host3$ctry_name[i])) {
                      h5(style = 'color:lime;', host3$ctry_name[i])
                    }
                  ),
                  # 1st place----
                  column(
                    width = 1,
                    h5(
                      style = 'color:white;', 
                      ifelse(
                        !is.na(place1$ctry_name[i]),
                        place1$ctry_name[i],
                        '?'
                      )
                    ),
                    img(
                      src = paste0(
                        'flags/rectangles/', 
                        ifelse(
                          !is.na(place1$winner[i]),
                          place1$winner[i], 
                          '00'
                        ),
                        '.jpg'
                      ),
                      width = '80%',
                      style = 'border:solid black 1px; border-radius:10px;'
                    )
                  ),
                  # 2nd place----
                  column(
                    width = 1,
                    h5(
                      style = 'color:white;', 
                      ifelse(
                        !is.na(place2$ctry_name[i]),
                        place2$ctry_name[i],
                        '?'
                      )
                    ),
                    img(
                      src = paste0(
                        'flags/rectangles/', 
                        ifelse(
                          !is.na(place2$second[i]),
                          place2$second[i], 
                          '00'
                        ),
                        '.jpg'
                      ),
                      width = '80%',
                      style = 'border:solid black 1px; border-radius:10px;'
                    )
                  ),
                  # 3rd place----
                  column(
                    width = 1,
                    h5(
                      style = 'color:white;', 
                      ifelse(
                        !is.na(place3$ctry_name[i]),
                        place3$ctry_name[i],
                        '?'
                      )
                    ),
                    img(
                      src = paste0(
                        'flags/rectangles/', 
                        ifelse(
                          !is.na(place3$third[i]),
                          place3$third[i], 
                          '00'
                        ),
                        '.jpg'
                      ),
                      width = '80%',
                      style = 'border:solid black 1px; border-radius:10px;'
                    )
                  ),
                  # goals----
                  column(
                    width = 2,
                    h3(
                      style = 'color:magenta; padding-top:15px;', 
                      ifelse(
                        !is.na(w$goals[i]),
                        round(w$goals[i] / w$mat_play[i], 1),
                        '?'
                      )
                    )
                  ),
                  # teams----
                  column(
                    width = 2,
                    h3(
                      style = 'color:magenta; padding-top:15px;', 
                      ifelse(
                        !is.na(w$qual_team[i]),
                        w$qual_team[i],
                        '?'
                      )
                    )
                  ),
                  # atten----
                  column(
                    width = 2,
                    h3(
                      style = 'color:magenta; padding-top:15px;', 
                      ifelse(
                        !is.na(w$atten[i]),
                        comma(as.numeric(w$atten[i]) / w$mat_play[i], accuracy = 1),
                        '?'
                      )
                    )
                  )
                )
              )
            }
          )
        )
      )
    }
  )
  
  # _events wc yr buttons----
  observeEvent(input$wcYr1, {wc$yr = 1; italyWCDet()})
  observeEvent(input$wcYr2, {wc$yr = 2; italyWCDet()})
  observeEvent(input$wcYr3, {wc$yr = 3; italyWCDet()})
  observeEvent(input$wcYr4, {wc$yr = 4; italyWCDet()})
  observeEvent(input$wcYr5, {wc$yr = 5; italyWCDet()})
  observeEvent(input$wcYr6, {wc$yr = 6; italyWCDet()})
  observeEvent(input$wcYr7, {wc$yr = 7; italyWCDet()})
  observeEvent(input$wcYr8, {wc$yr = 8; italyWCDet()})
  observeEvent(input$wcYr9, {wc$yr = 9; italyWCDet()})
  observeEvent(input$wcYr10, {wc$yr = 10; italyWCDet()})
  observeEvent(input$wcYr11, {wc$yr = 11; italyWCDet()})
  observeEvent(input$wcYr12, {wc$yr = 12; italyWCDet()})
  observeEvent(input$wcYr13, {wc$yr = 13; italyWCDet()})
  observeEvent(input$wcYr14, {wc$yr = 14; italyWCDet()})
  observeEvent(input$wcYr15, {wc$yr = 15; italyWCDet()})
  observeEvent(input$wcYr16, {wc$yr = 16; italyWCDet()})
  observeEvent(input$wcYr17, {wc$yr = 17; italyWCDet()})
  observeEvent(input$wcYr18, {wc$yr = 18; italyWCDet()})
  
  # function italy wc detail----
  italyWCDet = function() {
    # the data
    z = dbGetQuery(
      con,
      paste0(
        'SELECT *, date_part(\'month\', date) mo, date_part(\'day\', date) dy ',
        'FROM italy_past ',
        'WHERE date_part(\'year\', date) = ', wc$yi[wc$yr], ' ',
        'ORDER BY date;'
      )
    )
    # shiny alert popup
    shinyalert(
      title = paste('World Cup', wc$yi[wc$yr]), 
      text = tagList(
        hr(style = 'margin:2px 0 8px 0;'),
        lapply(
          1:nrow(z),
          function(i) {
            div(
              fluidRow(
                style = 'margin-bottom:10px;',
                # date----
                column(
                  width = 2,
                  h3(paste(mth[z$mo[i]], z$dy[i]))
                ),
                # stage----
                column(
                  width = 4,
                  h3(z$stage[i])
                ),
                # home flag----
                column(
                  width = 2,
                  align = 'right',
                  img(
                    src = paste0('flags/rectangles/', z$home_team[i], '.jpg'),
                    width = '80%',
                    style = 'border:solid black 1px; border-radius:10px;'
                  )
                ),
                # score----
                column(
                  width = 2,
                  h3(paste(z$home_team_goals[i], ':', z$away_team_goals[i]))
                ),
                # away flag----
                column(
                  width = 2,
                  align = 'left',
                  img(
                    src = paste0('flags/rectangles/', z$away_team[i], '.jpg'),
                    width = '80%',
                    style = 'border:solid black 1px; border-radius:10px;'
                  )
                )
              ),
              hr(style = 'margin:2px 0 8px 0;')
            )
          }
        ),
        if (wc$yi[wc$yr] %in% champ) {
          h4(
            'World Cup',
            img(
              src = 'trophy2.png',
              height = '80px'
            ),
            'Champs!'
          )
          
        }
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
    
  }
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # ui page 4 travel----
  output$uiPage4 = renderUI(
    {
      div(
        style = 'padding:10px 5% 0 5%;',
        div(
          align = 'center',
          h4(
            style = 'color:white;',
            'Travel Guide for Team Italy Fans'
          ),
          # _city buttons----
          div(
            style = 'padding:10px 80px 10px 80px;',
            radioGroupButtons(
              inputId = 'cityOpts',
              label = NULL,
              choices = c('Miami' = 1, 'Atlanta' = 2, 'Houston' = 3),
              selected = character(0),
              size = 'lg',
              status = 'warning',
              justified = TRUE
            )
          ),
          # _travel buttons----
          div(
            style = 'padding:50px 10% 50px 10%;',
            fluidRow(
              lapply(
                1:3,
                function(i) {
                  column(
                    width = 4,
                    tags$button(
                      id = paste0('trav', i),
                      class = 'btn action-button',
                      style = 'background-color:rgba(0,0,0,0); padding:0; margin:0;',
                      img(
                        src = paste0('trav', i, '.png'),
                        width = '300px'
                      )
                    ),
                    h4(
                      style = 'color:white;',
                      c('STAY', 'DINE', 'EXPLORE')[i]
                    )
                  )
                }
              )
            )
            
          )
        )
      )
    }
  )
  
  # _events travel buttons----
  observeEvent(input$trav1, {wc$to = 1; hotelDet()})
  observeEvent(input$trav2, {wc$to = 2; restaDet()})
  observeEvent(input$trav3, {wc$to = 3; attraDet()})
  
  # * * * * ----
  # function hotel detail----
  hotelDet = function() {
    # the data----
    wc$tc = ifelse(!is.null(input$cityOpts), as.numeric(input$cityOpts), 1)
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM hotels ',
        'JOIN venues USING (venue_id) ',
        'LIMIT 10 OFFSET ', 10 * (wc$tc - 1), ';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = paste('HOTELS -', city[wc$tc]), 
      text = tagList(
        lapply(
          1:10,
          function(i) {
            div(
              style = paste0(
                'padding:10px; margin:5px 0 0 0; ',
                'background-color:rgba(50,50,50,0.1); ',
                'border-radius:10px;'
              ),
              fluidRow(
                column(
                  width = 8,
                  align = 'left',
                  fluidRow(
                    # hotel name----
                    column(
                      width = 9,
                      style = 'margin:0; padding:0 0 0 15px;',
                      tags$button(
                        id = paste0('hotel', i),
                        class = 'btn action-button',
                        style = paste0(
                          'background-color:rgba(0,0,0,0); ',
                          'padding:0; margin:0;'
                        ),
                        div(
                          h4(
                            style = 'font-weight:bold; text-decoration:underline;',
                            txtShort(z$hotel_name[i], 30)
                          )
                        )
                      )
                    ),
                    # hotel stars----
                    column(
                      width = 3,
                      align = 'right',
                      style = 'padding:20px 5px 0 0;',
                      lapply(
                        1:z$hotel_stars[i],
                        function(j) {
                          img(src = 'star.png', width = '20px')
                        }
                      )
                    )
                  ),
                  # distance to stadium----
                  h4(
                    img(
                      src = paste0('pin', wc$tc, '.png'),
                      width = '15px',
                      style = 'margin-right:3px;'
                    ),
                    tags$span(paste0(z$dist_to_venue[i], ' from')),
                    tags$span(
                      style = paste0('color:', c('red','green','blue')[wc$tc]), 
                      z$venue_name[i]
                    )
                  ),
                  fluidRow(
                    # expedia rating----
                    column(
                      width = 8,
                      fluidRow(
                        column(
                          width = 3,
                          style = 'margin:0; padding:0 0 0 15px;',
                          div(
                            align = 'center',
                            style = paste0(
                              'padding:3px; margin:0 0 0 0; ',
                              'background-color:purple; ',
                              'border-radius:10px;'
                            ),
                            h4(
                              style = 'color:yellow;',
                              z$expedia_rating[i]
                            )
                          )
                        ),
                        column(
                          width = 9,
                          align = 'left',
                          h4(
                            style = 'color:purple; padding:5px 0 0 0;',
                            'Expedia Rating'
                          )
                        )
                      )
                    ),
                    # hotel price----
                    column(
                      width = 4,
                      align = 'right',
                      h3(
                        style = 'padding:0; margin:0;',
                        paste0('$', z$hotel_price[i])
                      )
                    )
                  )
                ),
                # hotel image----
                column(
                  width = 4,
                  img(
                    src = paste0('travel/', z$hotel_id[i], '.jpeg'),
                    width = '100%'
                  )
                )
              )
            )
          }
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
  }
  
  # events hotel buttons----
  observeEvent(input$hotel1, {hotelResv(1)})
  observeEvent(input$hotel2, {hotelResv(2)})
  observeEvent(input$hotel3, {hotelResv(3)})
  observeEvent(input$hotel4, {hotelResv(4)})
  observeEvent(input$hotel5, {hotelResv(5)})
  observeEvent(input$hotel6, {hotelResv(6)})
  observeEvent(input$hotel7, {hotelResv(7)})
  observeEvent(input$hotel8, {hotelResv(8)})
  observeEvent(input$hotel9, {hotelResv(9)})
  observeEvent(input$hotel10, {hotelResv(10)})
  
  # function hotel reserve----
  hotelResv = function(i) {
    j = 5 * (wc$tc - 1) + i
    # the data----
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM hotels ',
        'JOIN locations USING (locn_id) ',
        'JOIN venues USING (venue_id) ',
        'WHERE hotel_id = \'h', str_pad(j, 3, 'left', '0'), '\';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = z$hotel_name, 
      text = tagList(
        fluidRow(
          column(
            width = 8,
            leafletOutput('hotelMap'),
            h4(z$address)
          ),
          column(
            width = 4,
            h4('Reserve a Room'),
            hr(),
            dateInput(
              inputId = 'chkinDt',
              label = 'Check-In Date',
              value = Sys.Date()
            ),
            selectInput(
              inputId = 'nights',
              label = '# of Nights',
              choices = 1:14,
              selected = 2
            ),
            selectInput(
              inputId = 'guests',
              label = 'Guests',
              choices = 1:6,
              selected = 2
            ),
            hr(),
            actionBttn(
              inputId = 'confHotel',
              label = 'Confirm Reservation',
              style = 'gradient',
              color = c('danger', 'success', 'primary')[wc$tc],
              size = 'md',
              block = FALSE
            )
          )
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
    # hotel map----
    output$hotelMap = renderLeaflet(
      {
        leaflet(data = z) |> 
          addTiles() |> 
          addAwesomeMarkers(
            lat = ~lat,
            lng = ~lng,
            popup = ~hotel_name,
            icon = awesomeIcons(
              markerColor = c('red', 'green', 'blue')[wc$tc],
              icon = 'star'
            )
          )
      }
    )
  }
  
  # confirm hotel button----
  observeEvent(
    input$confHotel,
    {
      shinyalert(
        title = 'Thank You!', 
        text = tagList(
          h4(
            paste0(
              'We will see you in ',
              city[wc$tc],
              ' on ',
              input$chkinDt, '. ',
              'Safe travels!'
            )
          )
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  # * * * * ----
  # function resta detail----
  restaDet = function() {
    # the data----
    wc$tc = ifelse(!is.null(input$cityOpts), as.numeric(input$cityOpts), 1)
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM restaurants ',
        'JOIN venues USING (venue_id) ',
        'LIMIT 10 OFFSET ', 10 * (wc$tc - 1), ';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = paste('RESTAURANTS -', city[wc$tc]), 
      text = tagList(
        lapply(
          1:10,
          function(i) {
            div(
              style = paste0(
                'padding:10px; margin:5px 0 0 0; ',
                'background-color:rgba(50,50,50,0.1); ',
                'border-radius:10px;'
              ),
              fluidRow(
                column(
                  width = 8,
                  align = 'left',
                  fluidRow(
                    # resta name----
                    column(
                      width = 9,
                      style = 'margin:0; padding:0 0 0 15px;',
                      tags$button(
                        id = paste0('resta', i),
                        class = 'btn action-button',
                        style = paste0(
                          'background-color:rgba(0,0,0,0); ',
                          'padding:0; margin:0;'
                        ),
                        div(
                          h4(
                            style = 'font-weight:bold; text-decoration:underline;',
                            txtShort(z$rest_name[i], 30)
                          )
                        )
                      )
                    ),
                    # resta price----
                    column(
                      width = 3,
                      align = 'right',
                      style = 'padding:20px 5px 0 0;',
                      h4(
                        style = paste0(
                          'color:',
                          c('red','green','blue')[wc$tc]
                        ),
                        strrep('$', z$rest_price[i])
                      )
                    )
                  ),
                  fluidRow(
                    # resta rating----
                    column(
                      width = 6,
                      fluidRow(
                        column(
                          width = 3,
                          style = 'margin:0; padding:0 0 0 15px;',
                          div(
                            align = 'center',
                            style = paste0(
                              'padding:3px; margin:0 0 0 0; ',
                              'background-color:purple; ',
                              'border-radius:10px;'
                            ),
                            h4(
                              style = 'color:yellow;',
                              z$rest_rating[i]
                            )
                          )
                        ),
                        column(
                          width = 9,
                          align = 'left',
                          h4(
                            style = 'color:purple; padding:5px 0 0 0;',
                            'User Rating'
                          )
                        )
                      )
                    ),
                    # resta cuisine----
                    column(
                      width = 6,
                      align = 'right',
                      h4(
                        style = 'padding:0; margin:0;',
                        z$rest_cuisine[i]
                      )
                    )
                  )
                ),
                # resta image----
                column(
                  width = 4,
                  img(
                    src = paste0('travel/', z$rest_id[i], '.jpeg'),
                    width = '100%'
                  )
                )
              )
            )
          }
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
  }
  
  # events resta buttons----
  observeEvent(input$resta1, {restaResv(1)})
  observeEvent(input$resta2, {restaResv(2)})
  observeEvent(input$resta3, {restaResv(3)})
  observeEvent(input$resta4, {restaResv(4)})
  observeEvent(input$resta5, {restaResv(5)})
  observeEvent(input$resta6, {restaResv(6)})
  observeEvent(input$resta7, {restaResv(7)})
  observeEvent(input$resta8, {restaResv(8)})
  observeEvent(input$resta9, {restaResv(9)})
  observeEvent(input$resta10, {restaResv(10)})
  
  # function resta reserve----
  restaResv = function(i) {
    j = 5 * (wc$tc - 1) + i
    # the data----
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM restaurants ',
        'JOIN locations USING (locn_id) ',
        'JOIN venues USING (venue_id) ',
        'WHERE rest_id = \'r', str_pad(j, 3, 'left', '0'), '\';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = z$rest_name, 
      text = tagList(
        fluidRow(
          column(
            width = 8,
            leafletOutput('restaMap'),
            h4(z$address)
          ),
          column(
            width = 4,
            h4('Reserve a Table'),
            hr(),
            dateInput(
              inputId = 'resDt',
              label = 'Reservation Date',
              value = Sys.Date()
            ),
            selectInput(
              inputId = 'resTime',
              label = 'Reservation Time',
              choices = paste0(12:23, ':00'),
              selected = '12:00'
            ),
            selectInput(
              inputId = 'party',
              label = 'Party Size',
              choices = 1:8,
              selected = 2
            ),
            hr(),
            actionBttn(
              inputId = 'confResta',
              label = 'Confirm Reservation',
              style = 'gradient',
              color = c('danger', 'success', 'primary')[wc$tc],
              size = 'md',
              block = FALSE
            )
          )
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
    # resta map----
    output$restaMap = renderLeaflet(
      {
        leaflet(data = z) |> 
          addTiles() |> 
          addAwesomeMarkers(
            lat = ~lat,
            lng = ~lng,
            popup = ~rest_name,
            icon = awesomeIcons(
              markerColor = c('red', 'green', 'blue')[wc$tc],
              icon = 'star'
            )
          )
      }
    )
  }
  
  # confirm resta button----
  observeEvent(
    input$confResta,
    {
      shinyalert(
        title = 'Thank You!', 
        text = tagList(
          h4(
            paste0(
              'We will see you in ',
              city[wc$tc],
              ' on ',
              input$resDt, '. ',
              'Safe travels!'
            )
          )
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )
  
  # * * * * ----
  # function attra detail----
  attraDet = function() {
    # the data----
    wc$tc = ifelse(!is.null(input$cityOpts), as.numeric(input$cityOpts), 1)
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM attractions ',
        'JOIN venues USING (venue_id) ',
        'LIMIT 10 OFFSET ', 10 * (wc$tc - 1), ';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = paste('ATTRACTIONS -', city[wc$tc]), 
      text = tagList(
        lapply(
          1:10,
          function(i) {
            div(
              style = paste0(
                'padding:10px; margin:5px 0 0 0; ',
                'background-color:rgba(50,50,50,0.1); ',
                'border-radius:10px;'
              ),
              fluidRow(
                column(
                  width = 8,
                  align = 'left',
                  fluidRow(
                    # attra name----
                    column(
                      width = 9,
                      style = 'margin:0; padding:0 0 0 15px;',
                      tags$button(
                        id = paste0('attra', i),
                        class = 'btn action-button',
                        style = paste0(
                          'background-color:rgba(0,0,0,0); ',
                          'padding:0; margin:0;'
                        ),
                        div(
                          h4(
                            style = 'font-weight:bold; text-decoration:underline;',
                            txtShort(z$attr_name[i], 30)
                          )
                        )
                      )
                    ),
                    # attra price----
                    column(
                      width = 3,
                      align = 'right',
                      style = 'padding:20px 5px 0 0;',
                      h4(
                        style = paste0(
                          'color:',
                          c('red','green','blue')[wc$tc]
                        ),
                        ifelse(
                          z$attr_price[i] > 0,
                          strrep('$', z$attr_price[i]),
                          'FREE'
                        )
                      )
                    )
                  ),
                  fluidRow(
                    # attra duration----
                    column(
                      width = 4,
                      div(
                        align = 'center',
                        style = paste0(
                          'padding:3px; margin:0 0 0 0; ',
                          'background-color:purple; ',
                          'border-radius:10px;'
                        ),
                        h4(
                          style = 'color:yellow;',
                          z$duration[i]
                        )
                      )
                    ),
                    # attra type----
                    column(
                      width = 8,
                      align = 'right',
                      h4(
                        style = 'padding:0; margin:0;',
                        z$attr_type[i]
                      )
                    )
                  )
                ),
                # attra image----
                column(
                  width = 4,
                  img(
                    src = paste0('travel/', z$attr_id[i], '.jpeg'),
                    width = '100%'
                  )
                )
              )
            )
          }
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
  }
  
  # events attra buttons----
  observeEvent(input$attra1, {attraResv(1)})
  observeEvent(input$attra2, {attraResv(2)})
  observeEvent(input$attra3, {attraResv(3)})
  observeEvent(input$attra4, {attraResv(4)})
  observeEvent(input$attra5, {attraResv(5)})
  observeEvent(input$attra6, {attraResv(6)})
  observeEvent(input$attra7, {attraResv(7)})
  observeEvent(input$attra8, {attraResv(8)})
  observeEvent(input$attra9, {attraResv(9)})
  observeEvent(input$attra10, {attraResv(10)})
  
  # function attra reserve----
  attraResv = function(i) {
    j = 5 * (wc$tc - 1) + i
    # the data----
    z = dbGetQuery(
      con,
      paste0(
        'SELECT * FROM attractions ',
        'JOIN locations USING (locn_id) ',
        'JOIN venues USING (venue_id) ',
        'WHERE attr_id = \'a', str_pad(j, 3, 'left', '0'), '\';'
      )
    )
    # shiny alert popup----
    shinyalert(
      title = z$attr_name, 
      text = tagList(
        fluidRow(
          column(
            width = 8,
            leafletOutput('attraMap'),
            h4(z$address)
          ),
          column(
            width = 4,
            h4('Visit'),
            hr(),
            dateInput(
              inputId = 'visitDt',
              label = 'Visit Date',
              value = Sys.Date()
            ),
            selectInput(
              inputId = 'visitTime',
              label = 'Visit Time',
              choices = paste0(9:23, ':00'),
              selected = '12:00'
            ),
            selectInput(
              inputId = 'party2',
              label = 'Party Size',
              choices = 1:8,
              selected = 2
            ),
            hr(),
            actionBttn(
              inputId = 'confAttra',
              label = 'Confirm Visit',
              style = 'gradient',
              color = c('danger', 'success', 'primary')[wc$tc],
              size = 'md',
              block = FALSE
            )
          )
        )
      ),
      showConfirmButton = FALSE,
      html = TRUE,
      size = 'm',
      closeOnClickOutside = TRUE,
      animation = 'pop',
      timer = FALSE
    )
    # attra map----
    output$attraMap = renderLeaflet(
      {
        leaflet(data = z) |> 
          addTiles() |> 
          addAwesomeMarkers(
            lat = ~lat,
            lng = ~lng,
            popup = ~attr_name,
            icon = awesomeIcons(
              markerColor = c('red', 'green', 'blue')[wc$tc],
              icon = 'star'
            )
          )
      }
    )
  }
  
  # confirm attra button----
  observeEvent(
    input$confAttra,
    {
      shinyalert(
        title = 'Thank You!', 
        text = tagList(
          h4(
            paste0(
              'We will see you in ',
              city[wc$tc],
              ' on ',
              input$visitDt, '. ',
              'Safe travels!'
            )
          )
        ),
        showConfirmButton = FALSE,
        html = TRUE,
        size = 'm',
        closeOnClickOutside = TRUE,
        animation = 'pop',
        timer = FALSE
      )
    }
  )

  # >>>>>>>>>>>>>>>>>>>>>----
  # ui about----
  output$uiAbout = renderUI(
    {
      div(
        style = 'padding:10px 5% 0 5%;',
        fluidRow(
          column(
            width = 6,
            style = 'padding:50px 5% 0 5%;',
            h3(style = 'color:lightblue;', 'Primary Team'),
            # _primary team----
            fluidRow(
              style = paste0(
                'padding:20px 10px 20px 10px; ',
                'background-color:rgba(0,0,0,0.5); ',
                # 'border:solid white 1px; ',
                'border-radius:10px;'
              ),
              lapply(
                1:3,
                function(i) {
                  column(
                    width = 4,
                    align = 'center',
                    img(
                      src = paste0(
                        'teammate/', 
                        team$first[i], ' ',
                        team$last[i],
                        '.jpg'
                      ),
                      width = '90%',
                      style = 'border-radius:10px;'
                    ),
                    h4(
                      style = 'color:white;',
                      paste0(
                        team$first[i], ' ', team$last[i]
                      )
                    ),
                    tags$a(
                      href = linkedin[i],
                      target = '_blank',
                      img(
                        src = 'linkedin.png',
                        width = '25%'
                      )
                    )
                  )
                }
              )
            ),
            h3(style = 'color:lightblue;', 'Secondary Team'),
            # _secondary team----
            h5(
              style = 'color:white;',
              paste0(
                team$first[4], ' ', team$last[4], ', ',
                team$first[5], ' ', team$last[5], ', ',
                team$first[6], ' ', team$last[6], ', ',
                team$first[7], ' ', team$last[7]
              )
            )
          ),
          # _testimonials----
          column(
            width = 6,
            align = 'left',
            style = 'padding:100px 5% 0 5%;',
            h3(
              style = 'color:white;',
              paste0(
                '\"This is a fantastic app, one of the best I have seen! ',
                'This is a very useful one that can be utilized and ',
                'referenced for the years leading up to the World Cup. ',
                'Congratulations on the excellent work team!\"'
              )
            ),
            div(
              align = 'right',
              h5(
                style = 'color:white;',
                paste0(
                  '- Day Yi, Lecturer, Columbia University'
                )
              )
            )
          )
        )
      )
    }
  )
  
}
  