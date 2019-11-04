server <- shinyServer(function(input, output, session){
  
# TEAM COMPARISON
  
  ##teaam comparison
  selected_team_a <- reactive({
    return(input$team_a)})
  
  selected_team_b <- reactive({
    return(input$team_b)})
  
  output$hc_compare <- renderHighchart({
    
    team_filter <- rating %>% select(Stats, Team = selected_team_b())
    color_filter <- color.code %>% select(Team = selected_team_b())
    
    hchart(rating, type = "area", hcaes(x = Stats, y = Milwaukee), color = color.code$Milwaukee, showInLegend = FALSE) %>%
      hc_add_series(team_filter, "area", hcaes(x = Stats, y = Team), color = color_filter$Team, showInLegend = FALSE) %>% #masih rusak
      hc_chart(polar = TRUE) %>%
      hc_pane(size = '100%') %>% 
      hc_xAxis(tickmarkPlacement = 0,
               lineWidth = 0 ,
               title = list(enabled = FALSE)) %>%
      hc_yAxis(gridLineInterpolation = "polygon",
               title = list(enabled = FALSE)) %>%
      hc_plotOptions(area = list(threshold = -2,
                                 marker = list(enabled = FALSE)
                                 )

      )
  })
  
  ##logo
  output$logo_b <- renderUI({
    tags$img(src = paste0(selected_team_b(), ".png"), 
             width = "140px", 
             height = "140px", 
             style = "display: block; margin-left: auto; margin-right: auto;")
  })
  
  ##boxscore
  output$box_a <- renderText({
    kableExtra::kable(team.score %>% filter(Team == selected_team_a()) %>% dplyr::select(-Team)) %>%
      kable_styling(bootstrap_options = c("stiped", "hover"), 
                    stripe_color = "black",
                    full_width = TRUE) %>%
      column_spec(1:2, 
                  background = "rgba(225, 225, 225, 0.3)", 
                  color = "black") %>%
      row_spec(0, 
               background = "rgba(225, 225, 225, 0.3)", 
               color = "black")
  })
  
  output$box_b <- renderText({
    kableExtra::kable(team.score %>% filter(Team == selected_team_b()) %>% dplyr::select(-Team)) %>%
      kable_styling(bootstrap_options = c("stiped", "hover"),
                    stripe_color = "black",
                    full_width = TRUE,
                    position = "right")%>%
      column_spec(1:2, 
                  background = "rgba(225, 225, 225, 0.3)", 
                  color = "black") %>%
      row_spec(0, 
               background = "rgba(225, 225, 225, 0.3)", 
               color = "black")
  })

  
    
# PLAYER FINDER  
  
  ##player clustering
  output$cluster <- renderHighchart({ 
    
    hchart(pole, type = "column", name = "cluster 1", hcaes(x = Stats, y = c1), showInLegend = TRUE) %>%
      hc_add_series(pole, "column", name = "cluster 2", hcaes(x = Stats, y = c2), showInLegend = TRUE) %>% 
      hc_add_series(pole, "column", name = "cluster 3", hcaes(x = Stats, y = c3), showInLegend = TRUE) %>%
      hc_add_series(pole, "column", name = "cluster 4", hcaes(x = Stats, y = c4), showInLegend = TRUE) %>%
      hc_add_series(pole, "column", name = "cluster 5", hcaes(x = Stats, y = c5), showInLegend = TRUE) %>%
      hc_add_series(pole, "column", name = "cluster 6", hcaes(x = Stats, y = c6), showInLegend = TRUE) %>%
      hc_title(text = "Player Clustering",
               style = list(fontWeight = "bold")) %>% 
      hc_xAxis(tickmarkPlacement = 0,
               lineWidth = 0,
               gridLineColor = "#000000",
               labels = list(style = list(fontWeight = "bold"))) %>%
      hc_yAxis(gridLineInterpolation = "polygon",
               gridLineColor = "#000000",
               min = -50) %>% 
      hc_plotOptions(column = list(stacking = "percent",
                                   marker = list(enabled = FALSE)),
                     series = list(pointWidth = 60))
  })
  
  ##datatable output
  output$tableout <- DT::renderDataTable({
    
    if(input$cluster.filter == "All"){
      players <- cluster.pole
    }
    else{
      players <- cluster.pole %>% filter(cluster %in% input$cluster.filter)
    }
    
    
    
    DT::datatable(players, 
                  options = list(dom = 'tip', scrollX = TRUE, 
                                 scrollY = TRUE, pageLength = 5, 
                                 lengthChange = FALSE)
    )
  })
  

# SIMULATOR
  
  #text output
  output$player.list <- renderUI({
    
    selected.row.index <- input$tableout_rows_selected
    selected.row.index <- as.numeric(selected.row.index)
    selected.row <- player_pole[selected.row.index, 1]
    
    HTML(
      paste("These players were considered:\n\n"),
      paste(c("<pre>" , capture.output(print(data.frame(selected.row))), "</pre>"), collapse = "<br>")
    )
  })
  
  #salary
  output$salary <- renderUI({
    
    salarysum <- player_pole %>%
      filter(Player %in% input$custom.roster) %>%
      dplyr::group_by(`2019-20`) %>%
      summarise(sum = sum(`2019-20`))
    
    salarysum <- convert.money(salarysum)
    
    HTML(glue({"<h2>${salarysum}</h2>
      <h2>Salary</h2>"}))
  })
  
  
  #Probability
  output$probs <- renderUI({
    
    customized <- merge %>% filter(Player %in% input$custom.roster)
    
    pred <- predict.glm(model.boruta.glm, newdata = customized[, -1], type = "response")
    pred <- round(mean(pred),4)*100
    
    HTML(glue({"<h2>{pred}%</h2>
      <h2>Win prob.</h2>"}))
  })
  
  #age average
  output$age.ave <- renderUI({
    
    average.age <- player_pole %>%
      filter(Player %in% input$custom.roster) %>%
      dplyr::group_by(Age) %>% 
      summarise(mean = round(mean(Age),1))
    
    HTML(glue("<h2>{average.age}</h2>
              <h2>Age Average</h2>"))
  })
  
  #custom roster table
  output$customrostertable <- renderText({
    
    new.roster <- player_pole %>% 
      filter(Player %in% input$custom.roster) %>% 
      select(Player, Age, `2019-20`) %>% 
      mutate(`2019-20` = print.money(`2019-20`))
    
    kableExtra::kable(new.roster) %>%
      kable_styling(bootstrap_options = c("stiped", "hover"), 
                    stripe_color = "black",
                    full_width = TRUE) %>%
      column_spec(1, 
                  background = "rgba(225, 225, 225, 0.3)", 
                  color = "black") %>%
      row_spec(0, 
               background = "rgba(225, 225, 225, 0.3)", 
               color = "black") %>% 
      scroll_box(width = "100%", height = "300px")
  })
  
  #player worth
  output$worth <- renderHighchart({
    
    customized <- merge %>% filter(Player %in% input$custom.roster)
    
    pred <- predict.glm(model.boruta.glm, newdata = customized[, -1], type = "response")
    
    customized$pred <- pred
    worth <- customized %>% 
      left_join(player_pole, by = "Player") %>% 
      select(Player, pred, `2019-20`) %>% 
      mutate(pred = round(pred*100,4))
    
    worth$`2019-20`[worth$Player == "Andrew Harrison"] <- 277250
    worth$`2019-20`[worth$Player == "James Ennis III"] <- 1882867
    worth$`2019-20`[worth$Player == "Otto Porter Jr."] <- 27250576	
    worth$`2019-20`[worth$Player == "PJ Tucker"] <- 8349039
    worth$size <- round(worth$`2019-20`/worth$pred)
    worth$worthnet <- print.money(round(worth$`2019-20`/worth$pred))
    
    highchart() %>% 
      hc_chart(height = "350px") %>% 
      hc_add_series(worth, "scatter", hcaes(x = `2019-20`, y = pred, size = size, color = Player), showInLegend = FALSE) %>% 
      hc_tooltip(pointFormat = "<b>{point.Player}</b> 
                 <br> <b>Percentage:</b> {point.pred}
                 <br> <b>worth.net:</b> {point.worthnet}") %>% 
      hc_xAxis(title = list(text = "Salary (USD"),
               crosshair = list(color = "#000000",
                                snap = FALSE)) %>% 
      hc_yAxis(title = list(text = "Win Percentage"),
               crosshair = list(color = "#000000",
                                snap = FALSE),
               min = 0,
               max = 100) %>% 
      hc_add_theme(hc_theme_google())
    
  })
  
  
# PLAYER MAGNIFIER
  
  selected_player <- reactive({return(input$player)})
  
  #player image
  output$closeup <- renderUI({
    tags$img(src = paste0(selected_player(), ".png"), width = "300px", height = "260px")
  })
  
  #player bio
  output$bio <- renderFormattable({
   player.bio <- bio %>% select(input$player)
   formattable(player.bio, align = "left")
  })
  
  ##made-attemp and FILTER
  observe({
    updateSelectInput(session,
                      inputId = "versus",
                      label = "Pick opposing team:",
                      choices = sort(unique(shot.plot$OPP)))
  })
  
  observe({
    
    game <- shot.plot %>% 
      filter(PLAYER_NAME == input$player, OPP == input$versus)
    
    updateSelectInput(session,
                      inputId = "date_available",
                      label = "Select game date:",
                      choices = unique(game$GAME_DATE),
                      selected = game$GAME_DATE[1]
    )
    
  })
  
  
  output$shot_tracker <- renderPlot({
    
    shot.filter <- shot.plot %>% 
      filter(
        PLAYER_NAME == input$player,
        OPP == input$versus,
        GAME_DATE == input$date_available,
        PERIOD %in% input$period,
        EVENT_TYPE %in% input$event_type)
    
    ggplot(shot.filter, aes(x = LOC_X, y = LOC_Y)) +
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour = shot.filter$EVENT_TYPE), shape = 4, size = 2, stroke = 2) +
      guides(alpha = FALSE, size = FALSE, colour = guide_legend(override.aes = list(size = 4), ncol = 2)) +
      xlim(250, -250) +
      ylim(-50, 420) +
      coord_fixed(ratio = 1) +
      theme_void() +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            legend.position = "bottom",
            plot.title = element_text(size = 10, lineheight = 0.9, face = "bold"),
            plot.margin = unit(c(0, 0, 0, 0), "mm")) +
      scale_color_manual(values = c("#029268", "#9B1C31"))
  },
  height = 550,
  width = 725,
  bg = "transparent"
  )
  
  ##per distance
  output$perdistance <- renderHighchart({
    
    dist.filter <- shot.dist %>% filter(PLAYER_NAME == input$player)
    
    hchart(dist.filter, "areaspline", hcaes(x = SHOT_DISTANCE, y = n, group = EVENT_TYPE), color = c("#00471B", "#EEE1C6")) %>%
      hc_chart(spacingTop = 5) %>% 
      hc_plotOptions(areaspline = list(stacking = "normal",
                                       marker = list(enabled = FALSE))) %>%
      hc_xAxis(title = list(text = "Shot Distance (ft.)"),
               labels = list(style = list(color = "black")),
               max = 30,
               lineWidth = 2,
               lineColor = "#000000") %>%
      hc_yAxis(title = list(text = "Shot Frequency"),
               labels = list(style = list(color = "black")),
               lineWidth = 2,
               lineColor = "#000000") %>% 
      hc_title(text = "Shooting Distribution by Distance",
               style = list(fontWeight = "bold")) %>% 
      hc_add_theme(hc_theme_google())
  })
  

  ##per position
  output$perposition <- renderHighchart({
    
    left <- bucks %>% filter(PLAYER_NAME == input$player) %>% 
      filter(LOC_X < 0) %>% 
      mutate(x = ifelse(LOC_X > -246 & LOC_X <= -233.7, "221.4 - 233.7",
                        ifelse(LOC_X > -233.7 & LOC_X <= -221.4, "221.4 - 233.7 ",
                               ifelse(LOC_X > -221.4 & LOC_X <= -209.1, "209.1 - 221.4",
                                      ifelse(LOC_X > -209.1 & LOC_X <= -196.8, "196.8 - 209.1",
                                             ifelse(LOC_X > -196.8 & LOC_X <= -184.5, "184.5 - 196.8",
                                                    ifelse(LOC_X > -184.5 & LOC_X <= -172.2, "172.2 - 184.5",
                                                           ifelse(LOC_X > -172.2 & LOC_X <= -159.9, "159.9 - 172.2",
                                                                  ifelse(LOC_X > -159.9 & LOC_X <= -147.6, "147.6 - 159.9",
                                                                         ifelse(LOC_X > -147.6 & LOC_X <= -135.3, "135.5 - 147.6",
                                                                                ifelse(LOC_X > -135.3 & LOC_X <= -123, "123 - 135.5",
                                                                                       ifelse(LOC_X > -123 & LOC_X <= -110.7, "110.7 - 123",
                                                                                              ifelse(LOC_X > -110.7 & LOC_X <= -98.4, "98.4 - 110.7",
                                                                                                     ifelse(LOC_X > -98.4 & LOC_X <= -86.1, "86.1 - 98.4",
                                                                                                            ifelse(LOC_X > -86.1 & LOC_X <= -73.8, "73.8 - 86.1",
                                                                                                                   ifelse(LOC_X > -73.8 & LOC_X <= -61.5, "61.5 - 73.8",
                                                                                                                          ifelse(LOC_X > -61.5 & LOC_X <= -49.2, "49.2 - 61.5",
                                                                                                                                 ifelse(LOC_X > -49.2 & LOC_X <= -36.9, "36.9 - 49.2",
                                                                                                                                        ifelse(LOC_X > -36.9 & LOC_X <= -24.6, "24.6 - 36.9",
                                                                                                                                               ifelse(LOC_X > -24.6 & LOC_X <= -12.3, "12.3 - 24.6", "0 - 12.3"
                                                                                                                                               )
                                                                                                                                        )
                                                                                                                                 )
                                                                                                                          )
                                                                                                                   )
                                                                                                            )
                                                                                                     )
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
      )
      ) %>% 
      mutate(x = as.factor(x)) %>% 
      group_by(x, EVENT_TYPE) %>% 
      tally() %>% 
      ungroup() %>% 
      mutate(n = n*-1)
    
    right <- bucks %>% filter(PLAYER_NAME == input$player) %>% 
      filter(LOC_X > 0) %>% 
      mutate(x = ifelse(LOC_X < 246 & LOC_X >= 233.7, "221.4 - 233.7",
                        ifelse(LOC_X < 233.7 & LOC_X >= 221.4, "221.4 - 233.7 ",
                               ifelse(LOC_X < 221.4 & LOC_X >= 209.1, "209.1 - 221.4",
                                      ifelse(LOC_X < 209.1 & LOC_X >= 196.8, "196.8 - 209.1",
                                             ifelse(LOC_X < 196.8 & LOC_X >= 184.5, "184.5 - 196.8",
                                                    ifelse(LOC_X < 184.5 & LOC_X >= 172.2, "172.2 - 184.5",
                                                           ifelse(LOC_X < 172.2 & LOC_X >= 159.9, "159.9 - 172.2",
                                                                  ifelse(LOC_X < 159.9 & LOC_X >= 147.6, "147.6 - 159.9",
                                                                         ifelse(LOC_X < 147.6 & LOC_X >= 135.3, "135.5 - 147.6",
                                                                                ifelse(LOC_X < 135.3 & LOC_X >= 123, "123 - 135.5",
                                                                                       ifelse(LOC_X < 123 & LOC_X >= 110.7, "110.7 - 123",
                                                                                              ifelse(LOC_X < 110.7 & LOC_X >= 98.4, "98.4 - 110.7",
                                                                                                     ifelse(LOC_X < 98.4 & LOC_X >= 86.1, "86.1 - 98.4",
                                                                                                            ifelse(LOC_X < 86.1 & LOC_X >= 73.8, "73.8 - 86.1",
                                                                                                                   ifelse(LOC_X < 73.8 & LOC_X >= 61.5, "61.5 - 73.8",
                                                                                                                          ifelse(LOC_X < 61.5 & LOC_X >= 49.2, "49.2 - 61.5",
                                                                                                                                 ifelse(LOC_X < 49.2 & LOC_X >= 36.9, "36.9 - 49.2",
                                                                                                                                        ifelse(LOC_X < 36.9 & LOC_X >= 24.6, "24.6 - 36.9",
                                                                                                                                               ifelse(LOC_X < 24.6 & LOC_X >= 12.3, "12.3 - 24.6", "0 - 12.3"
                                                                                                                                               )
                                                                                                                                        )
                                                                                                                                 )
                                                                                                                          )
                                                                                                                   )
                                                                                                            )
                                                                                                     )
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
      )
      ) %>% 
      mutate(x = as.factor(x)) %>% 
      group_by(x) %>% 
      tally() %>% 
      ungroup()
    
    
    highchart() %>% 
      hc_chart(height = "350px",
               spacingTop = 5) %>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_add_series(left, "bar", hcaes(y = n, x = x), color = "#00471B", showInLegend = FALSE) %>% 
      hc_add_series(right, "bar", hcaes(y = n, x = x), color = "#EEE1C6", showInLegend = FALSE) %>% 
      hc_plotOptions(bar = list(stacking = "normal",
                                pointWidth = 8.5)) %>% 
      hc_xAxis(title = list(enabled = TRUE,
                            text = "Distance (ft.)"),
               reversed = FALSE) %>%
      hc_yAxis(title = list(enabled = TRUE,
                            text = "LEFT - RIGHT")) %>% 
      hc_title(text = "Shooting Distribution by Position",
               style = list(fontWeight = "bold"))
    
  })
  
  
  
  ##shot by team
  output$against <- renderHighchart({
    
    recap.filter <- recap %>% filter(PLAYER_NAME == selected_player())
    shot.average <- shot.recap %>% filter(PLAYER_NAME == selected_player())
    
    highchart() %>%
      hc_chart(height = "300px",
               spacingLeft = 5,
               spacingRight = 5,
               spacingBottom = 5,
               spacingTop = 5) %>% 
      hc_title(text = "Shooting Percentage By Team",
               style = list(fontWeight = "bold")) %>% 
      hc_xAxis(categories = levels(unique(recap.filter$OPP)),
               labels = list(style = list(color = "black"),
                             rotation = 45),
               lineColor = "#000000",
               lineWidth = 3) %>%
      hc_yAxis(title = list(text = "Percentage %"),
               labels = list(style = list(color = "black")),
               min = 0,
               plotLines = list(list(value = round(mean(shot.average$PROP),2),
                                     color = "red",
                                     dashStyle = "solid",
                                     width = 2,
                                     zIndex = 5,
                                     label = list(text = "Season Average",
                                                  align = "center",
                                                  style = list(color = "red", fontWeight = "bold")))),
               lineColor = "#000000",
               lineWidth = 3) %>%
      hc_add_series(recap.filter, "columnrange", hcaes(y = OPP, low = MIN, high = MAX), color = "#00471B", showInLegend = FALSE) %>%
      hc_plotOptions(columnrange = list(pointWidth = 30)) %>%
      hc_add_theme(hc_theme_google())
  })
  
  # GIF
  output$gif <- renderUI({
    
    selected.animation <- reactive({input$gif})
    
    tags$img(src = paste0(selected.animation(), ".gif"), width = "600px", height = "560px")
  })
  
  #game date picker
  observe({
    
    matchup.update <- BucksMatch %>% filter(OPP == input$team_b)
    
    updateSelectInput(session,
                      inputId = "matchup.date",
                      label = "Select game date:",
                      choices = unique(matchup.update$GAME.DATE),
                      selected = matchup.update$GAME.DATE[1]
    )
  })
  
  #Bucks/Opponent
  # game table
  output$matchsummary <- renderFormattable({
    
    if(input$switch == "Bucks"){
      
      if(input$quarter == "All"){
        
        BucksMatch <- BucksMatch %>%
          filter(TEAM == input$team_a,
                 OPP == input$team_b) %>% 
          select(-GAME.DATE, -Quarter, -TEAM, -OPP) %>% 
          mutate_if(is.character, as.numeric) %>% 
          mutate(MP = period_to_seconds(ms(MP))) %>% 
          group_by(Players) %>%
          summarise_at(.vars = vars("MP", "FGM", "FGA", "3PM", "3PA", "FTM", 
                                    "FTA", "ORB", "DRB", "TRB", "AST", "STL", 
                                    "BLK", "TOV", "PF", "PTS"),
                       .funs = sum, na.rm = TRUE) %>%       
          mutate(`FG%` = round(FGM/FGA*100, 2),
                 `3P%` = round(`3PM`/`3PA`*100, 2),
                 `FT%` = round(FTM/FTA*100, 2),
                 MP = lubridate::ms(substr(as.character(as.hms(MP)), start = 4, stop = 8))) %>% 
          arrange(desc(MP))
        
        BucksMatch <- BucksMatch[, c(1:4, 18, 5:6, 19, 7:8, 20, 9:17)]
        BucksMatch[is.na(BucksMatch)] <- "-"
        
        formattable(BucksMatch,
                    align = c("l", rep("r", NCOL(BucksMatch)-1)),
                    list(area(col = 14:20) ~ color_bar("#EEE1C6")
                    )
        )
      }
      else{
        BucksMatch <- BucksMatch %>% 
          filter(TEAM == input$team_a,
                 OPP == input$team_b,
                 Quarter == input$quarter) %>% 
          select(-GAME.DATE, -Quarter, -TEAM, -OPP) %>% 
          arrange(desc(MP))
        
        BucksMatch[is.na(BucksMatch)] <- ""
        
        formattable(BucksMatch,
                    align = c("l", rep("r", NCOL(BucksMatch)-1)),
                    list(area(col = 14:20) ~ color_bar("#EEE1C6")
                    )
        )
      }
    }
    else{
      if(input$quarter == "All"){
        BucksMatch <- BucksMatch %>%
          filter(TEAM == input$team_b,
                 OPP == input$team_a) %>% 
          select(-GAME.DATE, -Quarter, -TEAM, -OPP) %>% 
          mutate_if(is.character, as.numeric) %>% 
          mutate(MP = period_to_seconds(ms(MP))) %>% 
          group_by(Players) %>%
          summarise_at(.vars = vars("MP", "FGM", "FGA", "3PM", "3PA", "FTM", 
                                    "FTA", "ORB", "DRB", "TRB", "AST", "STL", 
                                    "BLK", "TOV", "PF", "PTS"),
                       .funs = sum, na.rm = TRUE) %>%       
          mutate(`FG%` = round(FGM/FGA*100, 2),
                 `3P%` = round(`3PM`/`3PA`*100, 2),
                 `FT%` = round(FTM/FTA*100, 2),
                 MP = lubridate::ms(substr(as.character(as.hms(MP)), start = 4, stop = 8))) %>% 
          arrange(desc(MP))
        
        BucksMatch <- BucksMatch[, c(1:4, 18, 5:6, 19, 7:8, 20, 9:17)]
        BucksMatch[is.na(BucksMatch)] <- "-"
        
        formattable(BucksMatch,
                    align = c("l", rep("r", NCOL(BucksMatch)-1)),
                    list(area(col = 14:20) ~ color_bar("#EEE1C6")
                    )
        )
        
      }
      else{
        BucksMatch <- BucksMatch %>% 
          filter(TEAM == input$team_b,
                 OPP == input$team_a,
                 Quarter == input$quarter) %>% 
          select(-GAME.DATE, -Quarter, -TEAM, -OPP) %>% 
          arrange(desc(MP))
        
        BucksMatch[is.na(BucksMatch)] <- ""
        
        formattable(BucksMatch,
                    align = c("l", rep("r", NCOL(BucksMatch)-1)),
                    list(area(col = 14:20) ~ color_bar("#EEE1C6")
                    )
        )
      }
    }
  })
})
