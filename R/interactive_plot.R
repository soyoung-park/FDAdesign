#' Optimal Sampling Design for Functional Data Analysis - interactive plots 
#'
#' Produces interactive graphics to visualize objective functions for selecting
#' optimal sampling points for p = 1, 2, and 3.
#' 
#'
#' @param result   list of results of \code{opt_design_fda} for p = 1, 2, 3; i.e. selection_p(p_vec = 1:3,...)
#' @param labels_candidate_pts   candidate points for x-label on interactive plots; 
#'        default = NULL is equivalent to 1:number of candidate points.
#' 
#' @return user interface illustrating objective functions for p = 1, 2, 3.
#' @examples
#' # see example for opt_design_fda() function
#' @author So Young Park \email{spark13@@ncsu.edu},
#' Luo Xiao \email{lxiao5@@ncsu.edu},
#' Ana-Maria Staicu \email{astaicu@@ncsu.edu}
#' @references \url{}
#' @seealso \code{\link{opt_design_fda}} /
#' \code{\link{selection_p}} /
#' \code{\link{interactive_plot}}
#' @import shiny
#' @import ggplot2
#' @export
interactive_plot <- function(result, labels_candidate_pts = NULL){

  require(shiny)
  require(ggplot2)
 
  opt_result <- result$opt_result
  if(is.null(labels_candidate_pts)) labels_candidate_pts <- c(opt_result[[1]]$index_all_comb)
  
  result_obj_eval <- result_opt_index <- result_opt_obj <- list()
  for(p in 1:3){
    # p = 1 ; res <- opt_result[[p]]
    result_obj_eval[[p]] <- opt_result[[p]]$obj_eval_all
    result_opt_index[[p]] <- opt_result[[p]]$index_opt
    result_opt_obj[[p]] <- opt_result[[p]]$obj_opt
  }
  Phi <- result$INPUT$Phi
  vec.choice <- labels_candidate_pts
  ####################################################
  #  App 
  ####################################################
  shinyApp(
    
    ####################################################
    #  UI
    ####################################################
    
    ui = navbarPage( 
      title = h3("Optimal Design for Functional Data"), 
      windowTitle = "OptimalDesign", 
      style = "font-size: 20px;line-height: 15px;  padding: 15px;",
      collapsible = FALSE, id = "nav",
      inverse = FALSE, header = NULL,
      fluid = TRUE,       
    
      ### TAB 1 :
      tabPanel(
        h3("One Point (p = 1)"), style = "font-size: 18px",
        fluidRow(
            column(width = 2,
                   wellPanel(radioButtons("p1_checkbox", label = "Select:", 
                                    choices = list("Prediction error" = 1, 
                                                  "Show the optimal point" = 2,
                                                  "Select a sampling point" = 3),
                                                  selected = 1),

                             conditionalPanel( 
                                     condition = "input.p1_checkbox == 3",
                                     tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
                                     selectInput("p1_pt1", label = "for 1st point", 
                                                 choices =  vec.choice, 
                                                 selected = 1)
                              )
                     )
                   ),
            column(width=7,  plotOutput("p1_plot1")),
            column(width=3,  plotOutput("p1_plot2"))
        )
      ),
    
    ### TAB 2 :
    tabPanel(h3("Two Points (p = 2)"), style = "font-size: 18px",
             fluidRow(
               column(width = 2,
                      wellPanel(radioButtons("p2_checkbox", label = "Select:",
                                      choices = list("Prediction error"= 1,
                                                     "Show the optimal points" = 2,
                                                     "Select sampling points" = 3),
                                      selected = 1),

                                conditionalPanel(
                                     condition = "input.p2_checkbox == 3",
                                     tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
                                     selectInput("p2_pt1", label = "for 1st point",
                                                 choices = vec.choice,
                                                 selected = 1)
                                ),
                                uiOutput("p2_pt2")
                       )
                      ),
             column(width=7, plotOutput("p2_plot1")),
             column(width=3, plotOutput("p2_plot2"))
          )
      ),

    ### TAB 3 :
    tabPanel(h3("Three Points (p = 3)"), style = "font-size: 18px",
             fluidRow(
               column( width = 2,
                       wellPanel(radioButtons("p3_checkbox", label = "Select:",
                                       choices = list("Prediction error"= 1,
                                                      "Show the optimal points" = 2,
                                                      "Select sampling points" = 3),
                                       selected = 1),

                                 conditionalPanel(
                                      condition = "input.p3_checkbox == 3",
                                      selectInput("p3_pt1", label = "for 1st point",
                                                  choices = vec.choice,
                                                  selected = 1)
                                 ),
                                uiOutput("p3_pt2"),
                                uiOutput("p3_pt3")
                        )
                       ),
               column( width = 7, plotOutput("p3_plot1", height = "100%", width = "100%")),
               column( width = 3, plotOutput("p3_plot2", height = "100%", width = "100%"))
          )
      )
    ), # end ui 
    
    ##########################################################################################      
    
        
    ####################################################
    #  Server 
    ####################################################
    server = function(input, output){
      range_y0 <<-  range(unlist(result_obj_eval))
      range_y <<- c(0, range_y0[2])
      tick <<- seq(1, nrow(Phi), by = 2)
      
      # === render UI === #
      
      # ##########################################################################################      
      output$p2_pt2 <- renderUI({

        vec.ch <-  vec.choice[(which(vec.choice == input$p2_pt1)+1):length(vec.choice)]
        vec.ch <- c("NA", vec.ch)

        conditionalPanel(
          condition = "input.p2_checkbox == 3 && input.p2_pt1 > 0",
          tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
          selectInput("p2_pt2", label = "for second point",
                      choices =  vec.ch,
                      selected = 1)

        )
      })
       
      output$p3_pt2 <- renderUI({

        vec.ch1 <-  vec.choice[(which(vec.choice == input$p3_pt1)+1):length(vec.choice)]
        vec.ch1 <- c("NA", vec.ch1)

        conditionalPanel(
          condition = "input.p3_checkbox == 3 && input.p3_pt1 > 0",
          tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
          selectInput("p3_pt2", label = "for 2nd point",
                      choices =  vec.ch1,
                      selected = 1)

        )

      })

      ##########################################################################################
      output$p3_pt3 <- renderUI({
        if(  input$p3_pt2=="NA" || length(input$p3_pt2) < 1 ){
          conditionalPanel(
            condition = "input.p3_checkbox == 3 && input.p3_pt1 > 0",
            tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
            selectInput("p3_pt3", label = "for 3rd point",
                        choices = c("NA"),
                        selected = 1)
          )
        }else{
          vec.ch2 <-  vec.choice[(which(vec.choice == input$p3_pt2)+1):length(vec.choice)]
          #vec.ch2 <- paste0("t = ", vec.ch2)
          vec.ch2 <- c("NA", vec.ch2)

          conditionalPanel(
            condition = "input.p3_checkbox == 3 && input.p3_pt1 > 0",
            tags$style(type='text/css', ".selectize-dropdown { font-size: 15px;  line-height: 15px; } .selectize-input { font-size: 15px; line-height: 15px;  padding: 15px; }"),
            selectInput("p3_pt3", label = "for 3rd point",
                        choices =  vec.ch2,
                        selected = 1)
          )

        }
      })

      
      # === plots === #
      
      ##########################################################################################
      onePoint1 <- reactive({

        optNum <-1
        temp <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[optNum]])); colnames(temp) <- c("x1","y")

        p <- ggplot(temp, aes(x=x1,y=y))  + theme_bw() +  ylim(range_y)  +   geom_line(aes(color=y), size=2)  + 
          scale_colour_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
        p <- p + scale_x_continuous(limits = c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
          xlab("Candidate Sampling Points") + ylab("") +
          ggtitle(paste0('Prediction Errors for One Point'))
        p <- p +  theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                        legend.title = element_blank(),
                        axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                        axis.title.x = element_text(size=rel(2), margin = margin(20,20,20,20)),
                        axis.text = element_text(size=rel(1.5)),
                        plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"),
                        plot.title = element_text(size=30, margin = margin(0,0,17,0)))

        if(input$p1_checkbox==2){

          p <- p + geom_point(data = temp[which(temp$y == min(temp$y)),], aes(x = x1, y = y), colour = 'red', size = 5)
          p <- p + annotate("text", fontface="bold", size=8, colour = "red",
                            x = temp[which(temp$y == min(temp$y)),"x1"],
                            y = min(temp$y)-diff(range_y)/15,
                            label = paste0('t = ', vec.choice[ (temp[which(temp$y == min(temp$y)), "x1"])] ,'; PE = ',
                                           round(min(temp$y, na.rm=TRUE), digits=3)))


          a <- vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=8,
                            x = length(vec.choice)-6,
                            y = diff(range_y)/5, na.rm = TRUE,
                            label = paste0('Optimal schedule = (', a,") \nwith prediction error = ", round(min(temp$y), digits=3)) )

        }

        if(input$p1_checkbox==3){

          p <- p + geom_point(data = temp[which(temp$y == min(temp$y)),], aes(x = x1, y = y), colour = 'red', size = 5)
          p <- p + annotate("text", fontface="bold", size=8, colour = "red", x = temp[which(temp$y == min(temp$y)),"x1"],
                            y = min(temp$y)-diff(range_y)/15,
                            label = paste0('t = ', vec.choice[ (temp[which(temp$y == min(temp$y)),"x1"])] ,'; PE = ',
                                           round(min(temp$y, na.rm=TRUE), digits=3)))

          a <- vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=8,
                            x = length(vec.choice)-6,
                            y = 2*diff(range_y)/5, na.rm = TRUE,
                            label = paste0('Optimal schedule = (', a,") \nwith prediction error = ", round(min(temp$y), digits=3)) )


          ind <- which(vec.choice == input$p1_pt1)
          p <- p + geom_point(data = temp[ind,], aes(x = x1, y = y), colour = 'deepskyblue4', size = 5, shape = 17)
          p <- p + annotate("text",size=8, colour = "deepskyblue4", 
                            x = length(vec.choice)-6,
                            y = diff(range_y)/5, na.rm = TRUE,
                            label = paste0("User's choice = (", input$p1_pt1, ")\nwith prediction error = ", round(temp$y[ind], digits=3)))
        }
        p
    })

      onePoint2 <- reactive({

        optNum <-1
        temp <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[optNum]])); colnames(temp) <- c("x1","y")
        bar_val <- data.frame(x= c(0,0), y=factor(c("User","Opt"), levels=c("User","Opt")))
        lab <- c("","")


        if(input$p1_checkbox==2){
          bar_val[2,1] <- round(min(temp$y), digits=3)
          a <- vec.choice[result_opt_index[[optNum]]]

          lab[2] <- paste0("(",a,")")
          lab[1] <- paste0('')
        }

        if(input$p1_checkbox==3){
          ind <- which(vec.choice == input$p1_pt1)
          bar_val[1,1] <- temp$y[ind]
          bar_val[2,1] <- round(min(temp$y), digits=3)

          a<- vec.choice[result_opt_index[[optNum]]]
          lab[2] <- paste0("(",a,")")
          lab[1] <- paste0('(', input$p1_pt1, ")")
        }

        pbar <- ggplot(data=bar_val, aes(x=y, y=x, fill = x)) + theme_bw() +
          geom_bar(colour="black", stat="identity", position="dodge") +
          guides(fill=FALSE)+  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1]) +

          ylim(c(0,range_y[2]+diff(range_y)/5)) +
          # ggtitle(paste0('Comparison of Prediction Errors')) +
          xlab("") +ylab("") +
          theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                legend.title = element_blank(),
                axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.title.x = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.text.y = element_text(size=rel(1.5)),
                axis.text.x = element_text(size=rel(2), angle = 90, hjust=1, vjust = 0),
                plot.margin = unit(c(3, 0.1, 0.5, -2), "cm"),
                panel.margin = unit(0,"cm"),

                plot.title = element_text(size=30, margin = margin(20,20,20,20)))+
          geom_text(aes(label=lab), position=position_dodge(width=0.8), hjust= -0.05, size=8, angle = 90)


        pbar


      })
      
      # ##########################################################################################      
      twoPoint1 <- reactive({
        optNum <- 2
        temp <- data.frame(cbind(t(combn(1:length(vec.choice), 2)), result_obj_eval[[optNum]])); colnames(temp) <- c("x1","x2","y")

        if(input$p2_checkbox == 1){

          p <- ggplot(temp, aes(x=x1, y=x2, fill=y))  +   theme_bw() +  geom_tile()
          p <- p +  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
          p <- p + scale_y_continuous(breaks=tick, labels=vec.choice[tick]) +
            scale_x_continuous(breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (1st)") + ylab("Candidate Sampling Points (2nd)") + 
            ggtitle('Prediction Errors for Two Points')
          p <- p +  theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                          legend.title = element_blank(),

                          axis.title.y = element_text(size=rel(2), margin = margin(20,20,20,20)),
                          axis.title.x = element_text(size=rel(2), margin = margin(20,20,20,20)),
                          axis.text = element_text(size=rel(1.5)),
                          plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"),
                          plot.title = element_text(size=30, margin = margin(0,0,17,0)))
          
        } else if(input$p2_checkbox == 2){

          p <- ggplot(temp, aes(x=x1,y=x2, fill=y))  +   geom_tile() + theme_bw()
          p <- p +  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
          p <- p + scale_y_continuous(breaks=tick, labels=vec.choice[tick]) +
            scale_x_continuous(breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (1st)") + ylab("Candidate Sampling Points (2nd)") + ggtitle('Prediction Errors for Two Scans')
          p <- p +  theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                          legend.title = element_blank(),
                          axis.title.y = element_text(size=rel(2), margin = margin(20,20,20,20)),
                          axis.title.x = element_text(size=rel(2), margin = margin(20,20,20,20)),
                          axis.text = element_text(size=rel(1.5)),
                          plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"),
                          plot.title = element_text(size=30, margin = margin(0,0,17,0)))

          p <- p + geom_point(data = temp[which(temp$y == min(temp$y)),] , aes(x = x1, y = x2), colour = 'red', size = 7) +
            annotate("text", fontface="bold",size=7, colour = "red", x = temp[which(temp$y == min(temp$y)),"x1"],
                     y = temp[which(temp$y == min(temp$y)),"x2"] - length(vec.choice)/15,
                     label = paste0('t = (', vec.choice[temp[which(temp$y == min(temp$y)),"x1"]],", ",
                                    vec.choice[temp[which(temp$y == min(temp$y)),"x2"]],') ; PE = ', round(min(temp$y, na.rm=TRUE), digits=3)))
          
          a <- vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=7, 
                            x = length(vec.choice) - 5,
                            y = 5,
                            label = paste0('Optimal schedule = (',a[1],", ",a[2],")\nwith prediction error = ", round(result_opt_obj[[optNum]],digits=3)))
       }else if(input$p2_checkbox == 3){

          ind.first <- which(vec.choice == input$p2_pt1)
          temp_ <- temp[which(temp$x1 == ind.first), ]

          a <- input$p2_pt1
          p <- ggplot(temp_, aes(x=x2,y=y)) + theme_bw()  + ylim(range_y)  +   geom_line(aes(color=y), size=2)  + 
            scale_colour_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
          p <- p + scale_x_continuous(limits = c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (2nd)") + ylab("") +
            ggtitle( expression(atop("Prediction Errors for Two Points", atop(italic("(with 1st point at user's choice)")))) )

          p <- p +  theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                          legend.title = element_blank(),
                          axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                          axis.title.x = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                          axis.text = element_text(size=rel(1.5)),
                          plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"),
                          plot.title = element_text(size=30, margin = margin(0,0,17,0)))
          
          if(vec.choice[(temp_[which(temp_$y == min(temp_$y)),"x2"])] > (length(vec.choice)-8)){
            p <- p + geom_point(aes(x = temp_[which(temp_$y == min(temp_$y)),"x2"], y = min(temp_$y)), colour = 'red', size = 7)
            p <- p + annotate("text", fontface="bold",size=7, colour = "red",
                              x = temp_[which(temp_$y == min(temp_$y)),"x2"]-2,
                              y = min(temp_$y)-diff(range_y)/15,
                              label = paste0('t = ', vec.choice[(temp_[which(temp_$y == min(temp_$y)),"x2"])] ,'; PE = ', round(min(temp_$y, na.rm=TRUE), digits=3)))
          }else{
            p <- p + geom_point(aes(x = temp_[which(temp_$y == min(temp_$y)),"x2"], y = min(temp_$y)), colour = 'red', size = 7)
            p <- p + annotate("text", fontface="bold",size=7, colour = "red", x = temp_[which(temp_$y == min(temp_$y)),"x2"],
                              y = min(temp_$y)-diff(range_y)/15,
                              label = paste0('t = ', vec.choice[(temp_[which(temp_$y == min(temp_$y)),"x2"])] ,'; PE = ', round(min(temp_$y, na.rm=TRUE), digits=3)))
          }
          a <- vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=7,
                            x = 6,
                            y = 2*diff(range_y)/5,
                            na.rm = TRUE,
                            label = paste0('Optimal schedule = (',a[1],", ",a[2],")\nwith prediction error = ", round(result_opt_obj[[optNum]],digits=3)))


          if(input$p2_pt2 != "NA"){
            ind.second <- which(temp_$x2 == which( vec.choice ==as.numeric(input$p2_pt2)))
            temp__ <- temp_[ind.second,]
            p <- p + geom_point(data =  temp__, aes(x = x2 , y = y), colour = 'deepskyblue4', size = 5, shape = 17, na.rm=TRUE)
            p <- p + annotate("text",size=7, colour = "deepskyblue4", x = 6,
                              y = diff(range_y)/5, na.rm = TRUE,
                              label = paste0("User's choice = (", input$p2_pt1, ", ", input$p2_pt2, ")\nwith prediction error = ",
                                             round(min(temp__$y, na.rm=TRUE), digits=3)))
          }else{
            temp__1 <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[1]])); colnames(temp__1) <- c("x1","y")
            p <- p + annotate("text",size=7, colour = "deepskyblue4", 
                              x = 6,
                              y = diff(range_y)/5, na.rm = TRUE,
                              label = paste0("User's choice = (", input$p2_pt1, ")\nwith prediction error = ",
                                             round( temp__1$y[which(vec.choice==input$p2_pt1)] , digits=3)))
          }
        }
        p
      })
       
      twoPoint2 <- reactive({
        optNum <- 2
        temp <- data.frame(cbind(t(combn(1:length(vec.choice), 2)), result_obj_eval[[optNum]])); colnames(temp) <- c("x1","x2","y")
        bar_val <- data.frame(x= c(0,0,0,0), y=factor(c("(User)", "(User, User)",
                                                        "(User, Opt)", "(Opt, Opt)"),
                                                      levels=c("(User)", "(User, User)",
                                                               "(User, Opt)", "(Opt, Opt)")))
        lab <- c("", "", "", "")
        if(input$p2_checkbox == 2){

          a<-vec.choice[result_opt_index[[optNum]]]
          lab[4] <- paste0( '(',a[1],", ",a[2],")"  )
          bar_val[4,1] <- round(result_opt_obj[[optNum]], digits=3)

        }else if(input$p2_checkbox == 3){

          ind.first <- which(vec.choice == input$p2_pt1)
          temp_ <- temp[which(temp$x1 == ind.first), ]
          bar_val[3,1] <- round(min(temp_$y, na.rm=TRUE), digits=3)
          lab[3] <- paste0('(', input$p2_pt1, ", ", vec.choice[(temp_[which(temp_$y == min(temp_$y)),"x2"])],")")
          a<-vec.choice[result_opt_index[[optNum]]]
          
          if(input$p2_pt2 != "NA"){
            ind.second <- which(temp_$x2 == which(vec.choice==as.numeric(input$p2_pt2)))
            temp__ <- temp_[ind.second,]
            bar_val[2,1] <- round(min(temp__$y, na.rm=TRUE), digits=3)
            lab[2] <- paste0('(', input$p2_pt1, ", ", input$p2_pt2, ")")
          }else if(input$p2_pt2 == "NA"){
            temp_week1 <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[1]])); colnames(temp_week1) <- c("x1","y")
            bar_val[1,1] <- round( temp_week1$y[which(vec.choice==as.numeric(input$p2_pt1))] , digits=3)
            lab[1] <- paste0("")
          }

          temp_week1 <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[1]])); colnames(temp_week1) <- c("x1","y")
          bar_val[1,1] <- round( temp_week1$y[which(vec.choice==input$p2_pt1)] , digits=3)
          lab[1] <- paste0('(', input$p2_pt1, ")")

          a <- vec.choice[result_opt_index[[optNum]]]
          lab[4] <- paste0( '(',a[1],", ",a[2],")"  )
          bar_val[4,1] <- round(result_opt_obj[[optNum]], digits=3)


        }
        pbar <- ggplot(data=bar_val, aes(x=y, y=x, fill = x)) +  theme_bw() +
          geom_bar(colour="black", stat="identity", position="dodge") +
          guides(fill=FALSE)+  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1]) +
          ylim(c(0,range_y[2]+diff(range_y)/5)) +
          
          xlab("") +ylab("") +
          theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                legend.title = element_blank(),
                axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.title.x = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.text.y = element_text(size=rel(1.5)),
                axis.text.x = element_text(size=rel(2), angle=90, hjust=1, vjust = 0),
                plot.margin = unit(c(3, 0.1, 0.5, -2), "cm"),
                panel.margin = unit(0,"cm"),

                plot.title = element_text(size=30, margin = margin(20,20,20,20)))+
          geom_text(aes(label=lab), position=position_dodge(width=0.8), hjust= -0.05, size=8, angle = 90)

        pbar
      })
      # 
      # ##########################################################################################
      threePoint1 <- reactive({

        optNum = 3
        
        ind.first.opt <- result_opt_index[[optNum]][1]
        temp.all <- data.frame(cbind(t(combn(1:length(vec.choice),3)), result_obj_eval[[optNum]])); colnames(temp.all) <- c("x1","x2","x3","y")
        temp <- temp.all[which(temp.all$x1==ind.first.opt),]
        colnames(temp) <- c("x1","x2","x3","y")



        if(input$p3_checkbox == 1){
          p <- ggplot(temp, aes(x=x2,y=x3, fill=y))  +   geom_tile() +theme_bw()
          p <- p +  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
          p <- p + scale_y_continuous(limits=c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
            scale_x_continuous(limits=c(1,length(vec.choice)),breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (2nd)") + ylab("Candidate Sampling Points (3rd)") +
            ggtitle(expression(atop("Prediction Errors for Three Points", atop(italic("(with 1st point at its optimal)"), ""))))



        }else if(input$p3_checkbox == 2){
          p <- ggplot(temp, aes(x=x2,y=x3, fill=y))  +   geom_tile() + theme_bw()
          p <- p +  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")

          p <- p + scale_y_continuous(limits=c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
            scale_x_continuous(limits=c(1,length(vec.choice)),breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (2nd)") + ylab("Candidate Sampling Points (3rd)") +
            ggtitle(expression(atop("Prediction Errors for Three Points", atop(italic("(with 1st point at its optimal)"), ""))))
          
          p <- p + geom_point(data = temp[which(temp$y == min(temp$y)),], aes(x = x2, y = x3), colour = 'red', size = 5, na.rm=TRUE) +
            annotate("text", fontface="bold",size=7, colour = "red", x = temp[which(temp$y == min(temp$y)),"x2"],
                     y = temp[which(temp$y == min(temp$y)),"x3"]- length(vec.choice)/15, na.rm=TRUE,
                     label = paste0('t = (',vec.choice[temp[which(temp$y == min(temp$y)),"x1"]], ", ", vec.choice[temp[which(temp$y == min(temp$y)),"x2"]],", ",
                                    vec.choice[temp[which(temp$y == min(temp$y)),"x3"]],') ; PE = ', round(min(temp$y, na.rm=TRUE), digits=3)  ))


          a<- vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=7,
                            x = length(vec.choice) - 5,
                            y = 5,
                            label = paste0('Optimal schedule = (',a[1],", ",a[2],", ",a[3],")\nwith prediction error = ", round(result_opt_obj[[optNum]],digits=3)))



        }else if(input$p3_checkbox == 3){
          
          a <- vec.choice[result_opt_index[[optNum]]]
          ind.first <- which(vec.choice == input$p3_pt1)
          temp <- temp.all[which(temp.all$x1==ind.first),]
          colnames(temp) <- c("x1","x2","x3","y")

          p <- ggplot(temp, aes(x=x2,y=x3, fill=y))  +   geom_tile() + theme_bw()
          p <- p +  scale_fill_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
          p <- p + scale_y_continuous(limits=c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
            scale_x_continuous(limits=c(1,length(vec.choice)),breaks=tick, labels=vec.choice[tick]) +
            xlab("Candidate Sampling Points (2nd)") + ylab("Candidate Sampling Points (3rd)") +
            ggtitle(expression(atop("Prediction Errors for Three Points", atop(italic("(with 1st point at user's choice)"), ""))))

          atemp <- temp[which(temp$y == min(temp$y)),]
          p <- p + geom_point(data = atemp, aes(x = x2, y = x3), colour = 'red', size = 3, na.rm=TRUE) +
            annotate("text", fontface="bold",size=7, colour = "red", x = atemp$x2,
                     y = atemp$x3 - length(vec.choice)/10, na.rm=TRUE,
                     label = paste0('t = (', input$p3_pt1,", ", vec.choice[atemp$x2]  ,", ",
                                    vec.choice[atemp$x3],') ; PE = ', round(min(temp$y, na.rm=TRUE), digits=3),
                                    "\n(optimal given 1st point at user's choice)"))

          a <-  vec.choice[result_opt_index[[optNum]]]
          p <- p + annotate("text", fontface="bold", size=7, 
                            x = length(vec.choice) - 5,
                            y = 5,
                            label = paste0('Optimal schedule = (',a[1],", ",a[2],", ",a[3],")\nwith prediction error = ", round(result_opt_obj[[optNum]],digits=3)))

          if(input$p3_pt2 != "NA"){

            ind.first <- which(vec.choice == input$p3_pt1)
            ind.second <- which(vec.choice == input$p3_pt2)
            temp_ <- temp.all[which(temp.all$x1==ind.first),]
            temp <- temp_[which(temp_$x2==ind.second),]
            colnames(temp) <- c("x1","x2","x3","y")

            p <- ggplot(temp, aes(x=x3,y=y))  + theme_bw() + ylim(range_y)  +   geom_line(aes(color=y), size=2)  + 
              scale_colour_gradientn(limits=range_y0, colours = rainbow(9)[7:1], name = "Obj")
            p <- p + scale_x_continuous(limits = c(1,length(vec.choice)), breaks=tick, labels=vec.choice[tick]) +
              xlab("Candidate Sampling Points (3rd)") + ylab("") +
              ggtitle(expression(atop("Prediction Errors for Three Points", atop(italic("(with 1st & 2nd points at user's choice)"), ""))))

            a <- vec.choice[result_opt_index[[optNum]]]
            p <- p + annotate("text", fontface="bold", size=7, 
                              x = length(vec.choice)-6,
                              y = range_y[2]-diff(range_y)/10, na.rm = TRUE,
                              label = paste0('Optimal schedule = (',a[1],", ",a[2],", ",a[3],")\nwith prediction error = ", round(result_opt_obj[[optNum]],digits=3)))

            p <- p + geom_point(data = temp[which(temp$y == min(temp$y)),], aes(x = x3, y = y), colour = 'red', size = 5)
            
            p <- p + annotate("text", fontface="bold", size=7, colour = "red", x = temp[which(temp$y == min(temp$y)),"x3"],
                                y = min(temp$y)-diff(range_y)/10,na.rm = TRUE,
                                label = paste0('t = (', vec.choice[ (temp[which(temp$y == min(temp$y)),"x1"])] ,", ",
                                               vec.choice[ (temp[which(temp$y == min(temp$y)),"x2"])],", ",
                                               vec.choice[ (temp[which(temp$y == min(temp$y)),"x3"])], '); PE = ',
                                               round(min(temp$y, na.rm=TRUE), digits=3),"\n(optimal given 1st & 2nd points at user's choice)"))
            
            if(input$p3_pt3 != "NA"){
              ind.third <- which(vec.choice == input$p3_pt3)
              temp_ <- temp[which(temp$x3==ind.third),]
              p <- p + geom_point(data =  temp_, aes(x = x3 , y = y),
                                  colour = 'deepskyblue4', size = 5, shape = 17, na.rm=TRUE)
              p <- p + annotate("text",size=7, colour = "deepskyblue4", x = length(vec.choice)-6,
                                y = range_y[2]- 0.35*diff(range_y), na.rm = TRUE,
                                label = paste0("User's choice = (", input$p3_pt1, ", ", input$p3_pt2, ", ", input$p3_pt3,
                                               ")\nwith prediction error = ",
                                               round(min(temp_$y, na.rm=TRUE), digits=3)))


            }else if(input$p3_pt3 == "NA"){

              temp_week12 <- data.frame(cbind(t(combn(1:length(vec.choice),2)), result_obj_eval[[2]])); colnames(temp_week12) <- c("x1","x2","y")
              y_ <-temp_week12$y[which((temp_week12$x1 == which(vec.choice==input$p3_pt1)) * (temp_week12$x2 == which(vec.choice==input$p3_pt2))==1)]
              p <- p + annotate("text",size=7, colour = "deepskyblue4",x = length(vec.choice)-6,
                                y = range_y[2]- 0.35*diff(range_y), na.rm = TRUE,
                                label = paste0("User's choice = (", input$p3_pt1, ", ", input$p3_pt2, ")\nwith prediction error = ",
                                               round(y_, digits=3)))


            }


          }else if(input$p3_pt2 == "NA"){

            temp_week1 <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[1]])); colnames(temp_week1) <- c("x1","y")
            p <- p + annotate("text",size=7, colour = "deepskyblue4", x = length(vec.choice) - 5,
                              y = 10, na.rm = TRUE,
                              label = paste0("User's choice = (", input$p3_pt1, ")\nwith prediction error = ",
                                             round( temp_week1$y[which(vec.choice==input$p3_pt1)] , digits=3)))

          }
        }

        p <- p +  theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                        legend.title = element_blank(),
                        axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                        axis.title.x = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                        axis.text = element_text(size=rel(1.5)),
                        plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"),
                        plot.title = element_text(size=30, margin = margin(0,0,17,0)))
        p
      })
      
      
      threePoint2 <- reactive({
        
        optNum = 3
        ind.first.opt <- result_opt_index[[optNum]][1]
        temp.all <- data.frame(cbind(t(combn(1:length(vec.choice),3)), result_obj_eval[[optNum]])); colnames(temp.all) <- c("x1","x2","x3","y")
        temp <- temp.all[which(temp.all$x1==ind.first.opt),]
        colnames(temp) <- c("x1","x2","x3","y")


        bar_val <- data.frame(x= c(0,0,0,0,0,0), y=factor(c("(User)", "(User, User)","(User, User, User)",
                                                            "(User, User, Opt)","(User, Opt, Opt)", "(Opt, Opt, Opt)"),
                                                          levels=c("(User)", "(User, User)","(User, User, User)",
                                                                   "(User, User, Opt)","(User, Opt, Opt)", "(Opt, Opt, Opt)")))
        lab <- rep("", 6)

        if(input$p3_checkbox == 2){

          a <-  vec.choice[result_opt_index[[optNum]]]
          bar_val[6,1] <- round(result_opt_obj[[optNum]],digits=3)
          lab[6] <- paste0('(',a[1],", ",a[2],", ",a[3],")")


        }else if(input$p3_checkbox == 3){
          a<- vec.choice[result_opt_index[[optNum]]]
          bar_val[6,1] <- round(result_opt_obj[[optNum]],digits=3)
          lab[6] <- paste0('(',a[1],", ",a[2],", ",a[3],")")

          ind.first <- which(vec.choice == input$p3_pt1)
          temp <- temp.all[which(temp.all$x1==ind.first),]
          colnames(temp) <- c("x1","x2","x3","y")

          atemp <- temp[which(temp$y == min(temp$y)),]

          bar_val[5,1] <- round(min(temp$y, na.rm=TRUE), digits=3)
          lab[5] <- paste0('(', input$p3_pt1,", ", vec.choice[atemp$x2]  ,", ", vec.choice[atemp$x3],')')

          temp_week1 <- data.frame(cbind(1:length(vec.choice), result_obj_eval[[1]])); colnames(temp_week1) <- c("x1","y")
          bar_val[1,1] <- round( temp_week1$y[which(vec.choice==input$p3_pt1)] , digits=3)
          lab[1] <- paste0( '(',input$p3_pt1,')' )


          if(input$p3_pt2 != "NA"){

            ind.first <- which(vec.choice == input$p3_pt1)
            ind.second <- which(vec.choice == input$p3_pt2)
            temp_ <- temp.all[which(temp.all$x1==ind.first),]
            temp <- temp_[which(temp_$x2==ind.second),]
            colnames(temp) <- c("x1","x2","x3","y")

            bar_val[4,1] <- round(min(temp$y, na.rm=TRUE), digits=3)
            lab[4] <- paste0( '(', vec.choice[ (temp[which(temp$y == min(temp$y)),"x1"])] ,", ",
                              vec.choice[ (temp[which(temp$y == min(temp$y)),"x2"])],", ",
                              vec.choice[ (temp[which(temp$y == min(temp$y)),"x3"])], ')' )


            if(input$p3_pt3 != "NA"){
              ind.third <- which(vec.choice == input$p3_pt3)
              temp_ <- temp[which(temp$x3==ind.third),]
              bar_val[3,1] <- round(min(temp_$y, na.rm=TRUE), digits=3)
              lab[3] <- paste0( '(',input$p3_pt1, ", ", input$p3_pt2, ", ", input$p3_pt3, ')' )
            }

            temp_week12 <- data.frame(cbind(t(combn(1:length(vec.choice),2)), result_obj_eval[[2]])); colnames(temp_week12) <- c("x1","x2","y")
            y_ <-temp_week12$y[which((temp_week12$x1 == which(vec.choice==input$p3_pt1)) * (temp_week12$x2 == which(vec.choice==input$p3_pt2))==1)]

            bar_val[2,1] <- round(y_, digits=3)
            lab[2] <- paste0('(', input$p3_pt1,", ", input$p3_pt2,')')




          }
       }

        pbar <- ggplot(data=bar_val, aes(x=y, y=x, fill = x)) + theme_bw() +
          geom_bar(colour="black", stat="identity", position="dodge") +
          guides(fill=FALSE)+    scale_fill_gradientn(limits=range_y, colours = rainbow(9)[7:1]) +

          ylim(c(0, range_y[2]+diff(range_y)/5)) +
          xlab("") +ylab("") +
          theme(legend.text=element_text(size=20), legend.key.size = unit(1, "cm"),
                legend.title = element_blank(),
                axis.title.y = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.title.x = element_text(size=rel(2.5), margin = margin(20,20,20,20)),
                axis.text.y = element_text(size=rel(1.5)),
                axis.text.x = element_text(size=rel(2), angle=90, hjust=1, vjust = 0),
                plot.margin = unit(c(4, 0.1, 0, -2), "cm"),
                panel.margin = unit(0,"cm"),

                plot.title = element_text(size=30, margin = margin(20,20,20,20)))+
          geom_text(aes(label=lab), position=position_dodge(width=0.8), hjust= -0.05, size=7, angle = 90)


        pbar
      })
      # ##########################################################################################
       
      # === render Plots === #
      
      output$p1_plot1 <- renderPlot({print(onePoint1())}, height = 570)
      output$p1_plot2 <- renderPlot({print(onePoint2())}, height = 570)
      
      output$p2_plot1 <- renderPlot({print(twoPoint1())}, height = 570)
      output$p2_plot2 <- renderPlot({print(twoPoint2())}, height = 570)

      output$p3_plot1 <- renderPlot({print(threePoint1())}, height = 570)
      output$p3_plot2 <- renderPlot({print(threePoint2())}, height = 570)
      
    } # end server
  ) # end shinyApp
  
} # end function

# result <- select_p(p_vec=1:3, threshold = 5,
#                    Phi=Phi.hat, lambda=lambda.hat, sigma2=sigma2.hat, B = NULL)
# labels_candidate_pts = 1:nrow(Phi.hat) + 20
# interactive_plot(result, labels_candidate_pts = labels_candidate_pts)
