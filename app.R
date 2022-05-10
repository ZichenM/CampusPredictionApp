library(tidyverse)
library(dplyr)
library(reshape2)
library(lubridate)
library(shiny)
library(shinyjs)
library(RColorBrewer)

'%notin%' = Negate('%in%')

protect.levels = 0:5
vax.rows = c(2,3,5,6)
boost.rows = c(3,6)
prev.infect.rows = 4:6
stud.cols = 1:3
emp.cols = 4:5

affiliation = c("in.state", "far.from.home", "off-campus", "faculty", "employee")

N.clemson = c(4857, 2266, 15629, 1675, 3379)
names(N.clemson) = affiliation
N.tab.clemson = matrix(c(1083, 287, 2577, 161, 537,
                         1399, 617, 4758, 920, 1189,
                         221, 150, 413, 254, 345,
                         1003, 350, 3287, 55, 546,
                         1037, 760, 4311, 233, 634,
                         114, 102, 283, 52, 128), 
                       nrow = length(protect.levels), ncol = length(affiliation), byrow = T)

r.clemson = matrix(c(4, 0, 15, 3, 8,
                     13, 0, 67, 24, 24,
                     4, 2, 6, 4, 5,
                     39, 5, 85, 6, 28,
                     36, 12, 107, 20, 42,
                     5, 5, 7, 5, 12), 
                   nrow = length(protect.levels), ncol = length(affiliation), byrow = T)
r.clemson.tot = apply(r.clemson, 2, sum)

i.clemson = matrix(c(0, 0, 0, 0, 4, 
                     1, 0, 13, 4, 1,
                     1, 0, 2, 1, 1,
                     0, 0, 0, 0, 1,
                     0, 0, 2, 0, 1,
                     0, 0, 0, 0, 0), 
                   nrow = length(protect.levels), ncol = length(affiliation), byrow = T)

q.clemson = matrix(c(0, 0, 0, 0, 4, 0,
                     1, 0, 9, 3, 1, 0,
                     0, 0, 2, 1, 1, 0,
                     0, 0, 0, 0, 1, 0,
                     0, 0, 2, 0, 1, 0,
                     0, 0, 0, 0, 0, 0), 
                   nrow = length(protect.levels), ncol = length(affiliation)+1, byrow = T)

qs.clemson = matrix(0, nrow = length(protect.levels), ncol = length(affiliation)+1, byrow = T)

iso.clemson = qua.clemson = vector(mode = "list", length = length(protect.levels))

for(k in 1:length(protect.levels)){
    iso.clemson[[k]] = qua.clemson[[k]] = matrix(0, nrow = 5, ncol = length(affiliation) + 1)
    iso.clemson[[k]][5,] = q.clemson[k,]
}

protection = matrix(c(0, 0,
                      .244, .243,
                      .487, .486,
                      .830, .578,
                      .830, .578,
                      .830, .578), 
                    nrow = length(protect.levels), ncol = 2)

hr = 1 - protection

weeks.since.vax = matrix(c(0, 0,
                           48, 54,
                           19, 24,
                           0, 0,
                           47, 53,
                           19, 22), nrow = length(protect.levels), ncol = 2)

#############################################################################################################





























#############################################################################################################

##### App starts here

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
    # App title ----
    headerPanel("Campus prediction"),
    # Sidebar panel for inputs ----
    sidebarPanel(
        
        tags$h3("Tutorial"),
        HTML("<p>R code and Tutorial of the application can be found <a href='https://github.com/ZichenMa-USC/CampusPredictionApp'>here</a>.</p>"),

        tags$h3("Input"),
        
        # slider input
        shinyjs::useShinyjs(),
        actionButton("rerun","Run Analysis", icon = icon("fa-solid fa-play")),
        tags$h5(" "),
        selectInput("by_affiliation_out", "Output by affiliation", choices = list("Student/Employee" = 1, "Residential/Non-residential/Faculty/Staff" = 0), selected = 0),
        selectInput("use_clemson_N", "Use Clemson population size", choices = list("Yes" = 1, "No" = 0), selected = 1),
        selectInput("by_affiliation", "Input by affiliation", choices = list("Student/Employee" = 1, "Residential/Non-residential/Faculty/Staff" = 0), selected = 1),
        numericInput("N_stud", "Number of students", value=sum(N.clemson[stud.cols]), min = 1, max = 500000),
        numericInput("N_emp", "Number of employees", value=sum(N.clemson[emp.cols]), min = 1, max = 500000),
        numericInput("N_on", "Number of residential students", value=sum(N.clemson[1:2]), min = 1, max = 500000),
        numericInput("N_off", "Number of non-residential students", value=sum(N.clemson[3]), min = 1, max = 500000),
        numericInput("N_fac", "Number of faculty", value=sum(N.clemson[4]), min = 1, max = 500000),
        numericInput("N_staff", "Number of staff", value=sum(N.clemson[5]), min = 1, max = 500000),
        numericInput("N_comm", "Community population size", 50000, min = 1, max = 500000),
        selectInput("use_vax", "Percent fully vaccinated", choices = list("Use CDC estimates" = 1, "Use Clemson esimates" = 2, "Input your own" = 3), selected = 1),
        numericInput("vax.p.stud", "Student fully vaccinated (%)", value=63.5, min = 0, max = 100),
        numericInput("vax.p.emp", "Employee fully vaccinated (%)", value=76.2, min = 0, max = 100),
        selectInput("use_boost", "Percent boosted among fully vaccinated", choices = list("Use CDC estimates" = 1, "Use Clemson esimates" = 2, "Input your own" = 3), selected = 1),
        numericInput("boost.p.stud", "Student percent boosted among fully vaccinated (%)", value=48.3, min = 0, max = 100),
        numericInput("boost.p.emp", "Employee percent boosted among fully vaccinated (%)", value=48.3, min = 0, max = 100),
        selectInput("use_infect", "Use Clemson currently infected", choices = list("Yes" = 1, "No" = 0), selected = 1),
        selectInput("testing_strat", "Testing strategy", choices = list("Surveillance testing" = 1, "Voluntary testing" = 0), selected = 0),
        numericInput("curr.infect.stud", "Currently infected students", value=sum(i.clemson[,stud.cols]), min = 0, max = 100000),
        numericInput("curr.infect.emp", "Currently infected employees", value=sum(i.clemson[,emp.cols]), min = 0, max = 100000),
        selectInput("use_recovered", "Use Clemson recovered", choices = list("Yes" = 1, "No" = 0), selected = 1),
        numericInput("r.stud", "Percent students recovered within last 90 days (%)", value=round(sum(r.clemson.tot[stud.cols])/sum(N.clemson[stud.cols]), 3)*100, min = 0, max = 100),
        numericInput("r.emp", "Percent employees recovered within last 90 days (%)", value=round(sum(r.clemson.tot[emp.cols])/sum(N.clemson[emp.cols]), 3)*100, min = 0, max = 100),
        selectInput("use_clemson_iq", "Use Clemson isolated/quarantine numbers", choices = list("Yes" = 1, "No" = 0), selected = 1),
        numericInput("curr.iso.stud", "Isolated students", value=sum(q.clemson[,stud.cols]), min = 0, max = 100000),
        numericInput("curr.qua.stud", "Quarantined students", value=sum(qs.clemson[,stud.cols]), min = 0, max = 100000),
        numericInput("curr.iso.emp", "Isolated employees", value=sum(q.clemson[,emp.cols]), min = 0, max = 100000),
        numericInput("curr.qua.emp", "Quarantined employees", value=sum(qs.clemson[,emp.cols]), min = 0, max = 100000),
        sliderInput("p_I_community","Community baseline infection rate",0.01,min=0,max=0.25),
        sliderInput("p_R_baseline_comm","Community baseline recovery rate",0.30,min=0.01,max=0.50),
        sliderInput("p_detected_on","On-campus proportion of infections detected through voluntary testing",value=0.15,min=0.05,max=0.80),
        sliderInput("p_detected_off","Off-campus proportion of infections detected through voluntary testing",value=0.15,min=0.05,max=0.80),
        sliderInput("p_detected_fac","Faculty proportion of infections detected through voluntary testing",value=0.25,min=0.05,max=0.80),
        sliderInput("p_detected_staff","Staff proportion of infections detected through voluntary testing",value=0.25,min=0.05,max=0.80),
        sliderInput("p_detected_comm","Community proportion of infections detected through voluntary testing",value=0.20,min=0.05,max=0.80),
        selectInput("add_recovered", "Additional recovered", choices = list("Yes" = 1, "No" = 0), selected = 1),
        sliderInput("p_recovered_on","Proportion of additional recovered (on-campus)",value=0.40,min=0,max=1),
        sliderInput("p_recovered_off","Proportion of additional recovered (off-campus)",value=0.40,min=0,max=1),
        sliderInput("p_recovered_fac","Proportion of additional recovered (faculty)",value=0.20,min=0,max=1),
        sliderInput("p_recovered_staff","Proportion of additional recovered (staff)",value=0.20,min=0,max=1),
        sliderInput("p_recovered_comm","Proportion of additional recovered (community)",value=0.60,min=0,max=1),
        sliderInput("R_0_on","R0 (on-campus)",value=12,min=0.5,max=15),
        sliderInput("R_0_off","R0 (off-campus)",value=10,min=0.5,max=15),
        sliderInput("R_0_fac","R0 (faculty)",value=5,min=0.5,max=15),
        sliderInput("R_0_staff","R0 (staff)",value=5.5,min=0.5,max=15),
        sliderInput("R_0_comm","R0 (community)",value=6,min=0.5,max=15),
        numericInput("prop_test_on","Residential testing frequency (days per test)",value=100,min=1,max=10000),
        numericInput("prop_test_off","Non-residential testing frequency (days per test)",value=100,min=1,max=10000),
        numericInput("prop_test_fac","Faculty testing frequency (days per test)",value=100,min=1,max=10000),
        numericInput("prop_test_staff","Staff testing frequency (days per test)",value=100,min=1,max=10000),
        sliderInput("lag_start", "Time between arrival of students (baseline) and onset of surveillance testing (days)", value=0, min=0, max=14),
        sliderInput("q_factor","Number of contacts per person",value=0, min = 0, max = 10),
        sliderInput("quarantineS", "Time in quarantine for non-infected individuals (days)",value=5,min=1,max = 14),
        sliderInput("quarantineSbarR", "Time in quarantine for infected individuals (days)", value=7.5, min=7,max=15),
        sliderInput("recovery_time","Days in isolation",value=5,min=3,max=14),
        sliderInput("test_time","Test turnaround time (days)",value=1,min=0.5,max=3),
        sliderInput("sensitivity_test_I","Test sensitivity (infectious)",value=0.97,min=0.50,max=0.99),
        sliderInput("sensitivity_test_E","Test sensitivity (exposed)",value=0.33,min=0.10,max=0.99),
        sliderInput("p_R_baseline_comm","Community baseline recovery rate",0.30,min=0.01,max=0.50),
        sliderInput("exposure_period","Exposure period (days)",value=3,min=2,max=7),
        sliderInput("infectious_period_A","Infectious period for asymptomatic/undetected cases (days)",value=10,min=5,max=14),
        sliderInput("infectious_period_I","Lag between infection and detection/isolation for symptomatic cases (days)",value=2,min=1,max=14),
        sliderInput("time.max","Semester length (weeks)",value = 4,min=2,max=20),
        sliderInput("time.step","Cycle time (hours)",value = 4,min=1,max=12)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        tabsetPanel(
            tabPanel("On-campus Weekly Symptomatic Infections",plotOutput("plot2")),
            tabPanel("On-campus Weekly Infections and Prevalence",plotOutput("plot1")),
            tabPanel("Number in Isolation (students)",plotOutput("plot3")),
            tabPanel("Number in Isolation and Quarantine (students) ",plotOutput("plot4")),
            tabPanel("Number in Isolation (employees)",plotOutput("plot5")),
            tabPanel("Number in Isolation and Quarantine (employees) ",plotOutput("plot6")),
            tabPanel("Baseline summary", tableOutput("summary")),
            #tabPanel("Baseline infection", tableOutput("infect_summary")),
            tabPanel("Estimated protection", tableOutput("protect_summary")),
            #tabPanel("Baseline I/Q summary", tableOutput("iq_summary"))
        ))
)


server = function(input, output)
{
    observeEvent(c(input$use_clemson_N,input$by_affiliation), {
        if(input$use_clemson_N == 1){
            enable("by_affiliation")
            disable("N_stud")
            disable("N_emp")
            disable("N_on")
            disable("N_off")
            disable("N_fac")
            disable("N_staff")
            
            updateNumericInput(inputId = "N_stud", value=sum(N.clemson[stud.cols]), min = 1, max = 500000)
            updateNumericInput(inputId = "N_emp", value=sum(N.clemson[emp.cols]), min = 1, max = 500000)
            updateNumericInput(inputId = "N_on", value=sum(N.clemson[1:2]), min = 1, max = 500000)
            updateNumericInput(inputId = "N_off", value=sum(N.clemson[3]), min = 1, max = 500000)
            updateNumericInput(inputId = "N_fac", value=sum(N.clemson[4]), min = 1, max = 500000)
            updateNumericInput(inputId = "N_staff", value=sum(N.clemson[5]), min = 1, max = 500000)
        } else{
            enable("by_affiliation")
            
            if(input$by_affiliation == 1){
                enable("N_stud")
                enable("N_emp")
                disable("N_on")
                disable("N_off")
                disable("N_fac")
                disable("N_staff")
            } else{
                disable("N_stud")
                disable("N_emp")
                enable("N_on")
                enable("N_off")
                enable("N_fac")
                enable("N_staff")
            }
        }
    })
    
    observeEvent(input$use_vax, {
        if(input$use_vax == 1){
            disable("vax.p.stud")
            disable("vax.p.emp")
            
            updateNumericInput(inputId = "vax.p.stud", 
                               value = 63.5, 
                               min = 0, max = 100)
            updateNumericInput(inputId = "vax.p.emp", 
                               value = 76.2, 
                               min = 0, max = 100)
        } else if(input$use_vax == 2){
            disable("vax.p.stud")
            disable("vax.p.emp")
            
            updateNumericInput(inputId = "vax.p.stud", 
                               value = round(sum(N.tab.clemson[vax.rows, stud.cols])/sum(N.clemson[stud.cols]),3)*100, 
                               min = 0, max = 100)
            updateNumericInput(inputId = "vax.p.emp", 
                               value = round(sum(N.tab.clemson[vax.rows, emp.cols])/sum(N.clemson[emp.cols]),3)*100, 
                               min = 0, max = 100)
            
        } else{
            enable("vax.p.stud")
            enable("vax.p.emp")
        }
    })
    
    observeEvent(input$use_boost, {
        if(input$use_boost == 1){
            disable("boost.p.stud")
            disable("boost.p.emp")
            
            updateNumericInput(inputId = "boost.p.stud", 
                               value = 48.3, 
                               min = 0, max = 100)
            updateNumericInput(inputId = "boost.p.emp", 
                               value = 48.3, 
                               min = 0, max = 100)
        } else if(input$use_boost == 2){
            disable("boost.p.stud")
            disable("boost.p.emp")
            
            updateNumericInput(inputId = "boost.p.stud", 
                               value = round(sum(N.tab.clemson[boost.rows, stud.cols])/sum(N.tab.clemson[vax.rows, stud.cols]),3)*100, 
                               min = 0, max = 100)
            updateNumericInput(inputId = "boost.p.emp", 
                               value = round(sum(N.tab.clemson[boost.rows, emp.cols])/sum(N.tab.clemson[vax.rows, emp.cols]),3)*100, 
                               min = 0, max = 100)
            
        } else{
            enable("boost.p.stud")
            enable("boost.p.emp")
        }
    })
    
    observeEvent(input$use_infect, {
        if(input$use_infect == 1){
            disable("testing_strat")
            disable("curr.infect.stud")
            disable("curr.infect.emp")
            
            updateSelectInput(inputId = "testing_strat", selected = 0)
            updateNumericInput(inputId = "curr.infect.stud", value = sum(i.clemson[,stud.cols]), min = 0, max = 100000)
            updateNumericInput(inputId = "curr.infect.emp", value = sum(i.clemson[,emp.cols]), min = 0, max = 100000)
            
        } else{
            enable("testing_strat")
            enable("curr.infect.stud")
            enable("curr.infect.emp")
        }
    })
    
    observeEvent(input$use_recovered, {
        if(input$use_recovered == 1){
            disable("r.stud")
            disable("r.emp")
            
            updateNumericInput(inputId = "r.stud", value = round(sum(r.clemson.tot[stud.cols])/sum(N.clemson[stud.cols]), 3)*100, min = 0, max = 100)
            updateNumericInput(inputId = "r.emp", value = round(sum(r.clemson.tot[emp.cols])/sum(N.clemson[emp.cols]), 3)*100, min = 0, max = 100)
            
        } else{
            enable("r.stud")
            enable("r.emp")
        }
    })
    
    observeEvent(input$use_clemson_iq, {
        if(input$use_clemson_iq == 1){
            disable("curr.iso.stud")
            disable("curr.qua.stud")
            disable("curr.iso.emp")
            disable("curr.qua.emp")
            
            updateNumericInput(inputId = "curr.iso.stud", value = sum(q.clemson[,stud.cols]), min = 0, max = 100000)
            updateNumericInput(inputId = "curr.iso.emp", value = sum(q.clemson[,emp.cols]), min = 0, max = 100000)
            updateNumericInput(inputId = "curr.qua.stud", value = sum(qs.clemson[,stud.cols]), min = 0, max = 100000)
            updateNumericInput(inputId = "curr.qua.emp", value = sum(qs.clemson[,emp.cols]), min = 0, max = 100000)
            
        } else{
            enable("curr.iso.stud")
            enable("curr.qua.stud")
            enable("curr.iso.emp")
            enable("curr.qua.emp")
        }
    })
    
    cmat.mod = function(mat,n.groups,p.instate){
        mat = mat
        mat2 = matrix(0,n.groups,n.groups)
        
        for(i in 1:nrow(mat2)){
            if(i==1){
                mat2[i,] = c(mat[i,1]*p.instate,mat[i,1]*(1-p.instate),mat[i,2:5])
            } else{
                mat2[i,] = c(mat[i-1,1]*p.instate,mat[i-1,1]*(1-p.instate),mat[i-1,2:5])
            }
        }
        return(round(mat2,2))
    }
    
    savePlots = function(){
        
        hr = hr[,c(1,1,1,2,2,2)]
        
        by.affiliation = input$by_affiliation
        use.clemson.n = input$use_clemson_N
        
        p.recovered.comm = input$p_R_baseline_comm
        p.infect.comm = input$p_I_community
        
        vax.p.stud = input$vax.p.stud/100
        vax.p.emp = input$vax.p.emp/100
        
        boost.p.stud = input$boost.p.stud/100
        boost.p.emp = input$boost.p.emp/100
        
        r.stud = input$r.stud/100
        r.emp = input$r.emp/100
        
        curr.iso.stud = input$curr.iso.stud
        curr.qua.stud = input$curr.qua.stud
        curr.iso.emp = input$curr.iso.emp
        curr.qua.emp = input$curr.qua.emp
        
        recovery.time = input$recovery_time
        quarantineS = input$quarantineS
        quarantineSbarR = input$quarantineSbarR
        
        infect.period.a = input$infectious_period_A
        infect.period.i = input$infectious_period_I
        exposure.time = input$exposure_period
        
        p.detected = c(input$p_detected_on,
                       input$p_detected_on,
                       input$p_detected_off,
                       input$p_detected_fac,
                       input$p_detected_staff,
                       input$p_detected_comm)
        
        if(use.clemson.n == 0){
            if(by.affiliation == 1){
                
                N.stud = input$N_stud
                N.emp = input$N_emp
                N.comm = input$N_comm
                
                N.stud.tab.vax = round(N.stud*vax.p.stud*N.tab.clemson[vax.rows,stud.cols]/sum(N.tab.clemson[vax.rows,stud.cols]))
                N.stud.tab.unvax = round(N.stud*(1-vax.p.stud)*N.tab.clemson[-vax.rows,stud.cols]/sum(N.tab.clemson[-vax.rows,stud.cols]))
                
                N.stud.tab = matrix(0,nrow=length(protect.levels),ncol = length(stud.cols))
                N.stud.tab[vax.rows,] = N.stud.tab.vax
                N.stud.tab[-vax.rows,] = N.stud.tab.unvax
                
                N.emp.tab.vax = round(N.emp*vax.p.emp*N.tab.clemson[vax.rows,emp.cols]/sum(N.tab.clemson[vax.rows,emp.cols]))
                N.emp.tab.unvax = round(N.emp*(1-vax.p.emp)*N.tab.clemson[-vax.rows,emp.cols]/sum(N.tab.clemson[-vax.rows,emp.cols]))
                
                N.emp.tab = matrix(0,nrow=length(protect.levels),ncol = length(emp.cols))
                N.emp.tab[vax.rows,] = N.emp.tab.vax
                N.emp.tab[-vax.rows,] = N.emp.tab.unvax
                
                N.tab = cbind(N.stud.tab, N.emp.tab)
                N.tab = cbind(N.tab, round(apply(N.tab,1,sum)/sum(N.tab)*N.comm))
                colnames(N.tab) = c(affiliation, "community")
                
                N.tab = rbind(N.tab[1,],
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))),
                              N.tab[4,],
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))))
                
                N = apply(N.tab, 2, sum)
                n.groups = length(N)
                
            } else{
                
                N.on = input$N_on
                N.off = input$N_off
                N.fac = input$N_fac
                N.staff = input$N_staff
                N.comm = input$N_comm
                
                N.stud = N.on + N.off
                N.emp = N.fac + N.staff
                
                N.stud.tab.vax = round(N.stud*vax.p.stud*N.tab.clemson[vax.rows,stud.cols]/sum(N.tab.clemson[vax.rows,stud.cols]))
                N.stud.tab.unvax = round(N.stud*(1-vax.p.stud)*N.tab.clemson[-vax.rows,stud.cols]/sum(N.tab.clemson[-vax.rows,stud.cols]))
                
                N.stud.tab = matrix(0,nrow=length(protect.levels),ncol = length(stud.cols))
                N.stud.tab[vax.rows,] = N.stud.tab.vax
                N.stud.tab[-vax.rows,] = N.stud.tab.unvax
                
                N.emp.tab.vax = round(N.emp*vax.p.emp*N.tab.clemson[vax.rows,emp.cols]/sum(N.tab.clemson[vax.rows,emp.cols]))
                N.emp.tab.unvax = round(N.emp*(1-vax.p.emp)*N.tab.clemson[-vax.rows,emp.cols]/sum(N.tab.clemson[-vax.rows,emp.cols]))
                
                N.emp.tab = matrix(0,nrow=length(protect.levels),ncol = length(emp.cols))
                N.emp.tab[vax.rows,] = N.emp.tab.vax
                N.emp.tab[-vax.rows,] = N.emp.tab.unvax
                
                N.tab = cbind(N.stud.tab, N.emp.tab)
                N.tab = cbind(N.tab, round(apply(N.tab,1,sum)/sum(N.tab)*N.comm))
                colnames(N.tab) = c(affiliation, "community")
                
                N.tab = rbind(N.tab[1,],
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))),
                              N.tab[4,],
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))))
                
                N = apply(N.tab, 2, sum)
                n.groups = length(N)
                
            }
        } else{
            
            if(by.affiliation == 1){
                
                N.stud = input$N_stud
                N.emp = input$N_emp
                N.comm = input$N_comm
                
                N.stud.tab.vax = round(N.stud*vax.p.stud*N.tab.clemson[vax.rows,stud.cols]/sum(N.tab.clemson[vax.rows,stud.cols]))
                N.stud.tab.unvax = round(N.stud*(1-vax.p.stud)*N.tab.clemson[-vax.rows,stud.cols]/sum(N.tab.clemson[-vax.rows,stud.cols]))
                
                N.stud.tab = matrix(0,nrow=length(protect.levels),ncol = length(stud.cols))
                N.stud.tab[vax.rows,] = N.stud.tab.vax
                N.stud.tab[-vax.rows,] = N.stud.tab.unvax
                
                N.emp.tab.vax = round(N.emp*vax.p.emp*N.tab.clemson[vax.rows,emp.cols]/sum(N.tab.clemson[vax.rows,emp.cols]))
                N.emp.tab.unvax = round(N.emp*(1-vax.p.emp)*N.tab.clemson[-vax.rows,emp.cols]/sum(N.tab.clemson[-vax.rows,emp.cols]))
                
                N.emp.tab = matrix(0,nrow=length(protect.levels),ncol = length(emp.cols))
                N.emp.tab[vax.rows,] = N.emp.tab.vax
                N.emp.tab[-vax.rows,] = N.emp.tab.unvax
                
                N.tab = cbind(N.stud.tab, N.emp.tab)
                N.tab = cbind(N.tab, round(apply(N.tab,1,sum)/sum(N.tab)*N.comm))
                colnames(N.tab) = c(affiliation, "community")
                
                N.tab = rbind(N.tab[1,],
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))),
                              N.tab[4,],
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))))
                
                N = apply(N.tab, 2, sum)
                n.groups = length(N)
                
            } else{
                
                N.on = input$N_on
                N.off = input$N_off
                N.fac = input$N_fac
                N.staff = input$N_staff
                N.comm = input$N_comm
                
                N.stud = N.on + N.off
                N.emp = N.fac + N.staff
                
                N.stud.tab.vax = round(N.stud*vax.p.stud*N.tab.clemson[vax.rows,stud.cols]/sum(N.tab.clemson[vax.rows,stud.cols]))
                N.stud.tab.unvax = round(N.stud*(1-vax.p.stud)*N.tab.clemson[-vax.rows,stud.cols]/sum(N.tab.clemson[-vax.rows,stud.cols]))
                
                N.stud.tab = matrix(0,nrow=length(protect.levels),ncol = length(stud.cols))
                N.stud.tab[vax.rows,] = N.stud.tab.vax
                N.stud.tab[-vax.rows,] = N.stud.tab.unvax
                
                N.emp.tab.vax = round(N.emp*vax.p.emp*N.tab.clemson[vax.rows,emp.cols]/sum(N.tab.clemson[vax.rows,emp.cols]))
                N.emp.tab.unvax = round(N.emp*(1-vax.p.emp)*N.tab.clemson[-vax.rows,emp.cols]/sum(N.tab.clemson[-vax.rows,emp.cols]))
                
                N.emp.tab = matrix(0,nrow=length(protect.levels),ncol = length(emp.cols))
                N.emp.tab[vax.rows,] = N.emp.tab.vax
                N.emp.tab[-vax.rows,] = N.emp.tab.unvax
                
                N.tab = cbind(N.stud.tab, N.emp.tab)
                N.tab = cbind(N.tab, round(apply(N.tab,1,sum)/sum(N.tab)*N.comm))
                colnames(N.tab) = c(affiliation, "community")
                
                N.tab = rbind(N.tab[1,],
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[2,] + N.tab[3,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))),
                              N.tab[4,],
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(1-boost.p.stud,3), rep(1-boost.p.emp,3)))),
                              round(t(N.tab[5,] + N.tab[6,]) %*% diag(c(rep(boost.p.stud,3), rep(boost.p.emp,3)))))
                
                N = apply(N.tab, 2, sum)
                n.groups = length(N)
                
            }
            
        }
        
        if(input$use_infect == 1){
            curr.infect.stud = input$curr.infect.stud*sum(N[stud.cols])/sum(N.clemson[stud.cols])
            curr.infect.emp = input$curr.infect.emp*sum(N[emp.cols])/sum(N.clemson[emp.cols])
        } else{
            curr.infect.stud = input$curr.infect.stud
            curr.infect.emp = input$curr.infect.emp
        }
        
        # Recovered:
        
        R.init.stud = round(r.stud*sum(N[stud.cols])*r.clemson[,stud.cols]/sum(r.clemson[,stud.cols]))
        R.init.emp = round(r.emp*sum(N[emp.cols])*r.clemson[,emp.cols]/sum(r.clemson[,emp.cols]))
        R.init = round(cbind(R.init.stud, R.init.emp, round(N.tab[,n.groups]*p.recovered.comm)))
        R.init = ifelse(is.na(R.init), 0, R.init)
        
        # Isolation/Quarantine
        
        use_clemson_iq = input$use_clemson_iq
        
        Q.init = Qs.init = QsbarR.init = matrix(0, nrow = length(protect.levels), ncol = n.groups)
        qSbarR = qS = isolation = vector(mode = "list", length = length(protect.levels))
        
        if(use_clemson_iq == 1){
            for(k in 1:length(protect.levels)){
                qSbarR[[k]] = matrix(0, nrow = ceiling(quarantineSbarR), ncol = n.groups)
                QsbarR.init[k,] = apply(qSbarR[[k]], 2, sum)
                
                Q.init[k,] = q.clemson[k,]
                isolation[[k]] = iso.clemson[[k]]
                
                Qs.init[k,] = qs.clemson[k,]
                qS[[k]] = qua.clemson[[k]]
            }
        } else{
            
            Q.init[,stud.cols] = round(curr.iso.stud*q.clemson[,stud.cols]/sum(q.clemson[,stud.cols]))
            Q.init[,emp.cols] = round(curr.iso.emp*q.clemson[,emp.cols]/sum(q.clemson[,emp.cols]))
            Q.init = ifelse(is.na(Q.init), 0, Q.init)
            
            Qs.init[,stud.cols] = round(curr.qua.stud*qs.clemson[,stud.cols]/sum(qs.clemson[,stud.cols]))
            Qs.init[,emp.cols] = round(curr.qua.emp*qs.clemson[,emp.cols]/sum(qs.clemson[,emp.cols]))
            Qs.init = ifelse(is.na(Qs.init), 0, Qs.init)
            
            for(k in 1:length(protect.levels)){
                
                qSbarR[[k]] = matrix(0, nrow = ceiling(quarantineSbarR), ncol = n.groups)
                QsbarR.init[k,] = apply(qSbarR[[k]], 2, sum)
                
                isolation[[k]] = matrix(0, nrow = recovery.time, ncol = n.groups)
                isolation[[k]][1,] = Q.init[k,]
                
                qS[[k]] = matrix(0, nrow = quarantineS, ncol = n.groups)
                qS[[k]][1,] = Qs.init[k,]
            }
        }
        
        # Currently infected
        
        testing.strat = input$testing_strat
        
        if(testing.strat == 0){
            
            # Voluntary testing
            I.init = matrix(0, nrow = length(protect.levels), ncol = n.groups)
            
            I.init[,stud.cols] = curr.infect.stud*i.clemson[,stud.cols]/sum(i.clemson[,stud.cols])
            I.init[,emp.cols] = curr.infect.emp*i.clemson[,emp.cols]/sum(i.clemson[,emp.cols])
            I.init = ifelse(is.na(I.init), 0, I.init)
            
            E.init = round(t(apply(I.init,1,function(x){x*exposure.time/infect.period.i})))
            A.init = round(t(apply(I.init,1,function(x){x*infect.period.a/infect.period.i})))
            
            E.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          exposure.time/(exposure.time+infect.period.a+infect.period.i))
            
            A.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          infect.period.a/(exposure.time+infect.period.a+infect.period.i))
            
            I.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          infect.period.i/(exposure.time+infect.period.a+infect.period.i))
            
        } else{
            # Surveillance testing
            E.init = A.init = I.init = matrix(0, nrow = length(protect.levels), ncol = n.groups)
            
            I.init[,stud.cols] = round(curr.infect.stud*N.tab[,stud.cols]/sum(N.tab[,stud.cols])*
                                           infect.period.i/(exposure.time+infect.period.a+infect.period.i))
            I.init[,emp.cols] = round(curr.infect.emp*N.tab[,emp.cols]/sum(N.tab[,emp.cols])*
                                          infect.period.i/(exposure.time+infect.period.a+infect.period.i))
            
            A.init[,stud.cols] = round(curr.infect.stud*N.tab[,stud.cols]/sum(N.tab[,stud.cols])*
                                           infect.period.a/(exposure.time+infect.period.a+infect.period.i))
            A.init[,emp.cols] = round(curr.infect.emp*N.tab[,emp.cols]/sum(N.tab[,emp.cols])*
                                          infect.period.a/(exposure.time+infect.period.a+infect.period.i))
            
            E.init[,stud.cols] = round(curr.infect.stud*N.tab[,stud.cols]/sum(N.tab[,stud.cols])*
                                           exposure.time/(exposure.time+infect.period.a+infect.period.i))
            E.init[,emp.cols] = round(curr.infect.emp*N.tab[,emp.cols]/sum(N.tab[,emp.cols])*
                                          exposure.time/(exposure.time+infect.period.a+infect.period.i))
            
            E.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          exposure.time/(exposure.time+infect.period.a+infect.period.i))
            A.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          infect.period.a/(exposure.time+infect.period.a+infect.period.i))
            I.init[,n.groups] = round(N.tab[,n.groups]*p.infect.comm*
                                          infect.period.i/(exposure.time+infect.period.a+infect.period.i))
        }
        
        # Susceptible
        
        S.init = N.tab - E.init - A.init - I.init - Q.init - Qs.init - R.init
        
        T.init = Te.init = matrix(0, nrow = length(protect.levels), ncol = n.groups)
        
        #### Adjust for additional recovered
        
        add_recovered = input$add_recovered
        
        if(add_recovered == 1){
            p.recovered = c(input$p_recovered_on,
                            input$p_recovered_on,
                            input$p_recovered_off,
                            input$p_recovered_fac,
                            input$p_recovered_staff,
                            input$p_recovered_comm*(1-p.recovered.comm))
            n.recovered = N*p.recovered
            susceptible = apply(S.init,2,sum)
            
            for(i in 1:n.groups){
                if(n.recovered[i] < susceptible[i]){
                    n.recovered.i = round(S.init[,i]/susceptible[i]*n.recovered[i])
                    replace.id = which(n.recovered.i < S.init[,i])
                    S.init[replace.id,i] = S.init[replace.id,i] - n.recovered.i[replace.id]
                    R.init[replace.id,i] = R.init[replace.id,i] + n.recovered.i[replace.id]
                }
            }
        }
        
        #############################################################################################################
        
        ##### I/Q summary
        
        # iq.baseline = (Reduce("+", isolation) + Reduce("+", qS))[,-n.groups]
        # iq.baseline = rbind(iq.baseline, apply(iq.baseline,2,sum))
        # rownames(iq.baseline) = c(paste0("Day ",1:recovery.time), "Total")
        # colnames(iq.baseline) = c("In state", "Out of state", "Off-campus","Faculty","Staff")
        # iq.baseline = as.data.frame(t(iq.baseline))
        # 
        # iq.baseline = iq.baseline %>% mutate(Affiliation = c("In state", "Out of state", "Off-campus","Faculty","Staff"))
        # iq.baseline = iq.baseline %>% select(Affiliation, everything())
        
        ##### Baseline infection
        
        # p.sym = matrix(NA, nrow = nrow(p.sympto), ncol = ncol(p.sympto))
        # p.sym[,1] = paste0(n.pos[,1],"/",n.pos[,2]," (",format(round(p.sympto[,1], 3)*100,nsmall=1),"%)")
        # p.sym[,2] = paste0(n.pos[,3],"/",n.pos[,4]," (",format(round(p.sympto[,2], 3)*100,nsmall=1),"%)")
        # 
        # infection.rate = matrix(paste0(format(round(infection.rate, 3)*100,nsmall=1), "%"), length(protect.levels), 2)
        # 
        # infect.tab = matrix(NA, nrow = 2*(length(protect.levels)+1), ncol = 3)
        # infect.tab[1,2:3] = c(" ", " ")
        # infect.tab[2:(2-1+length(protect.levels)), 2:3] = p.sym
        # infect.tab[(2+length(protect.levels)),2:3] = c(" ", " ")
        # infect.tab[(2+length(protect.levels)+1):(2+2*length(protect.levels)), 2:3] = infection.rate
        # 
        # infect.tab[,1] = c("Positive tests", "...Unprotected", "...Fully vaccinated w/o prev. infection", "...Boosted w/o prev. infection", 
        #                    "...Previous infection only", "...Fully vaccinated w/ prev. infection", "...Boosted w/ prev. infection",
        #                    "Infection rate", "...Unprotected", "...Fully vaccinated w/o prev. infection", "...Boosted w/o prev. infection", 
        #                    "...Previous infection only", "...Fully vaccinated w/ prev. infection", "...Boosted w/ prev. infection")
        # colnames(infect.tab) = c("Infection", "Students", "Employees")
        
        ##### Summary statistics
        
        out.type = input$by_affiliation_out
        
        if(out.type == 0){
            N.2 = c(N[1]+N[2],N[-c(1,2)])
            R.tot = apply(R.init,2,sum)
            R.tot = c(R.tot[1]+R.tot[2],R.tot[-c(1,2)])
            N.tab.2 = cbind(N.tab[,1]+N.tab[,2], N.tab[,-c(1,2)])
            
            sum.stats.tab = rbind(N.2, # total
                                  R.tot, # recovered
                                  N.tab.2,
                                  apply(N.tab.2[vax.rows,],2,sum), # total vaccinated
                                  format(round(apply(N.tab.2[vax.rows,],2,sum)/N.2,3)*100,nsmall=1)) 
            
            colnames(sum.stats.tab) = c("Residential", "Non-residential","Faculty","Staff","Community")
        } else{
            N.2 = c(sum(N[stud.cols]), sum(N[emp.cols]), N[n.groups])
            R.tot = apply(R.init,2,sum)
            R.tot = c(sum(R.tot[stud.cols]), sum(R.tot[emp.cols]), R.tot[n.groups])
            N.tab.2 = cbind(apply(N.tab[,stud.cols],1,sum), apply(N.tab[,emp.cols],1,sum), N.tab[,n.groups])
            
            sum.stats.tab = rbind(N.2, # total
                                  R.tot, # recovered
                                  N.tab.2,
                                  apply(N.tab.2[vax.rows,],2,sum), # total vaccinated
                                  format(round(apply(N.tab.2[vax.rows,],2,sum)/N.2,3)*100,nsmall=1))
            colnames(sum.stats.tab) = c("Students","Employees","Community")
        }
        
        rownames(sum.stats.tab) = c("Total population", "Recovered", "Unprotected", "Fully vaccinated w/o prev. infection", "Boosted w/o prev. infection", 
                                    "Previous infection only", "Fully vaccinated w/ prev. infection", "Boosted w/ prev. infection", "Total vaccinated", "Percent vaccinated")
        sum.stats.tab = as.data.frame(sum.stats.tab)
        sum.stats.tab["Percent vaccinated",] = paste0(sum.stats.tab["Percent vaccinated",],"%")
        
        sum.stats.tab = sum.stats.tab %>% mutate(Status = c("Total population", "Recovered", "Unprotected", "Fully vaccinated w/o prev. infection", "Boosted w/o prev. infection", 
                                                            "Previous infection only", "Fully vaccinated w/ prev. infection", "Boosted w/ prev. infection", "Total vaccinated", "Percent vaccinated"))
        sum.stats.tab = sum.stats.tab %>% select(Status, everything())
        
        ####### Protection
        
        protect = matrix(paste0(format(round(protection, 3)*100,nsmall=1), "%"), nrow=length(protect.levels))
        protect[1,] = "Reference"
        colnames(protect) = c("Students", "Employee/Community")
        
        protect = rbind(protect, c(" ", " "))
        protect = rbind(protect, c(" ", " "))
        protect = rbind(protect, weeks.since.vax[vax.rows,])
        
        protect = as.data.frame(protect)
        protect = protect %>% mutate(Status = c("Unprotected", "Fully vaccinated w/o prev. infection", "Boosted w/o prev. infection", 
                                                "Previous infection only", "Fully vaccinated w/ prev. infection", "Boosted w/ prev. infection", " ",
                                                "Average weeks since vaccination", "...Fully vaccinated w/o prev. infection", "...Boosted w/o prev. infection", 
                                                "...Fully vaccinated w/ prev. infection", "...Boosted w/ prev. infection"))
        protect = protect %>% select(Status, everything())
        
        #############################################################################################################
        ########### Projection for on/off-campus, faculty and staff
        
        inits = rbind(t(S.init), t(E.init), t(A.init), t(I.init), t(T.init), t(Te.init), t(Q.init), t(Qs.init), t(QsbarR.init),
                      t(R.init))
        mat = matrix(c(0,c(inits)), nrow = 1)
        
        ##### Parameters 
        
        n.weeks = input$time.max
        time.max=7*n.weeks # 15-week prediction (in days)
        time.step = input$time.step/24 # increment = 1/3 days
        
        sigma = sigma2 = 1/input$exposure_period
        
        # infectious.period.A = 10
        phi = phi2 = 1/input$infectious_period_A
        
        #infectious.period.I = 4
        gamma = gamma2 = 1/input$infectious_period_I
        
        # infectious.period.T = 1
        kappa = 1/input$test_time
        
        #recovery.time = input$recovery_time/input$I.reduce
        rho = rho2 = 1/input$recovery_time
        
        p.detected.in = input$p_detected_on                 ###proportion of people would move to I from E
        p.detected.out = input$p_detected_on
        p.detected.off = input$p_detected_off
        p.detected.fac = input$p_detected_fac
        p.detected.staff = input$p_detected_staff
        p.detected.comm = input$p_detected_comm
        p.detected = c(p.detected.in,p.detected.out,p.detected.off,p.detected.fac,p.detected.staff,p.detected.comm)
        
        alpha = p.detected # input$p_symptomatic
        
        # red.coef2 = (2-alpha)/input$red_coef_pinf
        # red.coef3 = (2-alpha)/input$red_coef_mrna
        # red.coef4 = (2-alpha)/input$red_coef_jj
        
        # sensitivity_test_I = 0.95
        se = input$sensitivity_test_I
        
        # sensitivity_test_E = 0.33
        seE = input$sensitivity_test_E
        
        q_factor = input$q_factor
        #quarantineS = 7 # input$quarantineS
        #quarantineSbarR = 10 # input$quarantineSbarR
        
        #mrna.eff = input$mrna_eff
        #jj.eff = input$jj_eff
        #mrna.prop = 0.001*.75 # proportion in S getting vaccinated per day
        #jj.prop = 0.001*.25
        
        
        # Contact matrix (by row): on-campus, faculty, staff, off-campus, community
        # COME BACK TO THIS this is the contact matrix for every time step besides weekends
        #contact.mat = matrix(c(0.850, 0.025, 0.025, 0.075, 0.025,
        #                       0.175, 0.300, 0.150, 0.175, 0.200,
        #                       0.075, 0.150, 0.500, 0.075, 0.200,
        #                       0.075, 0.025, 0.025, 0.825, 0.005,
        #                       0.050, 0.050, 0.050, 0.050, 0.800),n.groups,n.groups,byrow=T)
        
        R0.on = input$R_0_on
        R0.fac = input$R_0_fac
        R0.staff = input$R_0_staff
        R0.off = input$R_0_off
        R0.comm = input$R_0_comm
        
        R0 = c(R0.on,R0.on,R0.off,R0.fac,R0.staff,R0.comm)
        
        # beta1 = R0*1/10    # force of infection for S
        # beta2 = beta1/input$red_coef_pinf # force of infection for previously infected
        # beta3 = beta1/input$red_coef_mrna # force of infection for mRNA vaccine
        # beta4 = beta1/input$red_coef_jj # red.coef3 # force of infection for J&J vaccine
        
        lag_start = input$lag_start
        
        prop.test.on = 1/input$prop_test_on
        prop.test.fac = 1/input$prop_test_fac
        prop.test.staff = 1/input$prop_test_staff
        prop.test.off = 1/input$prop_test_off
        prop.test.comm = 0.01
        
        prop.test = c(prop.test.on,
                      prop.test.on,
                      prop.test.off,
                      prop.test.fac,
                      prop.test.staff,
                      prop.test.comm)
        
        prop.test = ifelse(prop.test == Inf, 10000, prop.test)
        
        tot.test = prop.test*N
        # tot.test.off = prop.test.off*N[4:5]
        
        #######################################################################
        ####### Initial states - stored in "mat"
        
        time=seq(0,time.max,by=time.step)
        
        #mat = start # startMatrix(input$preArrival,input$arrival)
        
        positiveTestsCount = matrix(c(1,rep(0,n.groups)), nrow = 1)
        symptomaticCount = matrix(c(0,rep(0,n.groups)), nrow = 1)
        tests.count = matrix(0,nrow = 0, ncol = n.groups)
        
        ####### Add to community
        
        p.instate = N[1]/(N[1]+N[2])
        
        #hr = cbind(hr, apply(hr,1,mean))
        # 
        # classroom = input$classroom
        # building = input$building
        # weekend_event = input$weekend_event
        
        ####### getting the correct indices
        
        # Unprotected
        
        Sid      = 2:(n.groups+1)
        Eid      = Sid + n.groups*1
        Aid      = Sid + n.groups*2
        Iid      = Sid + n.groups*3
        Tid      = Sid + n.groups*4
        Teid     = Sid + n.groups*5
        Qid      = Sid + n.groups*6
        Qsid     = Sid + n.groups*7
        QsbarRid = Sid + n.groups*8
        Rid      = Sid + n.groups*9
        
        # fully vaccinated, no previous infection
        
        vSid      = Sid + n.groups*10
        vEid      = Sid + n.groups*11
        vAid      = Sid + n.groups*12
        vIid      = Sid + n.groups*13
        vTid      = Sid + n.groups*14
        vTeid     = Sid + n.groups*15
        vQid      = Sid + n.groups*16
        vQsid     = Sid + n.groups*17
        vQsbarRid = Sid + n.groups*18
        vRid      = Sid + n.groups*19
        
        # boosted, no previous infection
        
        bSid      = Sid + n.groups*20
        bEid      = Sid + n.groups*21
        bAid      = Sid + n.groups*22
        bIid      = Sid + n.groups*23
        bTid      = Sid + n.groups*24
        bTeid     = Sid + n.groups*25
        bQid      = Sid + n.groups*26
        bQsid     = Sid + n.groups*27
        bQsbarRid = Sid + n.groups*28
        bRid      = Sid + n.groups*29
        
        # previously infected only
        
        pSid      = Sid + n.groups*30
        pEid      = Sid + n.groups*31
        pAid      = Sid + n.groups*32
        pIid      = Sid + n.groups*33
        pTid      = Sid + n.groups*34
        pTeid     = Sid + n.groups*35
        pQid      = Sid + n.groups*36
        pQsid     = Sid + n.groups*37
        pQsbarRid = Sid + n.groups*38
        pRid      = Sid + n.groups*39
        
        # fully vaccinated, previously infected
        
        pvSid      = Sid + n.groups*40
        pvEid      = Sid + n.groups*41
        pvAid      = Sid + n.groups*42
        pvIid      = Sid + n.groups*43
        pvTid      = Sid + n.groups*44
        pvTeid     = Sid + n.groups*45
        pvQid      = Sid + n.groups*46
        pvQsid     = Sid + n.groups*47
        pvQsbarRid = Sid + n.groups*48
        pvRid      = Sid + n.groups*49
        
        # boosted, previously infected
        
        pbSid      = Sid + n.groups*50
        pbEid      = Sid + n.groups*51
        pbAid      = Sid + n.groups*52
        pbIid      = Sid + n.groups*53
        pbTid      = Sid + n.groups*54
        pbTeid     = Sid + n.groups*55
        pbQid      = Sid + n.groups*56
        pbQsid     = Sid + n.groups*57
        pbQsbarRid = Sid + n.groups*58
        pbRid      = Sid + n.groups*59
        
        ##### Forward Euler starts here
        
        for(i in 2:length(time)){
            
            #################################################
            ###### Recording results of last step	  
            oldS      = mat[i-1,Sid]
            oldE      = mat[i-1,Eid]
            oldA      = mat[i-1,Aid]
            oldI      = mat[i-1,Iid]
            oldT      = mat[i-1,Tid]
            oldTe     = mat[i-1,Teid]
            oldQ      = mat[i-1,Qid]
            oldQs     = mat[i-1,Qsid]
            oldQsbarR = mat[i-1,QsbarRid]
            oldR      = mat[i-1,Rid]
            
            oldvS      = mat[i-1,vSid]
            oldvE      = mat[i-1,vEid]
            oldvA      = mat[i-1,vAid]
            oldvI      = mat[i-1,vIid]
            oldvT      = mat[i-1,vTid]
            oldvTe     = mat[i-1,vTeid]
            oldvQ      = mat[i-1,vQid]
            oldvQs     = mat[i-1,vQsid]
            oldvQsbarR = mat[i-1,vQsbarRid]
            oldvR      = mat[i-1,vRid]
            
            oldbS      = mat[i-1,bSid]
            oldbE      = mat[i-1,bEid]
            oldbA      = mat[i-1,bAid]
            oldbI      = mat[i-1,bIid]
            oldbT      = mat[i-1,bTid]
            oldbTe     = mat[i-1,bTeid]
            oldbQ      = mat[i-1,bQid]
            oldbQs     = mat[i-1,bQsid]
            oldbQsbarR = mat[i-1,bQsbarRid]
            oldbR      = mat[i-1,bRid]
            
            oldpS      = mat[i-1,pSid]
            oldpE      = mat[i-1,pEid]
            oldpA      = mat[i-1,pAid]
            oldpI      = mat[i-1,pIid]
            oldpT      = mat[i-1,pTid]
            oldpTe     = mat[i-1,pTeid]
            oldpQ      = mat[i-1,pQid]
            oldpQs     = mat[i-1,pQsid]
            oldpQsbarR = mat[i-1,pQsbarRid]
            oldpR      = mat[i-1,pRid]
            
            oldpvS      = mat[i-1,pvSid]
            oldpvE      = mat[i-1,pvEid]
            oldpvA      = mat[i-1,pvAid]
            oldpvI      = mat[i-1,pvIid]
            oldpvT      = mat[i-1,pvTid]
            oldpvTe     = mat[i-1,pvTeid]
            oldpvQ      = mat[i-1,pvQid]
            oldpvQs     = mat[i-1,pvQsid]
            oldpvQsbarR = mat[i-1,pvQsbarRid]
            oldpvR      = mat[i-1,pvRid]
            
            oldpbS      = mat[i-1,pbSid]
            oldpbE      = mat[i-1,pbEid]
            oldpbA      = mat[i-1,pbAid]
            oldpbI      = mat[i-1,pbIid]
            oldpbT      = mat[i-1,pbTid]
            oldpbTe     = mat[i-1,pbTeid]
            oldpbQ      = mat[i-1,pbQid]
            oldpbQs     = mat[i-1,pbQsid]
            oldpbQsbarR = mat[i-1,pbQsbarRid]
            oldpbR      = mat[i-1,pbRid]
            
            #################################################
            ######Update random testing and tests count
            if(time[i]%%1 == 0 & time[i]>=lag_start){
                All = rowSums(cbind(newS, newE, newA, newI,
                                    newvS, newvE, newvA, newvI,
                                    newbS, newbE, newbA, newbI,
                                    newpS, newpE, newpA, newpI,
                                    newpvS, newpvE, newpvA, newpvI,
                                    newpbS, newpbE, newpbA, newpbI))
                
                p.test = tot.test/All # c(tot.test.on/All[1:3],tot.test.off/All[4:5])
                tests.count = rbind(tests.count, round(p.test * All, 0))
                
                ATests = oldA*p.test*se
                oldA = oldA - ATests
                oldT = oldT + ATests
                ETests = oldE*p.test*seE
                oldE = oldE - ETests
                oldTe = oldTe + ETests 
                ITests = oldI*p.test*se
                oldI = oldI - ITests
                oldT = oldT + ITests
                
                vATests = oldvA*p.test*se
                oldvA = oldvA - vATests
                oldvT = oldvT + vATests
                vETests = oldvE*p.test*seE
                oldvE = oldvE - vETests
                oldvTe = oldvTe + vETests 
                vITests = oldvI*p.test*se
                oldvI = oldvI - vITests
                oldvT = oldvT + vITests
                
                bATests = oldbA*p.test*se
                oldbA = oldbA - bATests
                oldbT = oldbT + bATests
                bETests = oldbE*p.test*seE
                oldbE = oldbE - bETests
                oldbTe = oldbTe + bETests 
                bITests = oldbI*p.test*se
                oldbI = oldbI - bITests
                oldbT = oldbT + bITests
                
                pATests = oldpA*p.test*se
                oldpA = oldpA - pATests
                oldpT = oldpT + pATests
                pETests = oldpE*p.test*seE
                oldpE = oldpE - pETests
                oldpTe = oldpTe + pETests 
                pITests = oldpI*p.test*se
                oldpI = oldpI - pITests
                oldpT = oldpT + pITests
                
                pvATests = oldpvA*p.test*se
                oldpvA = oldpvA - pvATests
                oldpvT = oldpvT + pvATests
                pvETests = oldpvE*p.test*seE
                oldpvE = oldpvE - pvETests
                oldpvTe = oldpvTe + pvETests 
                pvITests = oldpvI*p.test*se
                oldpvI = oldpvI - pvITests
                oldpvT = oldpvT + pvITests
                
                pbATests = oldpbA*p.test*se
                oldpbA = oldpbA - pbATests
                oldpbT = oldpbT + pbATests
                pbETests = oldpbE*p.test*seE
                oldpbE = oldpbE - pbETests
                oldpbTe = oldpbTe + pbETests 
                pbITests = oldpbI*p.test*se
                oldpbI = oldpbI - pbITests
                oldpbT = oldpbT + pbITests
                
                positiveTestsCount = rbind(positiveTestsCount, c(time[i], (ATests+ETests+ITests+
                                                                               vATests+vETests+vITests+
                                                                               bATests+bETests+bITests+
                                                                               pATests+pETests+pITests+
                                                                               pvATests+pvETests+pvITests+
                                                                               pbATests+pbETests+pbITests)))
                
                symptomaticCount = rbind(symptomaticCount, c(time[i], (ITests + vITests + bITests +
                                                                           pITests+pvITests+pbITests)))
                
                #######moving people to quarantine regarding testing results
                ##
                
                currDay = time[i-1]
                lastDay = round(currDay - 1 + time.step,0)
                lastDayID = which(time == lastDay)
                newEntryI =             gamma*colSums(mat[lastDayID:(i-1),Iid])*time.step
                newEntryI = newEntryI + gamma*colSums(mat[lastDayID:(i-1),vIid])*time.step
                newEntryI = newEntryI + gamma*colSums(mat[lastDayID:(i-1),bIid])*time.step
                newEntryI = newEntryI + gamma*colSums(mat[lastDayID:(i-1),pIid])*time.step
                newEntryI = newEntryI + gamma*colSums(mat[lastDayID:(i-1),pvIid])*time.step
                newEntryI = newEntryI + gamma*colSums(mat[lastDayID:(i-1),pbIid])*time.step
                toQuaran = newEntryI*q_factor
                
                toQuaran = toQuaran + (ATests+ETests+ITests+
                                           vATests+vETests+vITests+
                                           bATests+bETests+bITests+
                                           pATests+pETests+pITests+
                                           pvATests+pvETests+pvITests+
                                           pbATests+pbETests+pbITests)*q_factor
                
                #print(paste("SBIT detected:",sum(ATests+ETests+ITests)))
                #print(paste("SBIT voluntary:",sum(max((oldI-mat[(i-1/time.step),Iid]),0))))
                
                quaranDenom = oldS+oldA+oldE+oldI+oldR+
                    oldvS+oldvA+oldvE+oldvI+oldvR+
                    oldbS+oldbA+oldbE+oldbI+oldbR+
                    oldpS+oldpA+oldpE+oldpI+oldpR+
                    oldpvS+oldpvA+oldpvE+oldpvI+oldpvR+
                    oldpbS+oldpbA+oldpbE+oldpbI+oldpbR
                
                quaranProportionS = oldS/quaranDenom
                quaranProportionE = oldE/quaranDenom
                quaranProportionA = oldA/quaranDenom
                quaranProportionI = oldI/quaranDenom
                
                quaranProportionvS = oldvS/quaranDenom
                quaranProportionvE = oldvE/quaranDenom
                quaranProportionvA = oldvA/quaranDenom
                quaranProportionvI = oldvI/quaranDenom
                
                quaranProportionbS = oldbS/quaranDenom
                quaranProportionbE = oldbE/quaranDenom
                quaranProportionbA = oldbA/quaranDenom
                quaranProportionbI = oldbI/quaranDenom
                
                quaranProportionpS = oldpS/quaranDenom
                quaranProportionpE = oldpE/quaranDenom
                quaranProportionpA = oldpA/quaranDenom
                quaranProportionpI = oldpI/quaranDenom
                
                quaranProportionpvS = oldpvS/quaranDenom
                quaranProportionpvE = oldpvE/quaranDenom
                quaranProportionpvA = oldpvA/quaranDenom
                quaranProportionpvI = oldpvI/quaranDenom
                
                quaranProportionpbS = oldpbS/quaranDenom
                quaranProportionpbE = oldpbE/quaranDenom
                quaranProportionpbA = oldpbA/quaranDenom
                quaranProportionpbI = oldpbI/quaranDenom
                
                ########
                
                quaranS = rbind(quaranProportionS,quaranProportionvS,quaranProportionbS,
                                quaranProportionpS,quaranProportionpvS,quaranProportionpbS)
                
                quaranEAI = rbind(quaranProportionA+quaranProportionE+quaranProportionI,
                                  quaranProportionvA+quaranProportionvE+quaranProportionvI,
                                  quaranProportionbA+quaranProportionbE+quaranProportionbI,
                                  quaranProportionpA+quaranProportionpE+quaranProportionpI,
                                  quaranProportionpvA+quaranProportionpvE+quaranProportionpvI,
                                  quaranProportionpbA+quaranProportionpbE+quaranProportionpbI)
                
                # ###***Added here
                # print(c(time[i],"total quarantine","pool total", "total recover in pool"))
                # print(c(round(sum(toQuaran),0), round(sum(oldS+oldA+oldE+oldI+oldR),0), round(sum(oldR),0)))
                # ###***End
                
                for(kk in 1:length(protect.levels)){
                    qS[[kk]][1,] = qS[[kk]][1,] + toQuaran*quaranS[kk,]
                    qSbarR[[kk]][1,] = qS[[kk]][1,] + toQuaran*quaranEAI[kk,]
                }
                
                # oldQs = oldQs + toQuaran*quaranProportionS
                # oldQsbar = oldQsbar+toQuaran*(quaranProportionA+quaranProportionE+quaranProportionI)
                # oldQruk = oldQruk+toQuaran*quaranProportionRuk
                
                oldS = oldS - toQuaran*quaranProportionS
                oldE = oldE - toQuaran*quaranProportionE
                oldA = oldA - toQuaran*quaranProportionA
                oldI = oldI - toQuaran*quaranProportionI
                
                oldvS = oldvS - toQuaran*quaranProportionvS
                oldvE = oldvE - toQuaran*quaranProportionvE
                oldvA = oldvA - toQuaran*quaranProportionvA
                oldvI = oldvI - toQuaran*quaranProportionvI
                
                oldbS = oldbS - toQuaran*quaranProportionbS
                oldbE = oldbE - toQuaran*quaranProportionbE
                oldbA = oldbA - toQuaran*quaranProportionbA
                oldbI = oldbI - toQuaran*quaranProportionbI
                
                oldpS = oldpS - toQuaran*quaranProportionpS
                oldpE = oldpE - toQuaran*quaranProportionpE
                oldpA = oldpA - toQuaran*quaranProportionpA
                oldpI = oldpI - toQuaran*quaranProportionpI
                
                oldpvS = oldpvS - toQuaran*quaranProportionpvS
                oldpvE = oldpvE - toQuaran*quaranProportionpvE
                oldpvA = oldpvA - toQuaran*quaranProportionpvA
                oldpvI = oldpvI - toQuaran*quaranProportionpvI
                
                oldpbS = oldpbS - toQuaran*quaranProportionpbS
                oldpbE = oldpbE - toQuaran*quaranProportionpbE
                oldpbA = oldpbA - toQuaran*quaranProportionpbA
                oldpbI = oldpbI - toQuaran*quaranProportionpbI
                
            }
            
            scale.factor = matrix(c(.33,.33,.33,.33,.67,.67,
                                    .67,.67,.67,.67,.67,.67,
                                    .75,.75,.75,.67,.67,.67,
                                    2,  2,  2,1.5,1.5,1.5), ncol = n.groups, byrow = T)
            
            scale.factor = apply(scale.factor,2,function(x){ 1/ (x %*% c(5,5,20,12)/42) })
            
            if((weekdays(today()+time[i]) %notin% c("Saturday","Sunday")) & (time[i] %% 1 == 0)){
                # a. if weekday and first time step, classroom time  
                # Contact matrix (by row): on-campus, off-campus, faculty, staff, community
                
                contact.mat = matrix(c(.55, .30, .10, .05, .00,
                                       .30, .55, .10, .05, .00,
                                       .30, .60, .05, .05, .00,
                                       .10, .20, .10, .60, .00,
                                       .00, .00, .00, .00, 1.0), n.groups-1, n.groups-1,byrow=T)
                
                contact.mat = cmat.mod(contact.mat, n.groups, p.instate)
                
                beta1 = c(.33,.33,.33,.33,.67,.67)*scale.factor*R0*phi    # force of infection for S
                beta2 = beta1*hr[2,]
                beta3 = beta1*hr[3,]
                beta4 = beta1*hr[4,]
                beta5 = beta1*hr[5,]
                beta6 = beta1*hr[6,]
                
            } else if((weekdays(today()+time[i]) %notin% c("Saturday","Sunday")) & (round(time[i] %% 1 - time.step, 5)==0)){
                # b. if weekday and second time step, work time, not classroom time   
                # Contact matrix (by row): on-campus, off-campus, faculty, staff, community
                contact.mat = matrix(c(.80, .10, .05, .05, .00,
                                       .10, .80, .05, .05, .00,
                                       .10, .20, .60, .10, .00,
                                       .10, .20, .10, .60, .00,
                                       .00, .00, .00, .00, 1.0),n.groups-1,n.groups-1,byrow=T)
                
                contact.mat = cmat.mod(contact.mat,n.groups,p.instate)
                
                beta1 = c(.67,.67,.67,.67,.67,.67)*scale.factor*R0*phi    # force of infection for S
                beta2 = beta1*hr[2,]
                beta3 = beta1*hr[3,]
                beta4 = beta1*hr[4,]
                beta5 = beta1*hr[5,]
                beta6 = beta1*hr[6,]
                
            } else if((weekdays(today()+time[i]) %notin% c("Saturday","Sunday")) & (time[i] %% 1 > time.step)){
                # c. weekday after-hours   
                # Contact matrix (by row): on-campus, off-campus, faculty, staff, community
                contact.mat = matrix(c(.95, .05, .00, .00, .00,
                                       .05, .95, .00, .00, .00,
                                       .00, .00, .75, .00, .25,
                                       .00, .00, .00, .75, .25,
                                       .02, .02, .02, .02, .92),n.groups-1,n.groups-1,byrow=T)
                
                contact.mat = cmat.mod(contact.mat,n.groups,p.instate)
                
                beta1 = c(.75,.75,.75,.67,.67,.67)*scale.factor*R0*phi           # force of infection for S
                beta2 = beta1*hr[2,]
                beta3 = beta1*hr[3,]
                beta4 = beta1*hr[4,]
                beta5 = beta1*hr[5,]
                beta6 = beta1*hr[6,]
                
            } else{
                # d. weekend
                # Contact matrix (by row): on-campus, off-campus, faculty, staff, community
                
                contact.mat = matrix(c(.85, .10, .00, .00, .05,
                                       .05, .85, .00, .00, .10,
                                       .00, .00, .50, .00, .50,
                                       .00, .00, .00, .50, .50,
                                       .03, .03, .03, .03, .88),n.groups-1,n.groups-1,byrow=T)
                
                contact.mat = cmat.mod(contact.mat,n.groups,p.instate)
                
                beta1 = c(2,2,2,1.5,1.5,1.5)*scale.factor*R0*phi    # force of infection for S
                beta2 = beta1*hr[2,]
                beta3 = beta1*hr[3,]
                beta4 = beta1*hr[4,]
                beta5 = beta1*hr[5,]
                beta6 = beta1*hr[6,]
            }
            
            # renaming contact mat because we are too lazy to undo old code
            c.mat = contact.mat
            
            
            #################################################
            ######Calculating number in each compartment for current time step, based on results of last time step  
            
            p.AIT = (oldA + oldI + oldT + oldvA + oldvI + oldvT + 
                         oldbA + oldbI + oldbT +
                         oldpA + oldpI + oldpT + oldpvA + oldpvI + oldpvT +
                         oldpbA + oldpbI + oldpbT)/N
            
            ##### Unprotected
            
            qS.temp = qS[[1]]
            qS[[1]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[1]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newQs = colSums(qS[[1]])
            
            newS = oldS + (qS.temp[it,]                                 -beta1*c.mat%*%(p.AIT*oldS)                 )*time.step
            newE = oldE + (beta1*c.mat%*%(p.AIT*oldS)                   -(sigma)*oldE                               )*time.step
            newA = oldA + ((1-alpha)*sigma*oldE                         -(phi)*oldA                                 )*time.step
            newT = oldT + (                                             -kappa*oldT                                 )*time.step
            newTe= oldTe+ (                                             -kappa*oldTe                                )*time.step
            newI = oldI + (alpha*sigma*oldE                             -gamma*oldI                                 )*time.step
            
            Q.temp = isolation[[1]]
            isolation[[1]][1,] = Q.temp[1,] + (kappa*(oldTe+oldT)+gamma*oldI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[1]])){
                isolation[[1]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newQ = colSums(isolation[[1]])
            
            
            qSbarR.temp = qSbarR[[1]]
            qSbarR[[1]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[1]])){
                qSbarR[[1]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newQsbarR = colSums(qSbarR[[1]])
            
            newR = oldR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldA)*time.step
            
            ##### mRNA, no previous infection
            
            qS.temp = qS[[2]]
            qS[[2]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[2]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newvQs = colSums(qS[[2]])
            
            newvS = oldvS + (qS.temp[it,]                                 -beta2*c.mat%*%(p.AIT*oldvS)                 )*time.step
            newvE = oldvE + (beta2*c.mat%*%(p.AIT*oldvS)                  -(sigma)*oldvE                               )*time.step
            newvA = oldvA + ((1-alpha)*sigma*oldvE                        -(phi)*oldvA                                 )*time.step
            newvT = oldvT + (                                             -kappa*oldvT                                 )*time.step
            newvTe= oldvTe+ (                                             -kappa*oldvTe                                )*time.step
            newvI = oldvI + (alpha*sigma*oldvE                            -gamma*oldvI                                 )*time.step
            
            Q.temp = isolation[[2]]
            isolation[[2]][1,] = Q.temp[1,] + (kappa*(oldvTe+oldvT)+gamma*oldvI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[2]])){
                isolation[[2]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newvQ = colSums(isolation[[2]])
            
            
            qSbarR.temp = qSbarR[[2]]
            qSbarR[[2]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[2]])){
                qSbarR[[2]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newvQsbarR = colSums(qSbarR[[2]])
            
            newvR = oldvR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldvA)*time.step
            
            ##### Boosted, no previous infection
            
            qS.temp = qS[[3]]
            qS[[3]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[3]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newbQs = colSums(qS[[3]])
            
            newbS = oldbS + (qS.temp[it,]                                -beta3*c.mat%*%(p.AIT*oldbS)                 )*time.step
            newbE = oldbE + (beta3*c.mat%*%(p.AIT*oldbS)                 -(sigma)*oldbE                               )*time.step
            newbA = oldbA + ((1-alpha)*sigma*oldbE                       -(phi)*oldbA                                 )*time.step
            newbT = oldbT + (                                            -kappa*oldbT                                 )*time.step
            newbTe= oldbTe+ (                                            -kappa*oldbTe                                )*time.step
            newbI = oldbI + (alpha*sigma*oldbE                           -gamma*oldbI                                 )*time.step
            
            Q.temp = isolation[[3]]
            isolation[[3]][1,] = Q.temp[1,] + (kappa*(oldbTe+oldbT)+gamma*oldbI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[3]])){
                isolation[[3]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newbQ = colSums(isolation[[3]])
            
            
            qSbarR.temp = qSbarR[[3]]
            qSbarR[[3]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[3]])){
                qSbarR[[3]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newbQsbarR = colSums(qSbarR[[3]])
            
            newbR = oldbR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldbA)*time.step
            
            ##### Previously infected only
            
            qS.temp = qS[[4]]
            qS[[4]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[4]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newpQs = colSums(qS[[4]])
            
            newpS = oldpS + (qS.temp[it,]                                -beta4*c.mat%*%(p.AIT*oldpS)                 )*time.step
            newpE = oldpE + (beta4*c.mat%*%(p.AIT*oldpS)                 -(sigma)*oldpE                               )*time.step
            newpA = oldpA + ((1-alpha)*sigma*oldpE                       -(phi)*oldpA                                 )*time.step
            newpT = oldpT + (                                            -kappa*oldpT                                 )*time.step
            newpTe= oldpTe+ (                                            -kappa*oldpTe                                )*time.step
            newpI = oldpI + (alpha*sigma*oldpE                           -gamma*oldpI                                 )*time.step
            
            Q.temp = isolation[[4]]
            isolation[[4]][1,] = Q.temp[1,] + (kappa*(oldpTe+oldpT)+gamma*oldpI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[4]])){
                isolation[[4]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newpQ = colSums(isolation[[4]])
            
            
            qSbarR.temp = qSbarR[[4]]
            qSbarR[[4]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[4]])){
                qSbarR[[4]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newpQsbarR = colSums(qSbarR[[4]])
            
            newpR = oldpR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldpA)*time.step
            
            ##### mRNA, previously infected
            
            qS.temp = qS[[5]]
            qS[[5]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[5]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newpvQs = colSums(qS[[5]])
            
            newpvS = oldpvS + (qS.temp[it,]                                 -beta5*c.mat%*%(p.AIT*oldpvS)                 )*time.step
            newpvE = oldpvE + (beta5*c.mat%*%(p.AIT*oldpvS)                 -(sigma)*oldpvE                               )*time.step
            newpvA = oldpvA + ((1-alpha)*sigma*oldpvE                       -(phi)*oldpvA                                 )*time.step
            newpvT = oldpvT + (                                             -kappa*oldpvT                                 )*time.step
            newpvTe= oldpvTe+ (                                             -kappa*oldpvTe                                )*time.step
            newpvI = oldpvI + (alpha*sigma*oldpvE                           -gamma*oldpvI                                 )*time.step
            
            Q.temp = isolation[[5]]
            isolation[[5]][1,] = Q.temp[1,] + (kappa*(oldpvTe+oldpvT)+gamma*oldpvI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[5]])){
                isolation[[5]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newpvQ = colSums(isolation[[5]])
            
            
            qSbarR.temp = qSbarR[[5]]
            qSbarR[[5]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[5]])){
                qSbarR[[5]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newpvQsbarR = colSums(qSbarR[[5]])
            
            newpvR = oldpvR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldpvA)*time.step
            
            ##### Boosted, previously infected
            
            qS.temp = qS[[6]]
            qS[[6]][1,] = qS.temp[1,] - qS.temp[1,]*time.step
            for(it in 2:nrow(qS.temp)){
                qS[[6]][it,] = qS.temp[it,] + (qS.temp[it-1,] - qS.temp[it,])*time.step
            }
            newpbQs = colSums(qS[[6]])
            
            newpbS = oldpbS + (qS.temp[it,]                                 -beta6*c.mat%*%(p.AIT*oldpbS)                 )*time.step
            newpbE = oldpbE + (beta6*c.mat%*%(p.AIT*oldpbS)                 -(sigma)*oldpbE                               )*time.step
            newpbA = oldpbA + ((1-alpha)*sigma*oldpbE                       -(phi)*oldpbA                                 )*time.step
            newpbT = oldpbT + (                                             -kappa*oldpbT                                 )*time.step
            newpbTe= oldpbTe+ (                                             -kappa*oldpbTe                                )*time.step
            newpbI = oldpbI + (alpha*sigma*oldpbE                           -gamma*oldpbI                                 )*time.step
            
            Q.temp = isolation[[6]]
            isolation[[6]][1,] = Q.temp[1,] + (kappa*(oldpbTe+oldpbT)+gamma*oldpbI - Q.temp[1,])*time.step
            for(it in 2:nrow(isolation[[6]])){
                isolation[[6]][it,] = Q.temp[it,] + (Q.temp[it-1,] - Q.temp[it,])*time.step
            }
            newpbQ = colSums(isolation[[6]])
            
            
            qSbarR.temp = qSbarR[[6]]
            qSbarR[[6]][1,] = qSbarR.temp[1,] - qSbarR.temp[1,]*time.step
            for(it in 2:nrow(qSbarR[[6]])){
                qSbarR[[6]][it,] = qSbarR.temp[it,] + (qSbarR.temp[it-1,] - qSbarR.temp[it,])*time.step
            }
            newpbQsbarR = colSums(qSbarR[[6]])
            
            newpbR = oldpbR + (Q.temp[nrow(Q.temp),] + qSbarR.temp[nrow(qSbarR.temp),] + phi*oldpbA)*time.step
            
            ##### Count symptomatic infections
            
            symptomaticCount[nrow(symptomaticCount),-1] = symptomaticCount[nrow(symptomaticCount),-1] +
                gamma*time.step*(oldI+oldvI+oldbI+oldpI+oldpvI+oldpbI)
            
            ##### Bind with mat
            
            mat = rbind(mat, c(time[i], newS, newE, newA, newI, newT, newTe, newQ, newQs, newQsbarR, newR,
                               newvS, newvE, newvA, newvI, newvT, newvTe, newvQ, newvQs, newvQsbarR, newvR,
                               newbS, newbE, newbA, newbI, newbT, newbTe, newbQ, newbQs, newbQsbarR, newbR,
                               newpS, newpE, newpA, newpI, newpT, newpTe, newpQ, newpQs, newpQsbarR, newpR,
                               newpvS, newpvE, newpvA, newpvI, newpvT, newpvTe, newpvQ, newpvQs, newpvQsbarR, newpvR,
                               newpbS, newpbE, newpbA, newpbI, newpbT, newpbTe, newpbQ, newpbQs, newpbQsbarR, newpbR))
            
        }
        
        colnames(mat) = NULL
        
        column.names = c("time","S","E","A","I","T","Te", "Q","Qs","QsbarR","R",
                         "vS","vE","vA","vI","vT","vTe", "vQ","vQs","vQsbarR","vR",
                         "bS","bE","bA","bI","bT","bTe", "bQ","bQs","bQsbarR","bR",
                         "pS","pE","pA","pI","pT","pTe", "pQ","pQs","pQsbarR","pR",
                         "pvS","pvE","pvA","pvI","pvT","pvTe", "pvQ","pvQs","pvQsbarR","pvR",
                         "pbS","pbE","pbA","pbI","pbT","pbTe", "pbQ","pbQs","pbQsbarR","pbR")
        
        # Split mat by affiliation
        
        mat.by.affl = vector(mode = "list", length = n.groups)
        
        for(i in 1:n.groups){
            cols = c(1, which(rep(1:6, times = length(column.names) - 1) == i) + 1)
            mat.by.affl[[i]] = mat[ , cols] 
            colnames(mat.by.affl[[i]]) = column.names 
        }
        
        dayID = which(time%%1 == 0)
        weekID = which(time%%7 == 0)[1:n.weeks]
        final.bar.data = final.symptomatic = NULL
        
        for(k in 1:(n.groups-1)){
            curr = data.frame(mat.by.affl[[k]])
            
            Infection1 = curr[dayID[-1],"S"]+curr[dayID[-1],"Qs"]+
                curr[dayID[-1],"vS"]+curr[dayID[-1],"vQs"]+
                curr[dayID[-1],"bS"]+curr[dayID[-1],"bQs"]+
                curr[dayID[-1],"pS"]+curr[dayID[-1],"pQs"]+
                curr[dayID[-1],"pvS"]+curr[dayID[-1],"pvQs"]+
                curr[dayID[-1],"pbS"]+curr[dayID[-1],"pbQs"]
            
            Infection0 = curr[dayID[-length(dayID)],"S"]+curr[dayID[-length(dayID)],"Qs"]+
                curr[dayID[-length(dayID)],"vS"]+curr[dayID[-length(dayID)],"vQs"]+
                curr[dayID[-length(dayID)],"bS"]+curr[dayID[-length(dayID)],"bQs"]+
                curr[dayID[-length(dayID)],"pS"]+curr[dayID[-length(dayID)],"pQs"]+
                curr[dayID[-length(dayID)],"pvS"]+curr[dayID[-length(dayID)],"pvQs"]+
                curr[dayID[-length(dayID)],"pbS"]+curr[dayID[-length(dayID)],"pbQs"]
            
            newInfection = Infection0 - Infection1
            
            # creating variable for number of weekly new infections
            #n.weeks=time.max/7
            newWeeklyInfection=numeric(n.weeks)
            newWeeklyInfection[1]=sum(newInfection[1:7])
            newSymptomatic=numeric(n.weeks)
            newSymptomatic[1]=sum(symptomaticCount[1:8,k+1]) - sum(I.init[,k])
            for(i in 2:n.weeks){
                newWeeklyInfection[i] = sum(newInfection[(7*(i-1)+1):(7*i)])
                newSymptomatic[i] = sum(symptomaticCount[(7*(i-1)+2):(7*i+1),k+1])
            }
            
            if(k == 2){
                final.bar.data = final.bar.data + newWeeklyInfection
                final.symptomatic = final.symptomatic + newSymptomatic
            } else{
                final.bar.data = c(final.bar.data,newWeeklyInfection)
                final.symptomatic = c(final.symptomatic, newSymptomatic)
            }
        }
        
        if(out.type == 0){
            week = rep(1:n.weeks,n.groups-2)
            subject = rep(c("On-campus", "Off-campus", "Faculty", "Staff"), each=n.weeks)
            subject = factor(subject, levels = c("On-campus", "Off-campus", "Faculty", "Staff"))
            bar.data = data.frame(week = week, subject = subject, new.pos.cases = final.bar.data)
            tot.cases = (bar.data %>% group_by(subject) %>% summarize(total.cases = round(sum(new.pos.cases))))$total.cases
            
            ### prevalence estimate and total detected infections
            
            bar.data = bar.data %>% mutate(prevalence = paste(round(100*new.pos.cases / rep(c(N[1]+N[2],N[3],N[4],N[5]),
                                                                                            each=n.weeks),1),"%",sep=""))
            
            ### bar plot
            
            bar_cols = brewer.pal(n = 5, name = "Dark2")
            bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3], bar_cols[4])
            
            p1 = ggplot(bar.data, aes(x=week, y=new.pos.cases, fill=subject)) + 
                geom_bar(stat='identity', position='dodge') + 
                geom_text(aes(label=paste0(round(new.pos.cases), " (", prevalence, ")"),colour=subject,fontface="bold"),
                          angle = 90, size=7,position=position_dodge(width=0.9), vjust=-0.45, hjust = -0.2, show.legend=FALSE) +
                scale_colour_manual(values=bar_cols) +
                scale_x_continuous(name="Week", breaks=1:n.weeks) +
                scale_y_continuous(name="New Covid-19 positive cases",limits = c(0,5.5*max(bar.data$new.pos.cases))) +
                #scale_fill_manual(name="Summary : ", 
                #                    labels=c(paste0("Total infected on-campus students = ", total.infections[1]+total.infections[2]),
                #                             paste0("Total infected off-campus students = ",total.infections[3]), 
                #                             paste0("Total infected faculty = "             , total.infections[4]),
                #                             paste0("Total infected staff = ",total.infections[5])), values = bar_cols)+
                scale_fill_manual(name="Summary : ", 
                                  labels=c(paste0("Residential students: ", tot.cases[1]),
                                           paste0("Non-residential students: ", tot.cases[2]),
                                           paste0("Faculty: ", tot.cases[3]),
                                           paste0("Staff: ", tot.cases[4])
                                  ),values = bar_cols)+
                theme(legend.position = c(.85,.75), 
                      legend.direction = "vertical",
                      legend.key.size = unit(1, 'cm'),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size=20),
                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))
        } else{
            
            final.bar.data.2 = tapply(final.bar.data, INDEX = c(rep(1:n.weeks,2), rep((n.weeks+1):(2*n.weeks),2)), sum)
            
            week = rep(1:n.weeks,times = 2)
            subject = rep(c("Students", "Employees"), each=n.weeks)
            subject = factor(subject, levels = c("Students", "Employees"))
            bar.data = data.frame(week = week, subject = subject, new.pos.cases = final.bar.data.2)
            tot.cases.2 = (bar.data %>% group_by(subject) %>% summarize(total.cases = round(sum(new.pos.cases))))$total.cases
            
            ### prevalence estimate and total detected infections
            
            bar.data = bar.data %>% mutate(prevalence = paste(round(100*new.pos.cases / rep(c(N[1]+N[2]+N[3],N[4]+N[5]),
                                                                                            each=n.weeks),1),"%",sep=""))
            
            ### bar plot
            
            bar_cols = brewer.pal(n = 5, name = "Dark2")
            bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3], bar_cols[4])
            
            p1 = ggplot(bar.data, aes(x=week, y=new.pos.cases, fill=subject)) + 
                geom_bar(stat='identity', position='dodge') + 
                geom_text(aes(label=paste0(round(new.pos.cases), " (", prevalence, ")"),colour=subject,fontface="bold"),
                          angle = 90, size=7,position=position_dodge(width=0.9), vjust=-0.45, hjust = -0.2, show.legend=FALSE) +
                scale_colour_manual(values=bar_cols) +
                scale_x_continuous(name="Week", breaks=1:n.weeks) +
                scale_y_continuous(name="New Covid-19 positive cases",limits = c(0,5.5*max(bar.data$new.pos.cases))) +
                #scale_fill_manual(name="Summary : ", 
                #                    labels=c(paste0("Total infected on-campus students = ", total.infections[1]+total.infections[2]),
                #                             paste0("Total infected off-campus students = ",total.infections[3]), 
                #                             paste0("Total infected faculty = "             , total.infections[4]),
                #                             paste0("Total infected staff = ",total.infections[5])), values = bar_cols)+
                scale_fill_manual(name="Summary : ", 
                                  labels=c(paste0("Students: ", tot.cases.2[1]),
                                           paste0("Employees: ", tot.cases.2[2])
                                  ),values = bar_cols)+
                theme(legend.position = c(.85,.75), 
                      legend.direction = "vertical",
                      legend.key.size = unit(1, 'cm'),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size=20),
                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))
        }
        
        ### Symptomatic infections
        
        if(out.type == 0){
            week = rep(1:n.weeks,n.groups-2)
            subject = rep(c("On-campus", "Off-campus", "Faculty", "Staff"), each=n.weeks)
            subject = factor(subject, levels = c("On-campus", "Off-campus", "Faculty", "Staff"))
            bar.data = data.frame(week = week, subject = subject, new.pos.cases = final.symptomatic)
            tot.cases = (bar.data %>% group_by(subject) %>% summarize(total.cases = round(sum(new.pos.cases))))$total.cases
            
            ### prevalence estimate and total detected infections
            
            bar.data = bar.data %>% mutate(prevalence = paste(round(100*new.pos.cases / rep(c(N[1]+N[2],N[3],N[4],N[5]),
                                                                                            each=n.weeks),1),"%",sep=""))
            
            ### bar plot
            
            bar_cols = brewer.pal(n = 5, name = "Dark2")
            bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3], bar_cols[4])
            
            p2 = ggplot(bar.data, aes(x=week, y=new.pos.cases, fill=subject)) + 
                geom_bar(stat='identity', position='dodge') + 
                geom_text(aes(label=paste0(round(new.pos.cases), " (", prevalence, ")"),colour=subject,fontface="bold"),
                          angle = 90, size=7,position=position_dodge(width=0.9), vjust=-0.45, hjust = -0.2, show.legend=FALSE) +
                scale_colour_manual(values=bar_cols) +
                scale_x_continuous(name="Week", breaks=1:n.weeks) +
                scale_y_continuous(name="New symptomatic cases",limits = c(0,5.5*max(bar.data$new.pos.cases))) +
                #scale_fill_manual(name="Summary : ", 
                #                    labels=c(paste0("Total infected on-campus students = ", total.infections[1]+total.infections[2]),
                #                             paste0("Total infected off-campus students = ",total.infections[3]), 
                #                             paste0("Total infected faculty = "             , total.infections[4]),
                #                             paste0("Total infected staff = ",total.infections[5])), values = bar_cols)+
                scale_fill_manual(name="Summary : ", 
                                  labels=c(paste0("Residential students: ", tot.cases[1]),
                                           paste0("Non-residential students: ", tot.cases[2]),
                                           paste0("Faculty: ", tot.cases[3]),
                                           paste0("Staff: ", tot.cases[4])
                                  ),values = bar_cols)+
                theme(legend.position = c(.85,.75), 
                      legend.direction = "vertical",
                      legend.key.size = unit(1, 'cm'),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size=20),
                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))
        } else{
            final.symptomatic.2 = tapply(final.symptomatic, INDEX = c(rep(1:n.weeks,2), rep((n.weeks+1):(2*n.weeks),2)), sum)
            
            week = rep(1:n.weeks,times = 2)
            subject = rep(c("Students", "Employees"), each=n.weeks)
            subject = factor(subject, levels = c("Students", "Employees"))
            bar.data = data.frame(week = week, subject = subject, new.pos.cases = final.symptomatic.2)
            tot.cases.2 = (bar.data %>% group_by(subject) %>% summarize(total.cases = round(sum(new.pos.cases))))$total.cases
            
            ### prevalence estimate and total detected infections
            
            bar.data = bar.data %>% mutate(prevalence = paste(round(100*new.pos.cases / rep(c(N[1]+N[2]+N[3],N[4]+N[5]),
                                                                                            each=n.weeks),1),"%",sep=""))
            
            ### bar plot
            
            bar_cols = brewer.pal(n = 5, name = "Dark2")
            bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3], bar_cols[4])
            
            p2 = ggplot(bar.data, aes(x=week, y=new.pos.cases, fill=subject)) + 
                geom_bar(stat='identity', position='dodge') + 
                geom_text(aes(label=paste0(round(new.pos.cases), " (", prevalence, ")"),colour=subject,fontface="bold"),
                          angle = 90, size=7,position=position_dodge(width=0.9), vjust=-0.45, hjust = -0.2, show.legend=FALSE) +
                scale_colour_manual(values=bar_cols) +
                scale_x_continuous(name="Week", breaks=1:n.weeks) +
                scale_y_continuous(name="New symptomatic cases",limits = c(0,5.5*max(bar.data$new.pos.cases))) +
                #scale_fill_manual(name="Summary : ", 
                #                    labels=c(paste0("Total infected on-campus students = ", total.infections[1]+total.infections[2]),
                #                             paste0("Total infected off-campus students = ",total.infections[3]), 
                #                             paste0("Total infected faculty = "             , total.infections[4]),
                #                             paste0("Total infected staff = ",total.infections[5])), values = bar_cols)+
                scale_fill_manual(name="Summary : ", 
                                  labels=c(paste0("Students: ", tot.cases.2[1]),
                                           paste0("Employees: ", tot.cases.2[2])
                                  ),values = bar_cols)+
                theme(legend.position = c(.85,.75), 
                      legend.direction = "vertical",
                      legend.key.size = unit(1, 'cm'),
                      legend.title = element_text(size = 20),
                      legend.text = element_text(size=20),
                      axis.text.x = element_text(size=20),
                      axis.text.y = element_text(size=20),
                      axis.title.x = element_text(size=20),
                      axis.title.y = element_text(size=20))
        }
        
        ### Isolation plot: Students
        
        iso.outofstate = mat.by.affl[[2]][dayID,"Q"] + mat.by.affl[[2]][dayID,"vQ"] + mat.by.affl[[2]][dayID,"bQ"] + 
            mat.by.affl[[2]][dayID,"pQ"] + mat.by.affl[[2]][dayID,"pvQ"] + mat.by.affl[[2]][dayID,"pbQ"]
        
        iso.oncampus = iso.outofstate + 
            mat.by.affl[[1]][dayID,"Q"] + mat.by.affl[[1]][dayID,"vQ"] + mat.by.affl[[1]][dayID,"bQ"] + 
            mat.by.affl[[1]][dayID,"pQ"] + mat.by.affl[[1]][dayID,"pvQ"] + mat.by.affl[[1]][dayID,"pbQ"]
        
        iso.offcampus = mat.by.affl[[3]][dayID,"Q"] + mat.by.affl[[3]][dayID,"vQ"] + mat.by.affl[[3]][dayID,"bQ"] + 
            mat.by.affl[[3]][dayID,"pQ"] + mat.by.affl[[3]][dayID,"pvQ"] + mat.by.affl[[3]][dayID,"pbQ"]
        
        iso.allstudents = iso.oncampus + iso.offcampus
        
        IsoPlot = data.frame("dayID" = rep(time[dayID],3),"NumberOfIsolation"=c(iso.oncampus,iso.offcampus,iso.allstudents),
                             "subject" = rep(c("Residential","Non-residential","All students"),each=length(dayID)))
        IsoPlot$subject = factor(IsoPlot$subject,levels = c("Residential","Non-residential","All students"))
        IsoMax = c(max(iso.oncampus),max(iso.offcampus),max(iso.allstudents))
        
        bar_cols = brewer.pal(n = 4, name = "Dark2")
        bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3])
        
        p3 =ggplot(IsoPlot, aes(x=dayID, y=NumberOfIsolation, color=subject, group = subject))+
            geom_line(size=1.25)+
            scale_x_continuous(name="Day")+
            scale_y_continuous(name="Number in Isolation",limits = c(0,ceiling(max(IsoMax))+1))+
            scale_color_manual(name="Summary : ", values = bar_cols, 
                               labels=c(paste0("Max isolations among residential students = ", round(IsoMax[1])),
                                        paste0("Max isolations among non-residential students = ", round(IsoMax[2])),
                                        paste0("Max isolations among all students = ", round(IsoMax[3]))))+
            theme(legend.position = "bottom",
                  legend.direction = "vertical",
                  legend.key.size = unit(1, 'cm'),
                  legend.title = element_text(size = 20),
                  legend.text = element_text(size=20),
                  axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
        ### I/Q plot: Students
        
        iq.outofstate = iso.outofstate + 
            mat.by.affl[[2]][dayID,"Qs"] + mat.by.affl[[2]][dayID,"vQs"] + mat.by.affl[[2]][dayID,"bQs"] + 
            mat.by.affl[[2]][dayID,"pQs"] + mat.by.affl[[2]][dayID,"pvQs"] + mat.by.affl[[2]][dayID,"pbQs"] + 
            mat.by.affl[[2]][dayID,"QsbarR"] + mat.by.affl[[2]][dayID,"vQsbarR"] + mat.by.affl[[2]][dayID,"bQsbarR"] + 
            mat.by.affl[[2]][dayID,"pQsbarR"] + mat.by.affl[[2]][dayID,"pvQsbarR"] + mat.by.affl[[2]][dayID,"pbQsbarR"]
        
        iq.oncampus = iso.oncampus + 
            mat.by.affl[[1]][dayID,"Qs"] + mat.by.affl[[1]][dayID,"vQs"] + mat.by.affl[[1]][dayID,"bQs"] + 
            mat.by.affl[[1]][dayID,"pQs"] + mat.by.affl[[1]][dayID,"pvQs"] + mat.by.affl[[1]][dayID,"pbQs"] + 
            mat.by.affl[[1]][dayID,"QsbarR"] + mat.by.affl[[1]][dayID,"vQsbarR"] + mat.by.affl[[1]][dayID,"bQsbarR"] + 
            mat.by.affl[[1]][dayID,"pQsbarR"] + mat.by.affl[[1]][dayID,"pvQsbarR"] + mat.by.affl[[1]][dayID,"pbQsbarR"]
        
        iq.offcampus = iso.offcampus + 
            mat.by.affl[[3]][dayID,"Qs"] + mat.by.affl[[3]][dayID,"vQs"] + mat.by.affl[[3]][dayID,"bQs"] + 
            mat.by.affl[[3]][dayID,"pQs"] + mat.by.affl[[3]][dayID,"pvQs"] + mat.by.affl[[3]][dayID,"pbQs"] + 
            mat.by.affl[[3]][dayID,"QsbarR"] + mat.by.affl[[3]][dayID,"vQsbarR"] + mat.by.affl[[3]][dayID,"bQsbarR"] + 
            mat.by.affl[[3]][dayID,"pQsbarR"] + mat.by.affl[[3]][dayID,"pvQsbarR"] + mat.by.affl[[3]][dayID,"pbQsbarR"]
        
        iq.allstudents = iq.oncampus + iq.offcampus
        
        
        IQPlot = data.frame("dayID" = rep(time[dayID],3),"NumberOfIsolation"=c(iq.oncampus,iq.offcampus,iq.allstudents),
                            "subject" = rep(c("Residential","Non-residential","All students"),each=length(dayID)))
        IQPlot$subject = factor(IQPlot$subject,levels = c("Residential","Non-residential","All students"))
        IQMax = c(max(iq.oncampus),max(iq.offcampus),max(iq.allstudents))
        
        bar_cols = brewer.pal(n = 4, name = "Dark2")
        bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3])
        
        
        p4 = ggplot(IQPlot, aes(x=dayID, y=NumberOfIsolation, color=subject, group = subject))+
            geom_line(size=1.25)+
            scale_x_continuous(name="Day")+
            scale_y_continuous(name="Number in I/Q",limits = c(0,ceiling(max(IQMax))+2))+
            scale_color_manual(name="Summary : ", values = bar_cols, 
                               labels=c(paste0("Max I/Q among out-of-state students = ", round(IQMax[1])),
                                        paste0("Max I/Q among residential students = ", round(IQMax[2])),
                                        paste0("Max I/Q among all students = ", round(IQMax[3])))) +
            theme(legend.position = "bottom",
                  legend.direction = "vertical",
                  legend.key.size = unit(1, 'cm'),
                  legend.title = element_text(size = 20),
                  legend.text = element_text(size=20),
                  axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
        
        ### Isolation Plot: Faculty & Staff
        
        iso.faculty = mat.by.affl[[4]][dayID,"Q"] + mat.by.affl[[4]][dayID,"vQ"] + mat.by.affl[[4]][dayID,"bQ"] + 
            mat.by.affl[[4]][dayID,"pQ"] + mat.by.affl[[4]][dayID,"pvQ"] + mat.by.affl[[4]][dayID,"pbQ"]
        
        iso.staff = mat.by.affl[[5]][dayID,"Q"] + mat.by.affl[[5]][dayID,"vQ"] + mat.by.affl[[5]][dayID,"bQ"] + 
            mat.by.affl[[5]][dayID,"pQ"] + mat.by.affl[[5]][dayID,"pvQ"] + mat.by.affl[[5]][dayID,"pbQ"]
        
        IsoPlot2 = data.frame("dayID" = rep(time[dayID],2),"NumberOfIsolation"=c(iso.faculty,iso.staff),
                              "subject" = rep(c("Faculty","Staff"),each=length(dayID)))
        IsoPlot2$subject = factor(IsoPlot2$subject,levels = c("Faculty","Staff"))
        IsoMax2 = c(max(iso.faculty),max(iso.staff))
        
        bar_cols = brewer.pal(n = 4, name = "Dark2")
        bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3])
        
        
        p5 = ggplot(IsoPlot2, aes(x=dayID, y=NumberOfIsolation, color=subject, group = subject))+
            geom_line(size=1.25)+
            scale_x_continuous(name="Day")+
            scale_y_continuous(name="Number in I/Q",limits = c(0,ceiling(max(IsoMax2))+2))+
            scale_color_manual(name="Summary : ", values = bar_cols, 
                               labels=c(paste0("Max Isolation among faculty = ", round(IsoMax2[1])),
                                        paste0("Max Isolation among staff = ", round(IsoMax2[2]))))+
            theme(legend.position = "bottom",
                  legend.direction = "vertical",
                  legend.key.size = unit(1, 'cm'),
                  legend.title = element_text(size = 20),
                  legend.text = element_text(size=20),
                  axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
        ### I/Q Plot: Faculty & Staff
        
        iq.faculty = iso.faculty + 
            mat.by.affl[[4]][dayID,"Qs"] + mat.by.affl[[4]][dayID,"vQs"] + mat.by.affl[[4]][dayID,"bQs"] + 
            mat.by.affl[[4]][dayID,"pQs"] + mat.by.affl[[4]][dayID,"pvQs"] + mat.by.affl[[4]][dayID,"pbQs"] + 
            mat.by.affl[[4]][dayID,"QsbarR"] + mat.by.affl[[4]][dayID,"vQsbarR"] + mat.by.affl[[4]][dayID,"bQsbarR"] + 
            mat.by.affl[[4]][dayID,"pQsbarR"] + mat.by.affl[[4]][dayID,"pvQsbarR"] + mat.by.affl[[4]][dayID,"pbQsbarR"]
        
        iq.staff = iso.staff + 
            mat.by.affl[[5]][dayID,"Qs"] + mat.by.affl[[5]][dayID,"vQs"] + mat.by.affl[[5]][dayID,"bQs"] + 
            mat.by.affl[[5]][dayID,"pQs"] + mat.by.affl[[5]][dayID,"pvQs"] + mat.by.affl[[5]][dayID,"pbQs"] + 
            mat.by.affl[[5]][dayID,"QsbarR"] + mat.by.affl[[5]][dayID,"vQsbarR"] + mat.by.affl[[5]][dayID,"bQsbarR"] + 
            mat.by.affl[[5]][dayID,"pQsbarR"] + mat.by.affl[[5]][dayID,"pvQsbarR"] + mat.by.affl[[5]][dayID,"pbQsbarR"]
        
        IQPlot2 = data.frame("dayID" = rep(time[dayID],2),"NumberOfIsolation"=c(iq.faculty,iq.staff),
                             "subject" = rep(c("Faculty","Staff"),each=length(dayID)))
        IQPlot2$subject = factor(IQPlot2$subject,levels = c("Faculty","Staff"))
        IQMax2 = c(max(iq.faculty),max(iq.staff))
        
        bar_cols = brewer.pal(n = 4, name = "Dark2")
        bar_cols = c(bar_cols[1], bar_cols[2], bar_cols[3])
        
        p6 = ggplot(IQPlot2, aes(x=dayID, y=NumberOfIsolation, color=subject, group = subject))+
            geom_line(size=1.25)+
            scale_x_continuous(name="Day")+
            scale_y_continuous(name="Number in I/Q",limits = c(0,ceiling(max(IQMax2))+2))+
            scale_color_manual(name="Summary : ", values = bar_cols, 
                               labels=c(paste0("Max I/Q among faculty = ", round(IQMax2[1])),
                                        paste0("Max I/Q among staff = ", round(IQMax2[2]))))+
            theme(legend.position = "bottom",
                  legend.direction = "vertical",
                  legend.key.size = unit(1, 'cm'),
                  legend.title = element_text(size = 20),
                  legend.text = element_text(size=20),
                  axis.text.x = element_text(size=20),
                  axis.text.y = element_text(size=20),
                  axis.title.x = element_text(size=20),
                  axis.title.y = element_text(size=20))
        
        
        return(list("p1" = p1, "p2" = p2, "p3" = p3, "p4" = p4, "p5" = p5, "p6" = p6, "summary" = sum.stats.tab, 
                    "protect_summary" = protect))
    }
    
    rerunResults = eventReactive(input$rerun, {savePlots()})
    
    # weekly infections
    output$plot1 = renderPlot({rerunResults()$p1}) ###end of plot1
    
    # weekly symptomatic infections
    output$plot2 = renderPlot({rerunResults()$p2}) ###end of plot2
    
    # daily isolation for students
    output$plot3 = renderPlot({rerunResults()$p3}) ###end of plot2
    
    # daily i/q for students
    output$plot4 = renderPlot({rerunResults()$p4}) ###end of plot3
    
    # daily isolation for faculty & staff
    output$plot5 = renderPlot({rerunResults()$p5}) ###end of plot4
    
    # daily i/q for faculty & staff
    output$plot6 = renderPlot({rerunResults()$p6}) ###end of plot5
    
    # Baseline summary statistics
    output$summary = renderTable({rerunResults()$summary})
    
    # Baseline infection
    
    # output$infect_summary = renderTable({rerunResults()$infect_summary})
    
    # Protection summary
    
    output$protect_summary = renderTable({rerunResults()$protect_summary})
    
    # Baseline I/Q summary
    
    # output$iq_summary = renderTable({rerunResults()$iq_summary})
}

shinyApp(ui,server)
