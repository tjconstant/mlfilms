library(shiny)
#library(mlfilms)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Kretschmann-Raether SPP Angle Scan"),
  
  # Sidebar with a slider input for varibles
  sidebarPanel(
    
    checkboxInput("p.pol","p-Polarisation", T),
    
    checkboxInput("s.pol","s-Polarisation", F),
    
    checkboxInput("fit_flag","attempt fit of metal layer",F),
    
    sliderInput("wavelength", 
                "Incident Wavelength (nm)", 
                min = 450, 
                max = 750, 
                value = 633),
    
    sliderInput("thickness", animate=T,
                "Metal Thickness (nm)", 
                min = 0, 
                max = 100, 
                value = 45),
    
    sliderInput("epsiR", 
                "Metal Real Permittivity", 
                min = -50, 
                max = -5, 
                value = -16,
                step=0.5),
    
    sliderInput("epsiI", 
                "Metal Imaginary Permittivity", 
                min = 0, 
                max = 4, 
                value = 1.04,
                step=0.1),
    
    sliderInput("overlayer_index", 
                "Overlayer Refractive Index", 
                min = 1, 
                max = 3, 
                value = 1,
                step=0.01),
    
    sliderInput("overlayer_thickness", 
                "Overlayer Thickness (nm)", 
                min = 0, 
                max = 100, 
                value = 0),
    
    fileInput('datafile',label="Upload Data",multiple=F)
    

  ),
  
  # Show the plot
  mainPanel(
    tabsetPanel(
    tabPanel("Plot", plotOutput("reflPlot")),
    tabPanel("Data File", tableOutput("filetable")),
    tabPanel("Fit Results",tableOutput("fittedparameters"))
    )
  )
))