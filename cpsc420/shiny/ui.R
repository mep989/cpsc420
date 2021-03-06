
library(shiny)

shinyUI(fluidPage(

    tags$head(tags$link(rel="stylesheet", type="text/css", href="shiny.css")),

    titlePanel("CPSC 420 -- System Dynamics examples"),

    tabsetPanel(selected="Cows (Hannah)",

	tabPanel("Cows (Hannah)",
		sidebarLayout(sidebarPanel(
		  numericInput("deltat", label="Delta T", value=1),
			numericInput("time", label="Time (days)", value=100),
			numericInput("acow", label="Farmer A's Inital cows", value=25),
      numericInput("bcow", label="Farmer B's Inital cows", value=40),
      numericInput("Aearnings", label="Farmer A's Inital money", value=200),
      numericInput("Bearnings", label="Farmer B's Inital money", value=200),
      sliderInput("grass", label="Initial Blades of Grass in the Pasture",
          		min = 500, max= 3000, value=800),
      sliderInput("coweatrate", label="Cow Eating Rate (Blade of Grass/Cow/Day)",
          		min = 0, max= 0.5, value=0.3),
      sliderInput("grassgrowth", label="Grass Growth Rate (Blade of Grass/Day/Blade of Grass)",            		
                  min = 0, max= 0.1, value=0.02),
      sliderInput("a", label="Farmer A Reinvestment (dollars/dollar)",
              		min = 0, max= 1, value=0.15),
      sliderInput("b", label="Farmer B Reinvestment (Dollars/Dollar)",
              		min = 0, max= 1, value=0.15),
     	numericInput("cowcost", label="Cost per Cow (Dollars)", value=20),
    	numericInput("profit", label="Profit per Cow (Dollars/Cow/Day)",
          		value=4),
      numericInput("blades", label="Blades of Grass Consumed (Blades of Grass/Cow/Day)",
                		value=8),
    	selectInput("birth", label="Cow births (Cows/Day)", choices=list("None" = 0,
                                                            "One" = 1,
                                                            "Two" = 2,
                                                            "Three" = 3), selected=2),
      numericInput("y", label="y-Axis Max", value=1000)
      
		),
      		mainPanel(
       	 		plotOutput("Plot")
     		))
	),
        tabPanel("Cows (Thomas)",
                 sidebarLayout(sidebarPanel(
                   numericInput("time", "Simulation time (weeks)",
                                value=50, min=0, step=1, width="40%"),
                   sliderInput("farmersOnPasture", "Farmers using the pasture",
                               min=1, max=100, step=1, value=10),
                   sliderInput("initCowsPerFarmer", 
                               "Initial Cows on pasture (per farmer)",
                               min=1, max=10, step=1, value=2),
                   sliderInput("startingGrass", 
                               "Initial Grass on pasture (1 = 1 cow food week)",
                               min=100, max=5000, step=10, value=500),
                   sliderInput("grassRegrowthRate", 
                               "Rate at which grass grows back",
                               min=0, max=1, step=.01, value=.2),
                   sliderInput("sustainabilityDecisionRatio", 
                               "Sustainability decision ratio for farmers",
                               min=0, max=5, step=1, value=3)
                 ),
                 mainPanel(
                   plotOutput("cowsTragedyPlot")
                 ))
        ),

        tabPanel("ToyWars (Ruth)",
                sidebarLayout(sidebarPanel(
       			sliderInput("toySimLength", "Simulation time (weeks)", 
				value=2, min=0, step=1, max=52), div(class="row",
                       		div(class="col-lg-6", 
                        sliderInput("Child1BragRate",
                                "Child1 Brag Rate",
                                value=0, min=0, step=.1, max=3)
                       	),
                       	div(class="col-lg-6", 
                           sliderInput("Child2BragRate",
                                       "Child2 Brag Rate",
                                       value=0, min=0, step=.1, max=3)
                       	)
                   ),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("Child1WinRate",
                                       "Child1's desired percentage Above Child2",
                                       value=0, min=0, step=.1, max=3)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("Child2WinRate",
                                       "Child2's desired percentage Above Child1",
                                       value=0, min=0, step=.1, max=3)
                       )
                   ),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("Child1Piggy",
                                       "Child1 Piggy Bank ($)",
                                       value=20, min=0, step=1, max=100)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("Child2Piggy",
                                       "Child2 Piggy Bank ($)",
                                       value=20, min=0, step=1, max=100)
                       )
                   ),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("Child1Start",
                                       "Child1 Toys to Start",
                                       value=10, min=0, step=1, max=100)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("Child2Start",
                                       "Child2 Toys to Start",
                                       value=5, min=0, step=1, max=100)
                       )
                   ),
                   sliderInput("childAllowance",
                               "Allowance ($/week)",
                               value=10, min=0, step=1, max=50),
                   sliderInput("toyCost",
                               "Cost of a Toy ($/toy)",
                               value=5, min=0, step=1, max=100)
                 ),
                 mainPanel(
                   plotOutput("toysTimePlot")
                 ))
        ),
        tabPanel("Email Common (Elias)", 
                 sidebarLayout(sidebarPanel(
                   numericInput("emailsSimLength", "Simulation time (days)", 
                                value=100, min=0, step=1, width="40%"), 
                   sliderInput("spamPercentage", "Percent Spam Emails Make up (spam emails/total emails)", 
                               min=0, max=1, step=0.1, value=0.8), 
                   sliderInput("inflowRate", "Rate of Non-Spam emails ((email/day)/email)", 
                               min=0, max=1, step=0.01, value=0.05), 
                   sliderInput("outflowRate", "Rate of Spam emails ((email/day)/email)", 
                               min = 0, max=1, step=0.01, val=0.04)), 
                   mainPanel(
                     plotOutput("commonGoodPlot")
                   ))
        ),
        
         tabPanel("Zombie Apocalypse (Brian)",
           sidebarLayout(sidebarPanel(
             sliderInput("dullnessRate", 
                         "Rate at which Blade Dulls (healthPoints/day)",
                         min=0, max=1, step=.001, value=.003),
             sliderInput("zombieDamageRate", 
                         "Zombie Damage Rate (healthPoints/day per Zombie)",
                         min=0, max=100, step=.1, value=2),
            
             sliderInput("zombiesWall1", "Zombies Attacking Wall One",
                         min=0, max=20, step=1, value=4),
             sliderInput("guardOneKillRate", 
                         "Guard One's Kill Rate",
                         min=0, max=1, step=.1, value=.8),
             sliderInput("wallOneRebuildRate", 
                         "Rate at which wall one is rebuilt (healthPoint/day)",
                         min=0, max=50, step=1, value=3),
             sliderInput("zombiesWall2", "Zombies Attacking Wall Two",
                         min=0, max=20, step=1, value=3),
             sliderInput("guardTwoKillRate", 
                         "Guard Two's Kill Rate",
                         min=0, max=1, step=.1, value=.8),
             sliderInput("wallTwoRebuildRate", 
                         "Rate at which Wall Two is Rebuilt (healthPoint/day)",
                         min=0, max=50, step=1, value=2),
             sliderInput("zombiesWall3", "Zombies Attacking Wall three",
                         min=0, max=20, step=1, value=4),
             sliderInput("guardThreeKillRate", 
                         "Guard Three's Kill Rate",
                         min=0, max=1, step=.1, value=.8),
             sliderInput("wallThreeRebuildRate", 
                         "Rate at which Wall Three is Rebuilt (healthPoint/day)",
                         min=0, max=50, step=1, value=4),
             sliderInput("zombiesWall4", "Zombies Attacking Wall four",
                         min=0, max=20, step=1, value=6),
             sliderInput("guardFourKillRate", 
                         "Guard Four's Kill Rate",
                         min=0, max=1, step=.1, value=.5),
             sliderInput("wallFourRebuildRate", 
                         "Rate at which Wall Four is Rebuilt (healthPoint/day)",
                         min=0, max=50, step=1, value=7),
             numericInput("simLength", "Simulation time (days)",
                         value=365*4, min=0, step=1, width="40%")
           
           ),
           mainPanel(
             plotOutput("zombiePlot")
           ))
         ),
        tabPanel("Intergalactic Smugglers (Alex)",
                 sidebarLayout(sidebarPanel(
                   numericInput("simLength", "Simulation time (Galactic Standard Days [GSD])",
                                value=125, min=1, step=1, width="40%"),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("initPatrols",
                                       "Initial number of Security Patrols",
                                       value=1, min=0, step=1, max=100)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("initContraband",
                                       "Initial Number of Contraband Shipments",
                                       value=10, min=0, step=1, max=100)
                       )
                   ),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("desiredShipments",
                                       "The Desired Number of Shipments the Black Market Wants",
                                       value=10, min=0, step=1, max=100)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("demand",
                                       "Number of Credits Black Market will Pay per Shipment",
                                       value=100, min=0, step=100, max=1000)
                       )
                   ),
                   div(class="row",
                       div(class="col-lg-6", 
                           sliderInput("smuggleRate",
                                       "Number of Shipments a Smuggler can Deliver in 1 GSD",
                                       value=1, min=0, step=.1, max=3)
                       ),
                       div(class="col-lg-6", 
                           sliderInput("patrolRate",
                                       "Security Level (Patrols / Shipment of Commodity)",
                                       value=.5, min=0, step=.1, max=5)
                       )
                   ),
                   sliderInput("confiscationRatio",
                               "Rate at which an Encounter Results in a Confiscation",
                               value=.75, min=0, step=.01, max=1),
                   sliderInput("encounterFrequency",
                               "Encounter Frequency",
                               value=.25, min=0, step=.01, max=1),
                   sliderInput("contractIncentive",
                               "New Smugglers per 1000 Credit Offered",
                               value=.001, min=0, step=.001, max=.01)
                 ),
                 
                 mainPanel(
                   plotOutput("plot.contraband.plot")
                 ))
        ),
        tabPanel("Population (Michael P)",
            sidebarLayout(sidebarPanel(
              h3("Controls:"),
      
              sliderInput("valS", label=h4("Starting Year:"), 
                min=1900, max=2000, value=2000),
              sliderInput("valE", label=h4("Ending Year:"), 
                min=2100, max=3000, value=2400),
              sliderInput("val", label=h4("Initial Population:"), 
                min=1e9, max=10e9, step=1e9, value=3e9),
              sliderInput("val2", label=h4("Initial Resources for Survival:"), 
                min=1e9, max=30e9, value=10e9),
              sliderInput("val2R", label=h4("Rate of Initial Regulation:"), 
                min=0, max=.05, value=0),
              sliderInput("val3", label=h4("New Resources for Survival:"), 
                min=1e9, max=30e9, value=25e9),
              sliderInput("val3R", label=h4("Rate of New Regulation:"), 
                min=0, max=.05, value=.045),
              sliderInput("val4", label=h4("Birth Rate:"), 
                min=.01, max=.07, step=.01, value=.04),
              sliderInput("val5", label=h4("Death Rate:"), 
                min=.01, max=.07, step=.01, value=.02),
              sliderInput("val6", label=h4("Amount of Years till New Resource Found:"), 
                min=1, max=1000, value=100)
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("population.plot",width="100%",height="600px")
            ))
        ),
        tabPanel("Bathtub",
            sidebarLayout(sidebarPanel(
                numericInput("bathtubSimLength", "Simulation time (min)",
                    value=5, min=0, step=.5, width="40%"),
                sliderInput("initWaterLevel", "Initial water level (gal)",
                    min=0, max=60, value=50),

                # (Putting the code for these into server.R is necessary so
                # their width can vary dynamically dependent on the
                # bathtubSimLength widget's value.)
                uiOutput("faucetOnOffWidget"),
                uiOutput("pullPlugTimeWidget"),

                div(class="container-fluid",
                    div(class="row",
                        div(class="col-lg-6", 
                            numericInput("inflowRate", "Faucet rate (gal/min)",
                                value=15, min=0, step=1)),
                        div(class="col-lg-6", 
                            numericInput("outflowRate", "Drain rate (gal/min)",
                                value=20, min=0, step=1))
                    )
                )),
                mainPanel(
                    plotOutput("bathtubWaterLevelPlot")
                )
            )
        ),
        tabPanel("Caffeine",
            sidebarLayout(sidebarPanel(
                numericInput("caffeineSimLength", "Simulation time (hrs)",
                    value=5*24, min=0, step=1, width="40%"),
                sliderInput("initStoredEnergy", "Initial stored energy (kcal)",
                    min=0, max=1e6, step=1e4, value=.5e6),
                sliderInput("initAvailableEnergy", 
                    "Initial available energy (kcal)",
                    min=0, max=2e6, step=1e4, value=1e5),
                sliderInput("lowExpenditureLevel", 
                    "Energy expenditure at rest (kcal/hr)",
                    min=0, max=5000, step=100, value=2000),
                sliderInput("highExpenditureLevel", 
                    "Energy expenditure at work (kcal/hr)",
                    min=0, max=5000, step=100, value=3000),
                sliderInput("baselineMetabolizationRate", 
                    "Baseline energy metabolization rate (kcal/hr)",
                    min=0, max=5000, step=100, value=2300),
                sliderInput("desiredAvailableEnergy", 
                    "Desired available energy (kcal)",
                    min=0, max=5e4, step=100, value=1.5e4)
            ),
            mainPanel(
                plotOutput("caffeinePlot")
            ))
        ),
        tabPanel("Coffee",
            sidebarLayout(sidebarPanel(
                numericInput("coffeeSimLength", "Simulation time (mins)",
                    value=5*24, min=0, step=1, width="40%"),
                div(class="container-fluid",
                    div(class="row",
                        div(class="col-lg-6", 
                            actionButton("runCoffeeSim",label="Start/restart")),
                        div(class="col-lg-6", 
                            actionButton("contCoffeeSim",label="Continue"))
                    )
                ),
                div(
                    h4("Initial conditions"),
                    radioButtons("mugType","Mug type",
                        choices=list("7-11 (non-insulated)"="7-11",
                            "Contiga (insulated)"="Contiga"),
                        inline=TRUE),
                    sliderInput("initCoffeeTemp", "Initial temp (°F)",
                        min=20, max=220, step=1, value=192)
                ),
                div(
                    h4("Sim parameters"),
                    sliderInput("roomTemp", "Room temp (°F)",
                        min=40, max=100, step=1, value=72)
                )
            ),
            mainPanel(
                plotOutput("coffeePlot")
            ))
        ),
        tabPanel("Interest",
            sidebarLayout(sidebarPanel(
                numericInput("interestSimLength", "Simulation time (months)",
                    value=5*12, min=0, step=1, width="40%"),
                div(
                    h4("Initial conditions"),
                    sliderInput("initBalance", "Initial balance ($)",
                        min=0, max=10e4, step=100, value=1e4)
                ),
                div(
                    h4("Sim parameters"),
                    sliderInput("interestRate", "Annual interest rate",
                        min=0, max=.4, step=.01, value=.01),
                    sliderInput("amortizationPeriod",
                        "Amortization period (months)",
                        min=1/30, max=12, step=1/30, value=1)
                )
            ),
            mainPanel(
                plotOutput("interestPlot")
            ))
        ),
        tabPanel("Reinvestment",
            sidebarLayout(sidebarPanel(
                numericInput("reinvestmentSimLength", "Simulation time (yrs)",
                    value=10, min=0, step=1, width="40%"),
                div(class="container-fluid",
                    div(class="row",
                        div(class="col-lg-6", 
                            actionButton("runReinvestmentSim",
                                label="Start/restart")),
                        div(class="col-lg-6", 
                            actionButton("contReinvestmentSim",
                                label="Continue"))
                    )
                ),
                div(
                    h4("Initial conditions"),
                    sliderInput("initialCapital", "Initial capital ($)",
                        min=1e3, max=1e5, step=1e3, value=1e4)
                ),
                div(
                    h4("Sim parameters"),
                    sliderInput("fracOutputInvested",
                        "Fraction of output reinvested",
                        min=0, max=1, step=.025, value=.2)
                )
            ),
            mainPanel(
                plotOutput("reinvestmentPlot")
            ))
        ),
        tabPanel("CPSC",
            sidebarLayout(sidebarPanel(
                numericInput("cpscLength", "Simulation time (years)",
                    value=20, min=1, step=1, width="40%"),
                div(class="container-fluid",
                    div(class="row",
                        div(class="col-lg-6", 
                            actionButton("runCpscSim",label="Start/restart")),
                        div(class="col-lg-6", 
                            actionButton("contCpscSim",label="Continue"))
                    )
                ),
                div(
                    h4("Sim parameters"),
                    sliderInput("economy", "Economy GDP ($ trillions)",
                        min=1, max=30, step=1, value=16),
                    sliderInput("rigor", "CPSC curriculum rigor",
                        min=0, max=1, step=.05, value=.5),
                    sliderInput("admissions", "Admissions policy",
                        min=0, max=1, step=.05, value=.5)
                )
            ),
            mainPanel(
                plotOutput("cpscPlot")
            ))
        ),
        tabPanel("BatsMice",
            sidebarLayout(sidebarPanel(
                numericInput("batMouseSimLength", "Simulation time (months)",
                    value=120, min=0, step=1, width="40%"),
                div(class="row",
                    div(class="col-lg-6", 
                        sliderInput("batBirthRate",
                            "Bat birth rate",
                            value=1.2, min=0, step=.1, max=3)
                    ),
                    div(class="col-lg-6", 
                        sliderInput("mouseBirthRate",
                            "Mouse birth rate",
                            value=1.2, min=0, step=.1, max=3)
                    )
                ),
                div(class="row",
                    div(class="col-lg-6", 
                        sliderInput("batDeathRate",
                            "Bat death rate",
                            value=1.5, min=0, step=.1, max=3)
                    ),
                    div(class="col-lg-6", 
                        sliderInput("mouseDeathRate",
                            "Mouse death rate",
                            value=1.1, min=0, step=.1, max=3)
                    )
                ),
                sliderInput("nutritionFactor",
                    "Nutrition factor (bats/kill)",
                    value=2, min=0, step=.1, max=5),
                sliderInput("killRatio",
                    "Kill ratio (kills/encounter)",
                    value=.05, min=0, step=.05, max=1)
            ),
            mainPanel(
                plotOutput("batsMiceTimePlot")
            ))
        ),
        tabPanel("SIR",
            sidebarLayout(sidebarPanel(
                numericInput("sirSimLength", "Simulation time (days)",
                    value=30*1, min=0, step=1, width="40%"),
                sliderInput("infectionRate",
                    "Infection rate (1/(infectedPerson*day))",
                    value=.002, min=0, step=.0005, max=.01),
                sliderInput("meanDiseaseDuration",
                    "Mean disease duration (days)",
                    value=2, min=0, step=1, max=20),
                sliderInput("initS",
                    "Initial susceptible population (people)",
                    value=780, min=0, step=10, max=2000),
                htmlOutput("reproductiveNumber")
            ),
            mainPanel(
                plotOutput("sirPlot")
            ))
        ),       
        tabPanel("Escalation",
            sidebarLayout(sidebarPanel(
                numericInput("escalationLength", "Simulation time(years)",
                    value=20, min=1, step=1, width="40%"),
                div(class="container-fluid",
                    div(class="row",
                        div(class="col-lg-6", 
                            actionButton("runEscalationSim",label="Start/restart")),
                        div(class="col-lg-6", 
                            actionButton("contEscalationSim",label="Continue"))
                    )
                ),
                div(
                    h4("Sim parameters"),
                    sliderInput("usaPerceptionBias", "USA overestimation of USSR stockpile",
                        min=0, max=1, step=.05, value=.2),
                    sliderInput("ussrPerceptionBias", "USSR overestimation of USA stockpile",
                        min=0, max=1, step=.05, value=.2),
                    sliderInput("usaDesiredAdvantage", "USA desired advantage",
                        min=0, max=5, step=.05, value=.2),
                    sliderInput("ussrDesiredAdvantage", "USSR desired advantage",
                        min=0, max=5, step=.05, value=.2),
                    sliderInput("usaCorrectionPeriod", "USA target correction period (years)",
                        min=0, max=5, step=.05, value=1),
                    sliderInput("ussrCorrectionPeriod", "USSR target correction period (years)",
                        min=0, max=5, step=.05, value=1)
                )
            ),
            mainPanel(
                plotOutput("escalationPlot")
            ))
        )

    )
))

