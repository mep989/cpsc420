
library(shiny)

source("../sd/bathtub.R")
source("../sd/caffeine.R")
source("../sd/coffee.R")
source("../sd/contiga.R")
source("../sd/interest.R")
source("../sd/cpscStudents.R")
source("../sd/reinvestment.R")
source("../sd/batsMice.R")
source("../sd/SIR.R")
source("../sd/escalation.R")
source("../sd/toyWars.R")
source("../sd/cows.R")
source("../sd/commonGoodShiny-elias.R")
source("../sd/cowsTragedy.R")
source("../sd/zombie.R")
source("../sd/smuggling.R")
source("../sd/TofCommons.R")

shinyServer(function(input,output,session) {

  
  ############# Bathtub ###################################################

    output$bathtubWaterLevelPlot <- renderPlot({
        if (is.null(input$faucetOnOff)) {
            return(NULL)
        }
        sim.results <- bathtub.sim(init.water.level.gal=input$initWaterLevel,
            turn.on.faucet.time=input$faucetOnOff[1]*60,
            turn.off.faucet.time=input$faucetOnOff[2]*60,
            pull.plug.time=input$pullPlugTime*60,
            inflow.rate=input$inflowRate/60,
            outflow.rate=input$outflowRate/60,
            sim.length=input$bathtubSimLength)
        plot.bathtub.water.level(sim.results)
    })

    output$faucetOnOffWidget <- renderUI({
        sliderInput("faucetOnOff", "Turn on/off faucet time (min)",
            min=0, max=input$bathtubSimLength, step=.1, value=c(1,4))
    })

    output$pullPlugTimeWidget <- renderUI({
        sliderInput("pullPlugTime", "Pull plug time (min)",
            min=0, max=input$bathtubSimLength, step=.1, value=2)
    })


    ############# Caffeine ###################################################

    output$caffeinePlot <- renderPlot({
        sim.results <- caffeine.sim(init.stored.energy=input$initStoredEnergy,
            init.available.energy=input$initAvailableEnergy,
            low.expenditure.level=input$lowExpenditureLevel,
            high.expenditure.level=input$highExpenditureLevel,
            baseline.metabolization.rate=input$baselineMetabolizationRate,
            desired.available.energy=input$desiredAvailableEnergy,
            sim.length=input$caffeineSimLength)
        plot.energy(sim.results)
    })


    ############# Coffee #####################################################
    observeEvent(input$runCoffeeSim,
    {
        prev.coffee.results <<- NULL
        output$coffeePlot <- renderPlot({
            run.and.plot.coffee()
        })
    })

    observeEvent(input$contCoffeeSim,
    {
        output$coffeePlot <- renderPlot({
            run.and.plot.coffee()
        })
    })

    run.and.plot.coffee <- function() {
        isolate({
            if (input$mugType == "Contiga") {
                sim.func <- contiga.sim
                description <- "swanky Contiga mug"
            } else {
                sim.func <- coffee.sim
                description <- "sucky 7-11 mug"
            } 
            prev.coffee.results <<- 
                sim.func(init.coffee.temp=input$initCoffeeTemp,
                    room.temp=input$roomTemp,
                    sim.length=input$coffeeSimLength,
                    prev.results=prev.coffee.results)
            plot.coffee(prev.coffee.results, description)
        })
    }



    ############# Interest ##################################################

    output$interestPlot <- renderPlot({
        sim.results <- interest.sim(init.balance=input$initBalance,
            annual.interest.rate=input$interestRate,
            amortize.period=input$amortizationPeriod,
            sim.length=input$interestSimLength)

        amor.per <- as.numeric(input$amortizationPeriod)
        if (isTRUE(all.equal(amor.per, 1/30))) {
            plot.period.desc <- "daily"
        } else if (isTRUE(all.equal(amor.per, 1))) {
            plot.period.desc <- "monthly"
        } else if (isTRUE(all.equal(amor.per, 12))) {
            plot.period.desc <- "yearly"
        } else {
            plot.period.desc <- paste("every",
                round(input$amortizationPeriod,1), "months")
        }
        plot.interest(sim.results, paste0(100 * input$interestRate, "%"),
            plot.period.desc)
    })


    ############# Reinvestment ##############################################

    observeEvent(input$runReinvestmentSim,
    {
        prev.reinvestment.results <<- NULL
        output$reinvestmentPlot <- renderPlot({
            run.and.plot.reinvestment()
        })
    })

    observeEvent(input$contReinvestmentSim,
    {
        output$reinvestmentPlot <- renderPlot({
            run.and.plot.reinvestment()
        })
    })

    run.and.plot.reinvestment <- function() {
        isolate({
            prev.reinvestment.results <<- 
                reinvestment.sim(init.capital=input$initialCapital,
                    fraction.of.output.invested=input$fracOutputInvested,
                    sim.length=input$reinvestmentSimLength,
                    prev.results=prev.reinvestment.results)
            plot.reinvestment(prev.reinvestment.results)
        })
    }


    ############## CPSC students ############################################

    observeEvent(input$runCpscSim,
    {
        prev.cpsc.results <<- NULL
        output$cpscPlot <- renderPlot({
            run.and.plot.cpsc()
        })
    })

    observeEvent(input$contCpscSim,
    {
        output$cpscPlot <- renderPlot({
            run.and.plot.cpsc()
        })
    })

    run.and.plot.cpsc <- function() {
        isolate({
            prev.cpsc.results <<- 
                cpsc.students.sim(economy=input$economy,
                    admissions.policy=input$admissions,
                    CPSC.curric.rigor=input$rigor,
                    sim.length=input$cpscLength,
                    prev.results=prev.cpsc.results)
            plot.cpsc(prev.cpsc.results)
        })
    }


    ############## Bats and mice ###########################################

    output$batsMiceTimePlot <- renderPlot({
      sim.results <- bats.mice.sim(
        bat.birth.rate=input$batBirthRate,
        bat.death.rate=input$batDeathRate,
        mouse.birth.rate=input$mouseBirthRate,
        mouse.death.rate=input$mouseDeathRate,
        nutrition.factor=input$nutritionFactor,
        kill.ratio=input$killRatio,
        sim.length=input$batMouseSimLength)
      plot.bats.mice.time.plot(sim.results)
    })

    ################### SIR ################################################

    output$sirPlot <- renderPlot({
        sim.results <- sir.sim(
            infection.rate=input$infectionRate,
            mean.disease.duration=input$meanDiseaseDuration,
            init.S=input$initS,
            sim.length=input$sirSimLength)
        plot.sir(sim.results)
    })

    output$reproductiveNumber <- renderText({
        R0 <- calculate.reproductive.number(input$infectionRate,
            input$meanDiseaseDuration, input$initS)
        if (R0 <= 1) {
            R0.color <- "darkgreen"
        } else {
            R0.color <- "red"
        }
        paste("<div style=\"text-align:center;\">",
            "Basic Reproductive Number: <span style=\"color:", R0.color,
                ";font-weight:bold;\">", round(R0,2), "</span></div>")
    })
  
  output$zombiePlot <- renderPlot({
    results <- zombie.sim(
      dullness.rate = input$dullnessRate, 
      damage.rate = input$zombieDamageRate,
      walkers.per.day.wall1 = input$zombiesWall1,
      kill.rate.wall1 = input$guardOneKillRate,  
      rebuild.rate.wall1 = input$wallOneRebuildRate,
      walkers.per.day.wall2 = input$zombiesWall2, 
      kill.rate.wall2 = input$guardTwoKillRate,  
      rebuild.rate.wall2 = input$wallTwoRebuildRate, 
      walkers.per.day.wall3 = input$zombiesWall3,
      kill.rate.wall3 = input$guardThreeKillRate,  
      rebuild.rate.wall3 = input$wallThreeRebuildRate, 
      walkers.per.day.wall4 = input$zombiesWall4,
      kill.rate.wall4 = input$guardFourKillRate,  
      rebuild.rate.wall4 = input$wallFourRebuildRate,
      sim.time = input$simLength)
    plot.zombie(results)
    #plot.zombie.kills(results)
  })

	################### Cows ###########################################
	output$Plot <- renderPlot({
		sim.results <- cows.sim(
			del=input$deltat,
	      		acow=input$acow,
	      		bcow=input$bcow,
			      length=input$time,
	      		ae=input$Aearnings,
	      		be=input$Bearnings,
	      		blade=input$grass,
	      		cow.eat.rate=input$coweatrate,
	      		grass.growth.rate=input$grassgrowth,
	      		cow.cost <- input$cowcost,
	      		A.reinvestment <- input$a,
	      		B.reinvestment <- input$b,
	      		cow.profit <- input$profit,
	      		blades.per.cow <- input$blades,
	      		birth=input$birth)
		plot.cows.sim(sim.results, input$y)
	})


	############################### Toy Wars ###########################################
    output$toysTimePlot <- renderPlot({
      	toy.results <- toy.sim(
        sim.length=input$toySimLength, 
        brag.rate.child1=input$Child1BragRate,
        brag.rate.child2=input$Child2BragRate,
        win.rate.child1=input$Child1WinRate,
        win.rate.child2=input$Child2WinRate, 
        allowance=input$childAllowance,
        cost.of.toy=input$toyCost,
        child1.start=input$Child1Start,
        child2.start=input$Child2Start,
        piggy1.start=input$Child1Piggy,
        piggy2.start=input$Child2Piggy)
      plot.toys.time.plot(toy.results)
    })

    
    ############ Common Good Email - ELias Ingea ##########################################
    
    output$commonGoodPlot <- renderPlot({
      sim.results <- common.sim(
        spam.percentage=input$spamPercentage, 
        regen.rate=input$inflowRate, 
        rate.of.use=input$outflowRate, 
        sim.length=input$emailsSimLength)
      plot.common.good(sim.results)
    })
    
  ##################### Cows Tragedy of THe Commons ########################
    
    output$cowsTragedyPlot <- renderPlot({
      sim.results <- cows.tragedy.sim(
        sim.length=input$time,
        farmers=input$farmersOnPasture,
        init.cows=input$initCowsPerFarmer,
        init.food=input$startingGrass,
        grass.growth.rate=input$grassRegrowthRate,
        decision.ratio=input$sustainabilityDecisionRatio)
      plot.cows.tragedy.plot(sim.results)
    })


    ################# escalation ############################################
    observeEvent(input$runEscalationSim,
        {
            prev.escalation.results <<- NULL
            output$escalationPlot <- renderPlot({
                run.and.plot.escalation()
            })
        })

        observeEvent(input$contEscalationSim,
        {
            output$escalationPlot <- renderPlot({
                run.and.plot.escalation()
            })
        })

        run.and.plot.escalation <- function() {
            isolate({
                prev.escalation.results <<- 
                    escalation.sim(usa.perception.bias=input$usaPerceptionBias,
                        ussr.perception.bias=input$ussrPerceptionBias,
                        usa.desired.advantage=input$usaDesiredAdvantage,
                        ussr.desired.advantage=input$ussrDesiredAdvantage,
                        usa.correction.period=input$usaCorrectionPeriod,
                        ussr.correction.period=input$ussrCorrectionPeriod,
                        sim.length=input$escalationLength,
                        prev.results=prev.escalation.results)
                plot.escalation(prev.escalation.results)
            })
    }




    ############## Han Solo Smuggling ###########################################
    
    output$plot.contraband.plot <- renderPlot({
        sim.results <- han.solo.sim(
          init.patrols=input$initPatrols,
          init.contraband=input$initContraband,
          demand=input$demand,
          encounter.frequency=input$encounterFrequency,
          sim.length=input$simLength,
          contract.incentive=input$contractIncentive,
          smuggle.rate=input$smuggleRate,
          patrol.rate=input$patrolRate,
          confiscation.ratio=input$confiscationRatio,
          desired.shipments=input$desiredShipments
        )
        plot.contraband.plot(sim.results)
    })


    ############## Population and Tragedy of Commons #############################

    output$population.plot <- renderPlot({
      sim.results <- tOfCommons.sim(
        init.population=(as.numeric(input$val)), 
        init.resources=(as.numeric(input$val2)),
        init.new.resources=(as.numeric(input$val3)),
        birth.rate=(as.numeric(input$val4)),
        death.rate=(as.numeric(input$val5)),
        year.new.resources=(as.numeric(input$val6)),
        year.start=(as.numeric(input$valS)),
        year.end=(as.numeric(input$valE)),
        regulation.rate.R=(as.numeric(input$val2R)),
        regulation.rate.N=(as.numeric(input$val3R)))
      plot.TOfCommons.sim(sim.results)
    })
    
})
