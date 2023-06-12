;I am addign code to calcualte R0
; Time varying R represents the expected number of secondary local cases arising from a primary case infected at time t
extensions [matrix]
undirected-link-breed [lt-links lt-link]
undirected-link-breed [st-links st-link]

turtles-own
[
  ;Person related variables
  sex
  sexual-preference
  age
  age-days
  vaccinated?
  new_partners_per_year
  age-cohort ; from 0 to 15 , min age 16, max age 75
  brokenUp? ; set to true if just borken up and has to find new immediately



  disease-status
  days_sick
  days_exposed
  incubation_period
  sick-period

  ;transmission related (contact tracing) variables
  day_I_got_sick
  day_infector_got_sick
  numinfected
  infected_by
  isTravelCase

]
globals
[
  ToRecoverToday
  age-mix-agg
  lambda-hetero
  lambda-homo
  lambda-bi-same-sex
  lambda-bi-opp-sex
  bi_prob_same_sex
  total_population
  min-age
  max-age


  mean_infectious_period
  weekly_contact
]
to set-global-vars
  set min-age 16
  set max-age 74

  set weekly_contact 3
  set mean_infectious_period 21
end

to load-age-mix ; all categories, male, females, hetero, homo, bi
  ; 0 row is age-cohor 16-19
  ; 1 row is age-cohort 20-23 and so on
  set age-mix-agg matrix:from-row-list [
 [	0.747831829	0.177305409	0.049992136	0.014708313	0.007019141	0.001151133	0.001095006	0.00041946	0.000476774	0	0	0	0	0	0	]
[	0.231047208	0.490546778	0.185653325	0.057908237	0.022356877	0.007148993	0.003917986	0.001317601	0	0.000102724	0	0	0	0	0	]
[	0.127933614	0.21435767	0.4001897	0.14443666	0.078122539	0.019691981	0.009961396	0.002309779	0.002996109	0	0	0	0	0	0	]
[	0.076001244	0.16038241	0.280802333	0.262387001	0.116114574	0.034741706	0.037985451	0.021671947	0.003505733	0.000852436	0.005555166	0	0	0	0	]
[	0.040796399	0.122034949	0.156768675	0.249481921	0.192635573	0.106408415	0.071892102	0.038002785	0.011678285	0.003363232	0.00496565	0.001972014	0	0	0	]
[	0.046525192	0.10283495	0.130889686	0.115870047	0.192866939	0.161486445	0.133942873	0.064003387	0.024135176	0.019019923	0.003787471	0.002642426	0	0	0.002660646	]
[	0.006346823	0.019760219	0.03678893	0.064084986	0.117526889	0.210149343	0.247059086	0.173584578	0.07977703	0.023804868	0.016404244	0	0.002626774	0	0.002781639	]
[	0.012645278	0.01132567	0.075765226	0.07082388	0.13859355	0.117767443	0.180830415	0.22852376	0.108886271	0.021504307	0.0333342	0	0	0	0	]
[	0.026439486	0.042959568	0.036182116	0.03214682	0.046332086	0.071440982	0.208081498	0.20870304	0.193550598	0.086363269	0.027714186	0.020086352	0	0	0	]
[	0	0	0	0	0.077041259	0.12508842	0.160885877	0.045030504	0.225861297	0.137573529	0.178896382	0.042103777	0.007518954	0	0	]
[	0	0	0.061103034	0.050863023	0.032146605	0.063440383	0.188836193	0.055444896	0.164926323	0.070105275	0.172241693	0.09067874	0.021798034	0.028415801	0	]
[	0	0	0	0.150701142	0.018506024	0.019569801	0.063931694	0.058109094	0.211889124	0.16732933	0.087347778	0.125179978	0.097436034	0	0	]
[	0.045594308	0	0.026424301	0.052848602	0	0	0.023079258	0.023580197	0	0.081017046	0.0779076	0.351577669	0.27334111	0.024811683	0.026424301	]
[	0	0	0	0	0	0.064995283	0	0.066197868	0.115064423	0	0	0.073991064	0.286523556	0.393227806	0	]
[	0	0	0	0	0	0	0	0	0	0	0	0	0.324660919	0.283416828	0.522563003	]




  ]

end
to load-lambda

  set lambda-hetero matrix:from-row-list  [
[	  1.5916	1.1664	]
[		1.418	0.7995	]
[		0.8445	0.4307	]
[		0.6163	0.4064	]
[		0.5411	0.2946	]
[		0.3972	0.1787	]
[		0.2796	0.226	]
[		0.3053	0.1435	]
[		0.3566	0.1818	]
[		0.2299	0.1191	]
[		0.2379	0.07	]
[		0.2239	0.047	]
[		0.1136	0.0361	]
[		0.0645	0.0065	]
[		0.0496	0.0339	]


  ]

  set lambda-homo matrix:from-row-list ; replace NAN by 0
  [[	1.7	1.7	]
[	1.3	0.75	]
[	0.625	0.4286	]
[	3	0	]
[	7.3333	0	]
[	4.6667	0	]
[	4.5	0	]
[	6.6667	0	]
[	0	1	]
[	0	0	]
[	1.3333	0	]
[	0	0	]
[	0	0	]
[	0	0	]
[	5	0	]  ]


  set lambda-bi-same-sex matrix:from-row-list
  [[	0.9375	0.4444	]
[	1.5238	0.5679	]
[	0.6522	0.1667	]
[	2.9231	0.1789	]
[	1.05	0.1167	]
[	9.3636	0.3077	]
[	0.2727	0.1212	]
[	1.375	0.2059	]
[	0.2143	0.0714	]
[	0.0588	0.2105	]
[	0.7368	0.1	]
[	0.5417	0	]
[	0	0	]
[	0.25	0	]
[	0	0	]
]

  set lambda-bi-opp-sex matrix:from-row-list
  [
    [0.6250    1.4889]
    [0.9524    1.5926]
    [ 1.1304    0.7619]
    [0.3077    0.6526]
    [1.0500    0.5833]
    [0.8182    0.5000]
    [2.0000    4.9091]
    [0.5000    0.7647]
    [0.2143    0.2143]
    [0.2941    0.1053]
    [0.2632         0]
    [0.5417    0.1818]
    [    0    0.4286]
    [0.1250         0]
    [    0       0]
  ]

  set bi_prob_same_sex matrix:from-row-list
  [

    [ 0.6000    0.2299]
    [0.6154    0.2629]
    [0.3659    0.1795]
    [0.9048    0.2152]
    [0.5000    0.1667]
    [0.9196    0.3810]
    [0.1200    0.0241]
    [0.7333    0.2121]
    [0.5000    0.2500]
    [0.1667    0.6667]
    [0.7368    1.0000]
    [0.5000         0]
    [ 0.5         0]
    [0.6667       0.5]
    [0.5       0.5] ; changing some NAN to 0.5



  ]



end


to create-population

;set population  + heterosexual_females + homosexual_males + homosexual_females + bi_males + bi_females
create-turtles population
  [
    set numinfected 0
    set infected_by nobody
    set brokenUp? FALSE
    set age min-age + (random (max-age + 1 - min-age))
    set age-days 1 + random 365
    set age-cohort cohort-from-age age
    set vaccinated? FALSE
     set sexual-preference 1; to be updated later 1 for hetero, 2 for homo and 3 for bi
    setxy random-xcor random-ycor

    ifelse random-float 1 <  0.5;0.4150508 ; proportions taken from NATSAL data
    [
      set sex 1 ; 1 for male, 0 for female

    ]
    [
      set sex 2
    ]

   set-disease-status(1)
  ]

  let  homo_males   round ((%homosexual_males / 100) * count turtles with [sex = 1])
  ask n-of homo_males turtles  with [sex = 1]
   [
    set sexual-preference 2
   ]
   let homo_females round ((%homosexual_females / 100) * count turtles with [sex = 2])
  ask n-of homo_females turtles  with [sex = 2]
   [
    set sexual-preference 2
   ]

   let bi_males round ((%bi_males / 100) * count turtles with [sex = 1 and sexual-preference != 2])
  ask n-of bi_males turtles  with [sex = 1 and sexual-preference != 2]
   [
    set sexual-preference 3
   ]

    let bi_females round ((%bi_females / 100) * count turtles with [sex = 2 and sexual-preference != 2])
  ask n-of bi_females turtles  with [sex = 2  and sexual-preference != 2]
   [
    set sexual-preference 3
   ]


  ask turtles
  [

   update-lambda



    ]



end

to SuperSpreaderEvent ; function to model random events where intimate contact increases
;  output-print "party today"
 ; let total_partygoers count turtles with [partygoer? = TRUE]
  let partypeople1 no-turtles
  let n count turtles with [sex = 1 and sexual-preference > 1 and age-cohort >= 2 and age-cohort <= 5 ]
  let p 35
  if (n < p)
  [set p n]
  set partypeople1 n-of p turtles with [sex = 1 and sexual-preference > 1 and age-cohort >= 2 and age-cohort <= 5 ]
;output-print p
;  output-print "people going to party today"
  let partypeople2 no-turtles
 ; set partypeople2 n-of 15 turtles with [sex = 2 and age-cohort >= 2 and age-cohort <= 5 ]

  let partypeople3 no-turtles
  ;set partypeople3 n-of 10 turtles with [sex = 1 and age-cohort >= 2 and age-cohort <= 5 and sexual-preference = 1]


    let joinset (turtle-set partypeople1 partypeople2 partypeople3)

  ask joinset with [disease-status = 3]
  [

    create-st-links-with other joinset


  ]
  ask st-links
  [
    set color pink
   ; stop
  ]
  ;stop
end
to party-end

  ask st-links
  [die]
end
to-report cohort-from-age [my-age]
  let  cohort floor ((my-age - 16) / 4)
  report cohort
end

to go


  go-age
  ask turtles
  [
    change-relationship-status
  ]
  ask turtles with [brokenUp? = TRUE]
  [
    get-partners

  ]

  if (SuperSpreaderEventsPerYear > 0)
  [
    if (ticks mod (floor ( 364 / SuperSpreaderEventsPerYear)) = 0)
  [
      SuperSpreaderEvent
  ]
  ]

   if (ImportedCasesPerYear > 0)
  [
    if (ticks mod (floor ( 364 / ImportedCasesPerYear)) = 0)
  [
    TRAVEL
  ]
  ]

  SEIR

  party-end

  if ticks >= 180
  [
    print CalculateR
    stop

  ]
  tick
end


to TRAVEL


    if any? turtles with [age-cohort >= 2 and age-cohort <= 5 and sex = 1 and sexual-preference > 1]
    [
      output-print "New Travel Related Case"

      ask one-of turtles with [age-cohort >= 2 and age-cohort <= 5  and sex = 1 and sexual-preference > 1]
      [
        if (disease-status = 1 and vaccinated? = FALSE)

        [
        set day_Infector_got_sick ticks

          set-disease-status(2)
        set isTravelCase TRUE
         ; exposed after travel]
        ]

      ]



  ]


end
to stay-sick
  set days_sick days_sick + 1
end

to contract-disease-normal [digs mysex infector ] ; digs = day infector got sick
   let prb 1
      ifelse vaccinated? = FALSE
      [

        set prb  Transmission_probability * (weekly_contact / 7) ; assuming a couple comes in contact 5 times a week

      ];
      [

        set prb 0
      ]


      if mysex = 1 and sex  = 1
      [
        set prb prb * TP_multiplier_for_msm ; increase transmission in msm
      ]
      let pr1 random-float 1

        if pr1 < prb
        [

    set day_infector_got_sick digs
   set infected_by infector
    set-disease-status(2)
        ]
end

to contract-disease-through-event [digs mysex infector]
  let prb 1
      ifelse vaccinated? = FALSE
      [
         set prb  Transmission_probability
      ]
      [
       set prb 0
      ]

      if mysex = 1 and sex  = 1
      [
        set prb prb * TP_multiplier_for_msm ; increase transmission in msm

      ]


      let pr1 random-float 1
     ;output-print pr1
        if pr1 < prb
        [

    set infected_by infector
    set day_infector_got_sick digs
          set-disease-status(2)
        ]
end
to SEIR


  set ToRecoverToday count turtles with [disease-status = 3 and  days_sick > sick-period]
  print toRecoverToday
  ask turtles with [disease-status = 3]
  [
    let ds day_I_got_sick

    let mysex sex
    let infector self
    ask lt-link-neighbors with [disease-status = 1] ; only susceptible to be exposed
    [
     contract-disease-normal ds sex infector
    ]
    ask st-link-neighbors with [disease-status = 1] ; only susceptible to be exposed
    [

     contract-disease-through-event ds sex infector
    ]


    ifelse days_sick <= sick-period
    [
      stay-sick
    ]
    [
      set-disease-status (4)
    ]

  ]

  ask turtles with [disease-status = 2]
  [
    ifelse days_exposed <= incubation_period
    [
      stay-exposed
    ]
    [

      set-disease-status (3)
    ]

  ]


end


to stay-exposed
  set days_exposed days_exposed + 1
end
to expose


end
to update-lambda ; turtle procedure

  let cohort  cohort-from-age age
  let lambda 0

  if  sexual-preference = 1
    [
      set lambda matrix:get lambda-hetero cohort (sex - 1)

    ]
   if  sexual-preference = 2
    [
      set lambda matrix:get lambda-homo cohort (sex - 1)
      ]
   if  sexual-preference = 3
    [
      set lambda matrix:get lambda-bi-opp-sex cohort (sex - 1) + matrix:get lambda-bi-same-sex cohort (sex - 1)
    ]
  set new_partners_per_year random-poisson lambda


end



to change-relationship-status ; basically break up first

 let prob 1 - exp( -1 * ((new_partners_per_year / 365) + (0.1 / 365) )) ; daily probability


 if random-float  1 < prob
  [

    breakup
    set brokenUp? TRUE


  ]



end

to breakup
  if count my-links > 0
[
  ask my-lt-links
  [

    die

  ]
  ]
end
to setup
  clear-all
  reset-ticks
  set-global-vars
  load-lambda
  load-age-mix
  create-population

  initial-infection
 inital-relationships

    initial-vaccination


end
to initial-vaccination

    let really_vaccinated People_vaccinated
  if (Vaccination_Programme = "None")
  [

    set People_vaccinated 0
    ; do nothing.
    stop
  ]
  if (Vaccination_Programme = "RandomAll")
  [
    set really_vaccinated round (vaccine_efficacy * People_vaccinated)
  ask n-of really_vaccinated turtles with [disease-status = 1] ; leave already infected alone
  [
    set vaccinated? TRUE
    set color green

  ]
  ]

  if (Vaccination_Programme = "RandomMSM")
  [
    let n count turtles with [sex = 1 and sexual-preference > 1]

    if n < People_vaccinated
    [
      set People_Vaccinated n
    ]

    set really_vaccinated round (vaccine_efficacy * People_vaccinated)
   ask n-of really_vaccinated turtles with [disease-status = 1 and sex = 1 and sexual-preference > 1] ; leave already infected alone
  [
    set vaccinated? TRUE
    set color green

  ]
  ]


  if (Vaccination_Programme = "MSM_C1_C6")
  [
    let n count turtles with [sex = 1 and sexual-preference > 1 and age-cohort > 0 and age-cohort <= 6]

    if n < People_vaccinated
    [
      set People_Vaccinated n
    ]
   ask n-of People_vaccinated turtles with [disease-status = 1 and sex = 1 and sexual-preference > 1] ; leave already infected alone
  [
    set vaccinated? TRUE
    set color green

  ]
  ]
end
to set-disease-status [status]
  set disease-status status
  if disease-status  = 1
  [
    set color yellow
    get-susceptible



  ]

  if disease-status  = 2
  [
    set color orange
    get-exposed


  ]
   if disease-status  = 3
    [
      set color red
      get-sick
  ]
  if disease-status  = 4
    [
      set color blue ; recovered
  ]


end

to initial-infection
  ask n-of initially_infected_MSM  turtles with [age-cohort >= 2 and age-cohort <= 5 and sex = 1 and sexual-preference > 1]
  [

    set-disease-status 3;
    set infected_by nobody

  ]
   ask n-of initially_infected_others turtles
  [

    set-disease-status 3;
  ]
end

to get-susceptible

    set days_sick 0
    set days_exposed 0
    set sick-period 0
    set incubation_period 0
end


to get-exposed

  set  incubation_period   (round (random-weibull  1.5 8.4))  ;
  if incubation_period < 6 ; randomly chosen
  [
    set incubation_period incubation_period - random 4  ; can be infectious upto 4 days before appearance of symptoms, only doing this if incubation period is big enough

  ]
    set days_exposed 1
  set days_sick 0

end

to-report random-weibull [shp scl]

  let result round ( scl * (-1 * ln (random-float 1)) ^ (1 / shp))

  report result
end


to get-sick

   set days_sick  1
   set days_exposed 0
    set sick-period   round(random-normal mean_infectious_period 2)   ; 2 - 4 weeks
  set day_I_got_sick ticks;
  if ticks != 0
[
;     file-open "serial_intervals_ran4.txt"
;    file-write ser_int
;    file-close
;
;
;     file-open "incubs_ran4.txt"
;    file-write incubation_period
;    file-close
;
;      file-open "sickss_ran4.txt"
;    file-write sick-period
;    file-close
;
;       file-open "infector_Infectee_ran4.txt"
;    file-type day_infector_got_sick
;    file-type "\t"
;    file-type day_I_got_sick
;    file-type "\n"
; ;   file-write day_infector_got_sick  file-write day_I_got_sick   file-write "\r\n"
;    file-close


  ]
   ; set color red


end

to recover

    set days_sick 0
    set days_exposed 0
    set sick-period 0
    set incubation_period 0
  set isTravelCase FALSE
end

to initiate-hetero-relationship ; have to add age mixing matrices here later
;  let my-age age
;  if count link-neighbors = 0
 ; [
    let potential-partners no-turtles
  ifelse sex = 1
      [

        if any? other turtles with [sex = 2 and (sexual-preference = 1 or sexual-preference = 3) ]
        [
          set potential-partners other turtles with [sex = 2 and (sexual-preference = 1 or sexual-preference = 3)]; and count link-neighbors = 0]


        ]

     ]
    [ ; else
    if any? other turtles with [sex = 1 and (sexual-preference = 1 or sexual-preference = 3)];and count link-neighbors = 0]
    [
      set potential-partners other turtles with [sex = 1 and (sexual-preference = 1 or sexual-preference = 3)]; and count link-neighbors = 0]
    ]
  ]

  find-partner potential-partners
;  ]

end
to find-partner [potential-partners]
  ;output-print "potential partners"
  ; output-print count potential-partners
  let my-age age
  let my-cohort cohort-from-age my-age
  let selected-partner-cohort partner-age my-age
 ; output-print my-cohort
 ; output-print selected-partner-cohort
;  output-print "-------"
;  shuffle potential-partners
  let found? false
  let new-partner no-turtles
  ifelse any? potential-partners with [age-cohort =  selected-partner-cohort and count link-neighbors = 0]
  [
          breakup

    ask n-of 1 potential-partners  with  [age-cohort =  selected-partner-cohort and count link-neighbors = 0]

    [

      create-lt-link-with myself

;      output-print "new rel between"
    ]
 set brokenUp? FALSE


  ]
  [
;     output-print "no more single turtles" ; if there are no more single turles, make new ones

  ]


end
to  hatch-new-partner [cohort partner-sexual-preference partner-sex]
   hatch  1
  [
    set age age-from-cohort cohort
    set age-days 1 + random 365
    set age-cohort age-cohort
     set sexual-preference 1; to be updated later 1 for hetero, 2 for homo and 3 for bi
    setxy random-xcor random-ycor

    if partner-sexual-preference = 1
    [
     ifelse partner-sex = 1
      [
        set sex 2
      ]
      [
        set sex 1
      ]
    ]
    if partner-sexual-preference = 2
    [
      set sex partner-sex
    ]
    if partner-sexual-preference = 3
    [

      ifelse random-float 1 < 0.5
      [
      set sex 2
      ]
      [
      set sex 1
      ]



    ]
   set-disease-status(1)
  create-link-with myself

  ]

end

to-report age-from-cohort [cohort]
 let this-age cohort * 4 + 16 + random 4
  report this-age


end
to initiate-homo-relationship ; have to add age mixing matrices here later
    let my-age age
 ;   if count link-neighbors = 0
 ; [
  let potential-partners no-turtles
  ifelse sex = 2
      [
        if  any? other turtles with [sex = 2 and (sexual-preference = 2 or sexual-preference = 3)];and count link-neighbors = 0]
        [
          set potential-partners other turtles with [sex = 2 and (sexual-preference = 2 or sexual-preference = 3) ];and count link-neighbors = 0]

        ]

      ]
      [
    if   any? other turtles with  [sex = 1 and (sexual-preference = 2 or sexual-preference = 3)];and count link-neighbors = 0]
       [
          set potential-partners other turtles with [sex = 1 and (sexual-preference = 2 or sexual-preference = 3)]; and count link-neighbors = 0]
    ]
      ]

  find-partner potential-partners

 ; ]
end

to initiate-bi-relationship ; have to add age mixing matrices here later
 ;   let my-age age
 let cohort  cohort-from-age age
  ; first decided if its same sex or opposite sex
  ; lets make another matrix
  let same_sex? TRUE
  let same_sex_prob matrix:get bi_prob_same_sex cohort (sex - 1)
  ifelse random-float 1 < same_sex_prob
  [
    set same_sex? TRUE
  ]
  [
    set same_sex? FALSE
  ]

  let potential-partners no-turtles
;  ifelse random-float 1 < 0.5 ; pair with a male
 ; [
    if sex = 1 and same_sex? = TRUE ; men to men
    [
      if any? other turtles with [sex = 1 and (sexual-preference = 2 or sexual-preference = 3)] ; look at homo and bi men
      [
          set potential-partners other turtles with [sex = 1 and (sexual-preference = 2 or sexual-preference = 3)]
      ]
  ]

 if sex = 1 and same_sex? = FALSE


    [ ; men to women
       if any? other turtles with [sex = 2 and  (sexual-preference = 1 or sexual-preference = 3)   ];and count link-neighbors = 0)] ; het or bi women
      [
          set potential-partners other turtles with [sex = 2 and  (sexual-preference = 1 or sexual-preference = 3)   ]; and count link-neighbors = 0)]
      ]

    ]


  if sex = 2 and same_sex? = TRUE ; women to women
  [
    if any? other turtles with [sex = 2 and  (sexual-preference = 2 or sexual-preference = 3)   ];and count link-neighbors = 0)] ; look in homo or bi women
      [
          set potential-partners other turtles with [sex = 2 and  (sexual-preference = 2 or sexual-preference = 3) ]; and count link-neighbors = 0)]
      ]




  ]


  if sex = 2 and same_sex? = FALSE ; women to men
  [
   if any? other turtles with [sex = 1 and  (sexual-preference = 1 or sexual-preference = 3)  ];and count link-neighbors = 0)] ; het and bi men
      [
          set potential-partners other turtles with [sex = 1 and  (sexual-preference = 1 or sexual-preference = 3) ]; and count link-neighbors = 0)]
      ]





]



  find-partner potential-partners
;  ]

end
to inital-relationships
  ask  turtles ;
  [
    if random-float 1 < 0.5 ; reduce to 0.4 because they are in pairs
  [
    ;  output-print  count link-neighbors
      if count link-neighbors = 0
    [
      get-partners
    ]

  ]
  ]

end

to get-partners
;  output-print count link-neighbors
if sexual-preference = 1
    [initiate-hetero-relationship]
     if sexual-preference = 2
    [initiate-homo-relationship]
    if sexual-preference = 3
    [initiate-bi-relationship]

end


to go-age
  let dead 0
ask turtles
  [
  ;output-print age
    set age-days age-days + 1
    if age-days > 365
      [
       ; output-print age-days
      ;  output-print "Happy bday to me"
        set age-days 1
        set age age + 1
        set age-cohort cohort-from-age age
        ifelse age <= max-age
        [

          update-lambda
        ]
        [
         ;  output-print "One died, One Born"
           set dead dead + 1
          die
        ]

    ]

  ]
   create-turtles dead
          [
            set age min-age
            set age-days 1
            setxy random-xcor random-ycor
            set color yellow
            set-disease-status 1
            set vaccinated? FALSE

          ]

end

to-report partial-sums [lst]
report butfirst reduce [[result-so-far next-item] -> lput (next-item + last
result-so-far) result-so-far] fput [0] lst
end

to-report partner-age [my-age]
  let prob random-float 1
  let cohort cohort-from-age my-age
  let probabilities matrix:get-row age-mix-agg cohort
  let cum-pmf partial-sums probabilities
  let n random-float 1
  report length filter [ [?1] -> ?1 < n ] cum-pmf  ; indexing starts at 0

end


to-report TotalInfectedPop
  report count turtles with [disease-status = 3]
end
to-report TotalNewInfectedPop
  report count turtles with [disease-status = 3 and days_sick = 1 ]
end
to-report TotalInfectedSex [s]
  report count turtles with [disease-status = 3 and sex = s]
end
to-report TotalNewInfectedSex[s]
  report count turtles with [disease-status = 3 and sex = s and days_sick = 1]
end
to-report PercentageInfectedSex [s]
  let n   count turtles with [disease-status = 3 ]
  ifelse n = 0
  [report n]
  [report count turtles with [disease-status = 3 and sex = s] / n ]
end

to-report PercentageNewInfectedSex [s]
  let n   count turtles with [disease-status = 3 and days_sick = 1]
  ifelse n = 0
  [report n]
  [report count turtles with [disease-status = 3 and sex = s  and days_sick = 1] / n ]
end



to-report TotalInfectedSexSexualPref [s sp]
  report count turtles with [disease-status = 3 and sex = s and sexual-preference = sp]
end

to-report TotalNewInfectedSexSexualPref [s sp]
  report count turtles with [disease-status = 3 and sex = s and sexual-preference = sp and days_sick = 1 ]
end


to-report PercentageInfectedSexSexualPref [s sp]
   let n  count turtles with [sex = s and disease-status = 3 ]
  ifelse n  = 0
  [
    report 0
  ]

  [
    report count turtles with [disease-status = 3 and sex = s and sexual-preference = sp] / n
  ];
end

to-report PercentageNewInfectedSexSexualPref [s sp]
   let n  count turtles with [sex = s and disease-status = 3 and days_sick = 1 ]
  ifelse n  = 0
  [
    report 0
  ]

  [
    report count turtles with [disease-status = 3 and sex = s and sexual-preference = sp and days_sick = 1 ] / n
  ];
end
;to-report PercentageInfectedSexual [s]
;  report count turtles with [disease-status = 3 and sex = s] / count turtles with [sex = s ]
;end

to-report TotalInfectedAge [ac]
  report count turtles with [disease-status = 3 and age-cohort = ac]
end
to-report TotalNewInfectedAge [ac]
  report count turtles with [disease-status = 3 and age-cohort = ac and days_sick = 1 ]
end

to-report TotalInfectedAgeSex [ac s]
  report count turtles with [disease-status = 3 and sex = s and age-cohort = ac]
end
to-report TotalNewInfectedAgeSex [ac s]
  report count turtles with [disease-status = 3 and sex = s and age-cohort = ac and days_sick = 1]
end

to-report TotalInfectedAgeSexSexualPref [ac s sp]
  report count turtles with [disease-status = 3 and sex = s and age-cohort = ac]
end

to-report TotalNewInfectedAgeSexSexualPref [ac s sp]
  report count turtles with [disease-status = 3 and sex = s and age-cohort = ac and days_sick = 1]
end

to-report PercentageInfectedAgeSex [ac s]

  let n count turtles with [sex = s and disease-status = 3 ]
  ifelse n  = 0
  [report 0]
  [report count turtles with [disease-status = 3 and sex = s and age-cohort = ac] / n]

end

to-report PercentageNewInfectedAgeSex [ac s]

  let n count turtles with [sex = s and disease-status = 3 and days_sick = 1 ]
  ifelse n  = 0
  [report 0]
  [report count turtles with [disease-status = 3 and sex = s and age-cohort = ac  and days_sick = 1 ] / n]

end

to-report PropInfected [s sp]
  let n count turtles with [sex = s and sexual-preference = sp ]
  ifelse n = 0
  [report 0]
  [report count turtles with [sex = s and sexual-preference = sp and disease-status = 3] / n]
end
to-report PropNewInfected [s sp]
  let n count turtles with [sex = s and sexual-preference = sp and days_sick = 1 ]
  ifelse n = 0
  [report 0]
  [report count turtles with [sex = s and sexual-preference = sp and disease-status = 3 and days_sick = 1 ] / n]
end


to-report InfectedPop
  report count turtles with [disease-status = 3 ]

end

to-report RecoveredPop
  report count turtles with [disease-status = 4 ]

end
to-report NewTravelCases
  report count turtles with [disease-status = 3 and isTravelCase = TRUE  and days_sick = 1 ]
end
; Time varying R represents the expected number of secondary local cases arising from a primary case infected at time t
to TimeVaryingR

  let timeVaryingRt []
  let time_stamps []
   let i 1
    loop [

    ifelse i = ticks
    [

        file-open "RNumbers_fixed.txt"
      file-print behaviorspace-run-number
        file-print timeVaryingRt
        file-close
        stop


    ]
    [
  ;  output-print i
     let Rt_today []


    ifelse count turtles with [disease-status = 4 and day_i_got_sick = i and infected_by != nobody] > 0
    [
      ask turtles with [disease-status = 4 and day_i_got_sick = i and infected_by != nobody]
      [
        set Rt_today lput  numinfected Rt_today
      ]
      set Rt_today mean Rt_today
      set timeVaryingRt lput  Rt_today timeVaryingRt
      set time_stamps lput  i time_stamps

    ]
    [
      set Rt_today lput  0 Rt_today
      set timeVaryingRt lput  Rt_today timeVaryingRt
      set time_stamps lput  i time_stamps
    ]
     set i i + 1

  ]
  ]




end


to-report calculateR

  let R_est -1
  ask turtles with [ disease-status = 4 ];and infected_by != nobody]
  [
    set numinfected  count turtles with [
      infected_by = myself
    ]
  ]

  ;; If there are any agents who have recovered and were not the initially infected agents creates a list of count of agents infected by each of the recovered agents and then finds the average of the list.
  if count turtles with [ disease-status = 4 and infected_by != nobody] > 0
  [
    let infectedbycase []
    ask turtles with [ disease-status = 4  and infected_by != nobody]
    [
      set infectedbycase lput numinfected infectedbycase
    ]
  set R_est mean infectedbycase
 ]
report R_est

end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
700
501
-1
-1
14.61
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
31
31
94
64
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
706
144
861
204
%homosexual_males
1.144128
1
0
Number

INPUTBOX
707
209
862
269
%homosexual_females
0.3044312
1
0
Number

INPUTBOX
707
274
862
334
%bi_males
3.924996
1
0
Number

INPUTBOX
708
344
863
404
%bi_females
6.043522
1
0
Number

INPUTBOX
705
13
860
73
population
10000.0
1
0
Number

INPUTBOX
0
122
157
182
initially_infected_MSM
5.0
1
0
Number

BUTTON
31
77
94
110
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
293
110
654
291
SEIR
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"R" 1.0 0 -7500403 true "" "plot count turtles with [disease-status = 3 and days_sick = 1]"

INPUTBOX
5
263
160
323
People_vaccinated
0.0
1
0
Number

INPUTBOX
866
344
1021
404
ImportedCasesPerYear
30.0
1
0
Number

INPUTBOX
866
273
1021
333
SuperSpreaderEventsPerYear
30.0
1
0
Number

INPUTBOX
5
196
160
256
initially_infected_others
0.0
1
0
Number

CHOOSER
6
329
162
374
Vaccination_Programme
Vaccination_Programme
"None" "RandomAll" "RingVac" "RandomMSM" "MSM_C1_C6"
1

INPUTBOX
706
78
861
138
vaccine_efficacy
0.85
1
0
Number

INPUTBOX
6
379
161
439
Transmission_probability
0.125
1
0
Number

INPUTBOX
5
444
162
504
TP_multiplier_for_msm
5.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles with [disease-status = 3]</metric>
    <enumeratedValueSet variable="%bi_males">
      <value value="4.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_MSM">
      <value value="19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_others">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_females">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_females">
      <value value="6.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="People_vaccinated">
      <value value="0"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_males">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Vaccination_Programme">
      <value value="&quot;None&quot;"/>
      <value value="&quot;RandomMSM&quot;"/>
      <value value="&quot;RandomAll&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentVaccinationAgeCohorts" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles with [disease-status = 3]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 0]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 1]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 2]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 3]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 4]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 5]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 6]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 7]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 8]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 9]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 10]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 11]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 12]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 13]</metric>
    <metric>count turtles with [disease-status = 3 and age-cohort = 14]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 1]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 2]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 1 and sexual-preference = 2]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 2 and sexual-preference = 2]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 1 and sexual-preference = 3]</metric>
    <metric>count turtles with [disease-status = 3 and sex = 2 and sexual-preference = 3]</metric>
    <enumeratedValueSet variable="%bi_males">
      <value value="4.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_females">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_females">
      <value value="6.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="People_vaccinated">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_males">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PartiesPerYear">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TravelRelatedCasesPerYear">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_Parties_NoParties" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>Total_Infected</metric>
    <metric>Total_Infected_men</metric>
    <metric>Total_Infected_women</metric>
    <metric>Total_Infected_men_het</metric>
    <metric>Total_Infected_women_het</metric>
    <metric>Total_Infected_men_homosexual</metric>
    <metric>Total_Infected_women_homosexual</metric>
    <metric>Total_Infected_men_bi</metric>
    <metric>Total_Infected_women_bi</metric>
    <enumeratedValueSet variable="%bi_males">
      <value value="4.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_females">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_females">
      <value value="6.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PartiesPerYear">
      <value value="0"/>
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="People_vaccinated">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TravelRelatedCasesPerYear">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_males">
      <value value="1.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="VTP" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>file-open "RNumbers.txt"
file-print behaviorspace-run-number

file-print calculateR

file-close

TimeVaryingR</final>
    <timeLimit steps="180"/>
    <metric>NewTravelCases</metric>
    <metric>InfectedPop</metric>
    <metric>RecoveredPop</metric>
    <metric>TotalNewInfectedPop</metric>
    <metric>TotalNewInfectedSex 1</metric>
    <metric>PercentageNewInfectedSex 1</metric>
    <metric>PercentageNewInfectedSex 2</metric>
    <metric>TotalNewInfectedSex 2</metric>
    <metric>TotalNewInfectedAge 0</metric>
    <metric>TotalNewInfectedAge 1</metric>
    <metric>TotalNewInfectedAge 2</metric>
    <metric>TotalNewInfectedAge 3</metric>
    <metric>TotalNewInfectedAge 4</metric>
    <metric>TotalNewInfectedAge 5</metric>
    <metric>TotalNewInfectedAge 6</metric>
    <metric>TotalNewInfectedAge 7</metric>
    <metric>TotalNewInfectedAge 8</metric>
    <metric>TotalNewInfectedAge 9</metric>
    <metric>TotalNewInfectedAge 10</metric>
    <metric>TotalNewInfectedAge 11</metric>
    <metric>TotalNewInfectedAge 12</metric>
    <metric>TotalNewInfectedAge 13</metric>
    <metric>TotalNewInfectedAge 14</metric>
    <metric>TotalNewInfectedAge 15</metric>
    <metric>PercentageNewInfectedSexSexualPref 1 1</metric>
    <metric>PercentageNewInfectedSexSexualPref 1 2</metric>
    <metric>PercentageNewInfectedSexSexualPref 1 3</metric>
    <metric>PercentageNewInfectedSexSexualPref 2 1</metric>
    <metric>PercentageNewInfectedSexSexualPref 2 2</metric>
    <metric>PercentageNewInfectedSexSexualPref 2 3</metric>
    <metric>TotalNewInfectedSexSexualPref 1 1</metric>
    <metric>TotalNewInfectedSexSexualPref 1 2</metric>
    <metric>TotalNewInfectedSexSexualPref 1 3</metric>
    <metric>TotalNewInfectedSexSexualPref 2 1</metric>
    <metric>TotalNewInfectedSexSexualPref 2 2</metric>
    <metric>TotalNewInfectedSexSexualPref 2 3</metric>
    <metric>PropNewInfected 1 1</metric>
    <metric>PropNewInfected 1 2</metric>
    <metric>PropNewInfected 1 3</metric>
    <metric>PropNewInfected 2 1</metric>
    <metric>PropNewInfected 2 2</metric>
    <metric>PropNewInfected 2 3</metric>
    <metric>count turtles with [sex = 1]</metric>
    <metric>count turtles with [sex = 2]</metric>
    <metric>count turtles with [sex = 1 and sexual-preference = 1]</metric>
    <metric>count turtles with [sex = 1 and sexual-preference = 2]</metric>
    <metric>count turtles with [sex = 1 and sexual-preference = 3]</metric>
    <metric>count turtles with [sex = 2 and sexual-preference = 1]</metric>
    <metric>count turtles with [sex = 2 and sexual-preference = 2]</metric>
    <metric>count turtles with [sex = 2 and sexual-preference = 3]</metric>
    <enumeratedValueSet variable="PartiesPerYear">
      <value value="0"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="People_vaccinated">
      <value value="0"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Vaccination_Programme">
      <value value="&quot;RandomMSM&quot;"/>
      <value value="&quot;None&quot;"/>
      <value value="&quot;RandomAll&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TravelRelatedCasesPerYear">
      <value value="0"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_others">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_MSM">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine_efficacy">
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_males">
      <value value="1.144128"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_females">
      <value value="0.3044312"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_males">
      <value value="3.924996"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_females">
      <value value="6.043522"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attack_rate">
      <value value="0.125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attack_rate_multiplier_for_msm">
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="One" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>file-open "RNumbers.txt"
file-print behaviorspace-run-number

file-print calculateR

file-close

TimeVaryingR</final>
    <timeLimit steps="180"/>
    <metric>TotalNewInfectedPop</metric>
    <metric>NewTravelCases</metric>
    <enumeratedValueSet variable="PartiesPerYear">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="People_vaccinated">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Vaccination_Programme">
      <value value="&quot;RandomAll&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TravelRelatedCasesPerYear">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_others">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initially_infected_MSM">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine_efficacy">
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_males">
      <value value="1.144128"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%homosexual_females">
      <value value="0.3044312"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_males">
      <value value="3.924996"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%bi_females">
      <value value="6.043522"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attack_rate">
      <value value="0.125"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attack_rate_multiplier_for_msm">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
