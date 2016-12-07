;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recombranch
;; recombranch is an agent-based model designed to explore
;; the dynamics of innovation.
;; Copyright (C) 2009 Koen Frenken, Luis R. Izquierdo & Paolo Zeppini.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es


breed [agents agent]
breed [technologies technology]

;;;;;;;;;;;;;;;;;
;;; VARIABLES ;;;
;;;;;;;;;;;;;;;;;

globals [

  list-of-agents
    ;; this is just for efficiency, to have an ordered list of the agents
  list-of-technologies
    ;; ditto. Note that this list is in reverse chronological order, i.e. the
    ;; first technology is the most recent one. The main reason to do it this
    ;; way is that fput is much faster than lput.

  agents-per-technology
    ;; list containing the number of agents for each technology (in reverse chronological order)
  agents-per-technology-no-zeros
    ;; ditto without 0s
  quality-levels-of-agents
    ;; list containing the quality-level of the technology used by each individual agent
    ;; (always in the same order)
  min-quality
    ;; minimum quality-level across agents (used as an indicator of "major transitions")
  mean-quality
  max-quality

  time-step-innovators
    ;; agents that are innovating in the current tick

  entropy
    ;; entropy is a measure of variety in the distribution of agents among different technologies.
    ;; The formula is: entropy = - SUMi (si�log2 (si)) , where si > 0 stands for the share of agents using technology i.
  accumulated-entropy

  utility-list
    ;; a list containing the utility for each agent
  min-utility
  mean-utility
  max-utility

  n-of-recombinations
    ;; number of recombinations in the time-step
  cum-n-of-recombinations
    ;; accumulated number of recombinations
  cum-n-of-transitions
    ;; a transition occurs if the minimum quality level across agents (i.e. of every technology in use) increases.
  quality-level-of-last-transition
  transition-size-series

  my-random-seed
]

agents-own [
  my-technology         ;; techonology that the agent is currently using
  time-step-innovator?  ;; true if the agent innovates at this time-step
]

technologies-own [
  my-num-agents
  quality-level ;; intrinsic quality level of the technology

  score
    ;; this is a variable that measures how attractive this technology is
    ;; (for a certain agent that is making the decision at that point)
  scores-list
    ;; this is a list that contains the score of this and other technologies
    ;; (it measures how attractive this and other technologies are)

  distance-to-tech
    ;; this is a variable that stores the geodesic distance from this technology
    ;; to a certain other (that is performing this computation at that time)
  visited?
    ;; this is a supporting variable to compute distance-to-tech

  my-num-time-step-innovators

]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  clear-all
  setup-technologies
  setup-agents
  setup-variables
  gather-data
  do-plots
  set my-random-seed new-seed
  random-seed my-random-seed
    ;; thus we can keep the value of the random seed used (in my-random-seed)
  reset-ticks
end

to setup-technologies
  set-default-shape technologies "circle"
  create-technologies 1 [
    setxy min-pxcor max-pycor
    set color (5 + quality-level * 10)
    set label-color white
  ]
end

to setup-agents
  create-agents num-agents [
    set hidden? true
    set my-technology one-of technologies
    ask my-technology [set my-num-agents (my-num-agents + 1)]
  ]
end

to setup-variables
  set transition-size-series []
  set list-of-agents (sort agents)
  set list-of-technologies (reverse sort technologies)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN-TIME PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;; Main procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;

to go

  ask agents [set time-step-innovator? false]

  ;; INNOVATION STAGE

  ;; select the group of this time-step innovators
  ask agents [
    if random-float 1.0 < p-innovation [
      set time-step-innovator? true
    ]
  ]

  create-innovations

  ;; DECISION STAGE

  ask technologies [set size 1]

  ask (technologies with [my-num-agents > 0]) [create-scores-lists]
  ask agents with [not time-step-innovator?] [decide-technology]

  ;; REPORTING STAGE

  tick

  gather-data

  do-plots

  if ticks = pause-at-tick [stop]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Decision-related procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following is a procedure to be run by technologies.
;; The technology running "create-score-list" will update the value of its
;; variable scores-list.
;; - scorse-lists is a list that stores the score assigned by this technology to
;;   each technology available. These scores are a measure of how attractive each
;;   technology available is for an agent that is using this technology.

to create-scores-lists
  set-distances-from-me-to-other-technologies
  set scores-list map [[utility - distance-to-tech] of ?] list-of-technologies
end

;; The following is a procedure to be run by agents.
;; The agent running "decide-technology" will choose a technology with the best score.

to decide-technology
    ;; select the technology with the best score
    ;; if yours is one of them, stick to it. Otherwise resolve ties randomly
  (foreach list-of-technologies ([scores-list] of my-technology) [
    ask ?1 [set score ?2]
  ])
  if ([score] of my-technology) < (max ([scores-list] of my-technology)) [
    adopt-technology max-one-of technologies [score]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation of innovations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-innovations
  set n-of-recombinations 0
  set time-step-innovators (agents with [time-step-innovator?])
  let new-technology nobody

  if any? time-step-innovators [

  let technologies-of-timestep-innovators
      remove-duplicates ([my-technology] of time-step-innovators)

  let partition nobody

  ifelse recombination?
  [set partition (list technologies-of-timestep-innovators)]
  [set partition map [(list ?)] technologies-of-timestep-innovators]

  ask time-step-innovators [
    ask my-technology [set size 2]
  ]

  foreach partition [
    let list-of-innovating-techs ?
    let current-innovator-technology (first list-of-innovating-techs)
    let current-innovator one-of (time-step-innovators with [my-technology = current-innovator-technology])

    let new-technology-creators nobody
    let quality-level-of-new-technology (1 + max (map [[quality-level] of ?] list-of-innovating-techs))

    foreach list-of-innovating-techs [
      let innovating-technology ?
      set new-technology-creators (turtle-set new-technology-creators (time-step-innovators with [my-technology = ?]))
    ]

    create-technologies 1 [
      set quality-level quality-level-of-new-technology
      set my-num-agents 0
      set new-technology self
      set color (5 + quality-level-of-new-technology * 10)
      set size 2
      set label-color white
    ]
    set list-of-technologies (fput new-technology list-of-technologies)

    ask current-innovator-technology [
      ask new-technology [
        set xcor clip-xcor ([xcor] of myself + 1)
        set ycor clip-ycor ([ycor] of myself - 1)
      ]
    ]

    foreach list-of-innovating-techs [
      ask ? [create-link-to new-technology [set color white]]
    ]

    ask new-technology-creators [
      adopt-technology new-technology
    ]

    if ([count in-link-neighbors] of new-technology) > 1 [set n-of-recombinations (n-of-recombinations + 1)]

  ] ;; end of (foreach partition)

  ] ;; end of (if any? time-step-innovators)
  set cum-n-of-recombinations (cum-n-of-recombinations + n-of-recombinations)

end


to hide-dead-ends
  ask ((technologies with [not hidden?]) with [my-num-agents = 0 and (all? out-link-neighbors [hidden?])]) [
    set hidden? true
    ask my-in-links [set hidden? true]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supporting procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to adopt-technology [new-tech]
  ask my-technology [set my-num-agents (my-num-agents - 1)]
  set my-technology new-tech
  ask my-technology [set my-num-agents (my-num-agents + 1)]
end

to-report utility
  report quality-level + (my-num-agents * network-externalities)
end

to set-distances-from-me-to-other-technologies
  ask other technologies [
    set visited? false
    set distance-to-tech (10 ^ 10)
  ]
  set visited? true
  set distance-to-tech 0
  walk-through-the-network
end

to walk-through-the-network
  let all-my-nbrs (turtle-set in-link-neighbors out-link-neighbors) with [not visited?]
  ask all-my-nbrs [
    set visited? true
    set distance-to-tech ([distance-to-tech] of myself + 1)
  ]
  ask all-my-nbrs [walk-through-the-network]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reporting procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to gather-data

  set quality-levels-of-agents map [[quality-level] of ([my-technology] of ?)] list-of-agents
  set min-quality  min  quality-levels-of-agents
  set mean-quality mean quality-levels-of-agents
  set max-quality  max  quality-levels-of-agents

  set utility-list map [[utility] of ([my-technology] of ?)] list-of-agents
  set min-utility  min  utility-list
  set mean-utility mean utility-list
  set max-utility  max  utility-list

  check-transitions

  set agents-per-technology map [[my-num-agents] of ?] list-of-technologies
  set agents-per-technology-no-zeros (remove 0 agents-per-technology)
  calculate-entropy
  set accumulated-entropy (accumulated-entropy + entropy)

end

to check-transitions
  set transition-size-series (lput (min-quality - quality-level-of-last-transition) transition-size-series)
  if min-quality > quality-level-of-last-transition [
    set cum-n-of-transitions (cum-n-of-transitions + 1)
    set quality-level-of-last-transition min-quality
  ]

end

to do-plots
  ask technologies [set label my-num-agents] ;stop
  plot-utility
  plot-quality-levels
end

to plot-utility
  set-current-plot "Utility"
  set-current-plot-pen "min"   plot min-utility
  set-current-plot-pen "mean"  plot mean-utility
  set-current-plot-pen "max"   plot max-utility
end

to plot-quality-levels
  set-current-plot "Quality levels in use"
  set-current-plot-pen "min"   plot min-quality
  set-current-plot-pen "mean"  plot mean-quality
  set-current-plot-pen "max"   plot max-quality
end

to calculate-entropy
  let n-of-agents (count agents)
  set entropy ((log n-of-agents 2) - (sum map [ ? * (log ? 2) ] agents-per-technology-no-zeros) / n-of-agents)
end

to-report clip-ycor [y]
  if y < min-pycor [report min-pycor]
  if y > max-pycor [report max-pycor]
  report y
end

to-report clip-xcor [x]
  if x < min-pxcor [report min-pxcor]
  if x > max-pxcor [report max-pxcor]
  report x
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NETWORK APPEARANCE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to relax-network
  let visible-technologies (technologies with [not hidden?])
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few give poor layouts
  repeat 3 [
    ;; the more technologies we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count visible-technologies
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring (visible-technologies with [any? in-link-neighbors])
                  (links with [not hidden?]) (1 / factor) (7 / factor) (3 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of visible-technologies + min [xcor] of visible-technologies
  let y-offset max [ycor] of visible-technologies + min [ycor] of visible-technologies
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask visible-technologies [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

to layout-according-to-quality-level
  repeat 20 [relax-network]
  ask (technologies with [not hidden?]) [
    set ycor clip-ycor (max-pycor - quality-level  / 2)
  ]
end

to drag-and-drop
  if mouse-down? [
    let candidate min-one-of (technologies with [not hidden?]) [distancexy mouse-xcor mouse-ycor]
    if [distancexy mouse-xcor mouse-ycor] of candidate < 1 [
      ;; the WATCH primitive puts a "halo" around the watched turtle.
      watch candidate
      while [mouse-down?] [
        ;; If we don't force the display to update, the agent won't
        ;; be able to see the turtle moving around.
        display
        ;; The SUBJECT primitive reports the turtle being watched.
        ask subject [ setxy mouse-xcor mouse-ycor ]
      ]
      ;; Undoes the effects of WATCH.  Can be abbreviated RP.
      reset-perspective
    ]
  ]
end


; *** NetLogo 4.0.4 Model Copyright Notice ***
;
; Copyright 2005 by Uri Wilensky.  All rights reserved.
;
; Permission to use, modify or redistribute this model is hereby granted,
; provided that both of the following requirements are followed:
; a) this copyright notice is included.
; b) this model will not be redistributed for profit without permission
;    from Uri Wilensky.
; Contact Uri Wilensky for appropriate licenses for redistribution for
; profit.
;
; To refer to this model in academic publications, please use:
; Wilensky, U. (2005).  NetLogo Small Worlds model.
; http://ccl.northwestern.edu/netlogo/models/SmallWorlds.
; Center for Connected Learning and Computer-Based Modeling,
; Northwestern University, Evanston, IL.
;
; In other publications, please use:
; Copyright 2005 Uri Wilensky.  All rights reserved.
; See http://ccl.northwestern.edu/netlogo/models/SmallWorlds
; for terms of use.
;
; *** End of NetLogo 4.0.4 Model Copyright Notice ***
@#$#@#$#@
GRAPHICS-WINDOW
208
10
634
457
10
10
19.81
1
13
1
1
1
0
0
0
1
-10
10
-10
10
1
1
1
time-step
30.0

BUTTON
19
178
198
211
Setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
18
10
201
43
num-agents
num-agents
1
1000
100
1
1
NIL
HORIZONTAL

SLIDER
18
86
201
119
p-innovation
p-innovation
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
18
48
200
81
network-externalities
network-externalities
0
1
0.1
0.01
1
NIL
HORIZONTAL

BUTTON
19
308
200
342
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
19
270
200
304
Go
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

BUTTON
19
375
199
408
Relax network
with-local-randomness [relax-network]
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
641
296
906
457
Utility
time
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" ""
"max" 1.0 0 -13345367 true "" ""
"min" 1.0 0 -2674135 true "" ""

PLOT
641
130
906
292
Quality levels in use
time
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" ""
"max" 1.0 0 -13345367 true "" ""
"min" 1.0 0 -2674135 true "" ""

MONITOR
641
70
906
123
accumulated entropy
accumulated-entropy
3
1
13

SLIDER
19
233
200
266
pause-at-tick
pause-at-tick
0
100
50
10
1
NIL
HORIZONTAL

MONITOR
776
10
906
63
recombinations
cum-n-of-recombinations
0
1
13

SWITCH
18
123
200
156
recombination?
recombination?
0
1
-1000

MONITOR
641
10
764
63
transitions
cum-n-of-transitions
0
1
13

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
