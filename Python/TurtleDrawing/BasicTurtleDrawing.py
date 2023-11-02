# -*- coding: utf-8 -*-
"""
Created on Mon Oct  9 08:49:26 2023

@author: Afromullet
"""

import sys
import os
# append a new directory to sys.path
parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))

sys.path.append(parent_dir)


import LSystemTurtleDrawFunctions
import LSystemTurtleBuildingDrawingFunctions
import LSystemInit as LInit

import TurtleInit



# Define a context dictionary to store drawing state
context = {
    "line_length": 15,  # Length of a road segment
    "angle": 20,  # Angle for turning
    "building_width": 5,
    "building_height": 5,
    "hut_width": 5,  # Width of the hut
    "farm_width": 5,
    "stack": [],  # Stack for saving positions and angles
    "turtle": TurtleInit.initialize_turtle()
}


Drawing_Actions = {}
l_generations = LInit.generate_rules("F","bg3.txt",5)

TurtleInit.Initialize_Draw_Functions(context,Drawing_Actions,LSystemTurtleDrawFunctions,LSystemTurtleBuildingDrawingFunctions)

context['turtle'].pendown()


for symbol in l_generations:
      Drawing_Actions[symbol](context)
        
