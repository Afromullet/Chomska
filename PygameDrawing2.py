# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 13:56:25 2023

@author: Afromullet
"""
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 14:32:53 2023

@author: Afromullet
"""
# Importing pygame module
import pygame
from pygame.locals import *
import PygameInit as pInit
import LSystemInit as LInit
import numpy as np
import LSystemPygameDrawFunctions2 as pDraw2
import VoroniDiagram
import re


# Define a context dictionary to store drawing state
context = {
    "line_length": 3,  # Length of a road segment
    "angle": 0,  # Angle for turning
    "current_angle" : 0,
    "building_width": 10,
    "building_height": 10,
    "hut_width": 5,  # Width of the hut
    "farm_width": 5,
    "stack": [],  # Stack for saving positions and angles
    "heading" : 270,
    "drawing_window" : None,
    "angle_update" : False,
    "current_position" : pygame.math.Vector2(pInit.WIDTH/2, pInit.HEIGHT/2),
    "continue_drawing" : True, #Used By the drawing functions to tell it to stop drawing
    "rule_parameters" : ""
}



context['drawing_window'] = pInit.pygame_window
context['angle'] = 90

Drawing_Actions = {}
# l_generations = LInit.generate_rules("F","bg3.txt",3)
Drawing_Actions = pInit.Initialize_Draw_Functions(context,Drawing_Actions,pDraw2)


# vor,center_points = VoroniDiagram.generate(pInit.WIDTH,pInit.HEIGHT)
# pair_polygon_tuple = VoroniDiagram.get_pair_polygons_tuple(vor)
# VoroniDiagram.draw_voroni(context["drawing_window"],pair_polygon_tuple)

# for point in center_points:
#     context["current_position"] = pygame.math.Vector2(point[0], point[1])
#     for symbol in l_generations:
#           Drawing_Actions[symbol](context)
          







# # Example usage:
# file_path = 'bg3args.txt'  # Replace with the path to your rule text file
# rule_map = LInit.create_parametized_rules(file_path)
# l_generations_with_args = LInit.apply_parametized_lsystem_rules("F(1,3)",rule_map,3)

context['angle'] = 90


file_path = 'bg3.txt'
rule_map = LInit.create_parametized_rules(file_path)
l_generations_with_args = LInit.generate_rules("F-F-F-F","bg3.txt",4)



#Get the start location of all the parameters
param_start_locations = [m.start() for m in re.finditer('\(', l_generations_with_args)]

#Keeps track of when we start reading parameters
param_stack = ""
for index,symbol in enumerate(l_generations_with_args):
    
    #Look ahead to see if we have parameters to rad
    if index+1 in param_start_locations:
        param_stack += symbol
 
    elif symbol == ")":
        param_stack += ")" #Complete the parentehses for consistency
        context["rule_parameters"] = param_stack
        Drawing_Actions[param_stack[0]](context) #Index 0 contains the string
        param_stack = ""
        context["rule_parameters"] = []
    elif param_stack:
        param_stack += symbol
    else:
        Drawing_Actions[symbol](context)
        
    

        
    
    

      
# Draws the surface object to the screen.
pygame.display.update()


while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            quit()