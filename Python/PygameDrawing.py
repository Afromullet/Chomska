# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 14:32:53 2023

@author: Afromullet
"""
# Importing pygame module
import pygame
import PygameInit as pInit
import LSystemInit as LInit
import LSystemPygameDrawFunctions as pDraw

'''
Reads and draws a non parametized L System
'''

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
    "continue_drawing" : True #Used By the drawing functions to tell it to stop drawing
}



context['drawing_window'] = pInit.pygame_window
context['angle'] = 25

Drawing_Actions = {}
l_generations = LInit.generate_rules("X","bg3.txt",4)
Drawing_Actions = pInit.Initialize_Draw_Functions(context,Drawing_Actions,pDraw)


for symbol in l_generations:
      Drawing_Actions[symbol](context)
      
      
      # Draws the surface object to the screen.
pygame.display.update()


while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            quit()