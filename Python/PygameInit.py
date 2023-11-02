# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 16:38:30 2023

@author: Afromullet
"""


import pygame
from inspect import getmembers, isfunction

WIDTH = 800
HEIGHT = 600


# initiate pygame and give permission
# to use pygame's functionality.
pygame.init()
 
# create the display surface object
# of specific dimension.
pygame_window = pygame.display.set_mode((WIDTH, HEIGHT))
 
# Fill the scree with white color
pygame_window.fill((255, 255, 255))
 

'''
LOL, figured it'd be fun to add entries to the Drawing_Actions like this. We use a dictionary, with the key being the token,
and the value being the drawing function. Don't want to manually add to the dictionary, so instead we create it by
getting all of functions from the module and then adding them to the dictionary. We have to call each function once
so that we get the key (since each function returns the key). This requires each function implementation to take
context as input and return the 

DrawModules is a variable number argument so the user can pass multiple modules. Should work as long as each function returns the Token it's for'
'''
def Initialize_Draw_Functions(context,draw_dict,*drawModules):


    for mod in drawModules:
        for f in getmembers(mod, isfunction):
            drawing_func = getattr(mod, f[0])
            draw_dict[drawing_func((context))] = f[1]
            
    pygame_window.fill((255, 255, 255))
    
    return draw_dict
      
