# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 14:33:32 2023

@author: Afromullet
"""
import pygame
import PygameInit as pInit
import numpy as np
import Util 

'''
Needs to calculate the line_end_pos every time, since the angle may have changed. 

Since turtle draws things iteratively, the new start pos is the last end pos.
Setting the end pos equal to start pos so we know we've finished drawing (determine if this makes sense)
'''
def Forward(context):
    
    lineWidth = 1
    if context["continue_drawing"]:
        
        if context["rule_parameters"]:
            variables = Util.get_parameters(context["rule_parameters"])
            lineWidth = variables['lineWidth']
            
        print(lineWidth)

        new_x = context["current_position"].x + np.cos(np.radians(context["heading"])) * context["line_length"]
        new_y = context["current_position"].y + np.sin(np.radians(context["heading"])) * context["line_length"]
        next_vec = pygame.math.Vector2(new_x, new_y) #For now, don't store the next position in the vector. Only do that if creating a new vector impacts performance
        pygame.draw.line(context["drawing_window"],(0, 0, 255),context["current_position"],next_vec,width=lineWidth)
        #Setting this so that we continue drawing from where the last line ended.
        context["current_position"].x = new_x
        context["current_position"].y = new_y
    return "F"

def Positive_Turn(context):
    if context["continue_drawing"]:
        context["heading"] += context["angle"]
    return '+'


def Negative_Turn(context):
    if context["continue_drawing"]:
        context["heading"] -= context["angle"]
    return '-'
    
    
def Stack_Push(context):
    if context["continue_drawing"]:
        ang = context['heading']
        pos = pygame.math.Vector2(context['current_position'].x, context['current_position'].y)
        context['stack'].append((ang, pos)) # push state to save
    return '['
    
def Stack_Pop(context):
    
    if context["continue_drawing"]:
        if len(context['stack']):
            angle, pos = context['stack'].pop()  # pop state to restore
            context['heading'] = angle
            context['current_position'].x = pos.x
            context['current_position'].y = pos.y
    return ']'


# Function to draw a building based on the context
def draw_building(context):

    if context["continue_drawing"]:
        pygame.draw.rect(context["drawing_window"], (0, 0, 255), (context['current_position'].x, context['current_position'].y,
                                                                  context["building_width"], context["building_height"]))
    return "B"



def Do_Nothing(context):
    return 'X'