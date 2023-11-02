# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 16:41:57 2023

@author: Afromullet
"""
import turtle
from inspect import getmembers, isfunction

WIDTH = 2000
HEIGHT = 3000


def initialize_turtle():
    turtle.screensize(WIDTH,HEIGHT)
    
    t = turtle.Turtle()
    t.speed("fastest")
    t.penup()
    t.goto(t.xcor(), t.ycor()-300)
    t.pendown()
    t.setheading(90)
    return t

'''
LOL, figured it'd be fun to add entries to the Drawing_Actions like this. We use a dictionary, with the key being the token,
and the value being the drawing function. Don't want to manually add to the dictionary, so instead we create it by
getting all of functions from the module and then adding them to the dictionary. We have to call each function once
so that we get the key (since each function returns the key). This requires each function implementation to take
context as input and return the 

DrawModules is a variable number argument so the user can pass multiple modules. Should work as long as each function returns the Token it's for'
'''
def Initialize_Draw_Functions(context,draw_dict,*drawModules):

    context['turtle'].penup()
    for mod in drawModules:
        for f in getmembers(mod, isfunction):
            drawing_func = getattr(mod, f[0])
            draw_dict[drawing_func((context))] = f[1]
      
    context['turtle'].penup()