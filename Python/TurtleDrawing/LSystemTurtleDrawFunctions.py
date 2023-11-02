# -*- coding: utf-8 -*-
"""
Created on Tue Oct 17 15:09:14 2023

@author: Afromullet
"""

'''
This implementation requires context for every drawing function.
In addition, each functions returns the Token it's for'
'''
def Forward(context):
    context['turtle'].fd(context['line_length'])
    return 'F'

def Positive_Turn(context):
    context['turtle'].right(context['angle'])
    return '+'
    
def Negative_Turn(context):
    context['turtle'].left(context['angle'])
    return "-"

def Stack_Push(context):
    ang = context['turtle'].heading()
    pos = [context['turtle'].xcor(), context['turtle'].ycor()]
    context['stack'].append((ang, pos)) # push state to save
    return '['
    
def Stack_Pop(context):
    
    if len(context['stack']):
        angle, pos = context['stack'].pop()  # pop state to restore
        context['turtle'].setheading(angle)
        context['turtle'].penup()
        context['turtle'].goto(pos[0], pos[1])
        context['turtle'].pendown()
    return ']'

def X_DoNothing(context):
    return "X"

def H_DoNothing(context):
    return "H"

# def Heading_Right(context):
#     context['turtle'].setheading( context['turtle'].heading()+context['angle'])
#     return '+'
    
# def Heading_Left(context):
#     context['turtle'].setheading( context['turtle'].heading()-context['angle'])    
#     return '-'
    
    

    
