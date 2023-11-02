# -*- coding: utf-8 -*-
"""
Created on Wed Oct 18 14:54:00 2023

@author: Afromullet
"""

def draw_hut(context):
  context["turtle"].penup()
  context["turtle"].goto(context["turtle"].xcor() - context["hut_width"] / 2, context["turtle"].ycor())
  context["turtle"].pendown()
  context["turtle"].fillcolor('black')
  context["turtle"].begin_fill()
  for _ in range(4):
      context["turtle"].forward(context["hut_width"])
      context["turtle"].left(90)
  context["turtle"].end_fill()
#  context["hut_width"] = initial_width
  return "B"


def draw_farm(context):
  context["turtle"].penup()
  context["turtle"].goto(context["turtle"].xcor() - context["farm_width"] / 2, context["turtle"].ycor())
  context["turtle"].pendown()
  context["turtle"].fillcolor('green')
  context["turtle"].begin_fill()
  for _ in range(4):
      context["turtle"].forward(context["farm_width"])
      context["turtle"].left(90)
  context["turtle"].end_fill()
  return "A"



def draw_market_district(context):
  print(context["farm_width"])
  context["turtle"].penup()
  context["turtle"].goto(context["turtle"].xcor() - context["farm_width"] / 2, context["turtle"].ycor())
  context["turtle"].pendown()
  context["turtle"].fillcolor('red')
  context["turtle"].begin_fill()
  for _ in range(4):
      context["turtle"].forward(context["farm_width"])
      context["turtle"].left(90)
  context["turtle"].end_fill()
  return "M"



def draw_central_district(context):
  print(context["farm_width"])
  context["turtle"].penup()
  context["turtle"].goto(context["turtle"].xcor() - context["farm_width"] / 2, context["turtle"].ycor())
  context["turtle"].pendown()
  context["turtle"].fillcolor('yellow')
  context["turtle"].begin_fill()
  for _ in range(4):
      context["turtle"].forward(context["farm_width"])
      context["turtle"].left(90)
  context["turtle"].end_fill()
  return "C"


def draw_living_district(context):
  print(context["farm_width"])
  context["turtle"].penup()
  context["turtle"].goto(context["turtle"].xcor() - context["farm_width"] / 2, context["turtle"].ycor())
  context["turtle"].pendown()
  context["turtle"].fillcolor('blue')
  context["turtle"].begin_fill()
  for _ in range(4):
      context["turtle"].forward(context["farm_width"])
      context["turtle"].left(90)
  context["turtle"].end_fill()
  return "L"