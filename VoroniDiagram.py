# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 13:51:52 2023

@author: Afromullet
"""
import pygame
import numpy as np
from scipy.spatial import Voronoi

WHITE = (255, 255, 255)
BLACK = (0, 0, 0)

def generate(screen_width,screen_height):
    np.random.seed(0)
    points = np.random.rand(30, 2) * (screen_width,screen_height)
    vor = Voronoi(points)
    return vor,points

def get_pair_polygons_tuple(vor):
    
    regions = []
    # Draw Voronoi regions
    for region in vor.regions:
        if not -1 in region and len(region) > 0:
            polygon = [vor.vertices[i] for i in region]
            #Calculate the center (centroid) of the region
            center_x = sum(p[0] for p in polygon) / len(polygon)
            center_y = sum(p[1] for p in polygon) / len(polygon)
            point = (center_x,center_y)
            
            regions.append((point,polygon))
            
            # Calculate the center (centroid) of the region
            #center_x = sum(p[0] for p in polygon) / len(polygon)
            #center_y = sum(p[1] for p in polygon) / len(polygon)
            #center_points.append((center_x, center_y))
            
            
    return regions
            

        
def draw_voroni(screen,region_polygons):
    
    for polygon_group in region_polygons:
        pygame.draw.polygon(screen, BLACK, polygon_group[1], 1)
        pygame.draw.circle(screen, BLACK, (int(polygon_group[0][0]), int(polygon_group[0][1])), 5)
   
