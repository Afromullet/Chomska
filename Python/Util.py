# -*- coding: utf-8 -*-
"""
Created on Sat Oct 21 07:10:23 2023

@author: Afromullet
"""

import re

'''
LOL

Using exec to convert the parameters to variables that are returned.

Is a security risk
'''
def get_parameters(param_string):
    
    matches = re.findall(r'\w+=\d+', param_string)
    print(matches)
    
    variables = {}
    for var_string in matches:
        try:
            exec(var_string, variables)
        except SyntaxError as e:
            print(f"Failed to create variable: {e}")
    return variables
    
    
