# -*- coding: utf-8 -*-
"""
Created on Tue Oct 17 16:32:18 2023

@author: Afromullet
"""



'''
Reads the L-System rules and stores them in a dictionary.

The key is the token, and the value is what the rule expands to 
'''
def read_rules(fname):
    
    rule_map = {}
    with open(fname) as rules:
        for row in rules:
            
            if row:
                row = row.split(":")
                
                rule_key = row[0].replace(" ","")
                rule_value = row[1].replace(" ","").strip("\n")
      
                rule_map[rule_key] = rule_value    
    return rule_map


'''
Generates the String that represents all generations.
'''
def apply_lsystem_rules(axiom, rules, generations):
    current_string = axiom

    for _ in range(generations):
        next_string = ""
        for char in current_string:
            if char in rules:
                next_string += rules[char]
            else:
                next_string += char
        current_string = next_string

    return current_string


'''
Gets the generation string
'''
def generate_rules(axiom,rule_filename,num_generations):
    rule_map = read_rules(rule_filename)
    return apply_lsystem_rules(axiom,rule_map,num_generations)

'''
Generates the String that represents all generations. This is for L-System rules that allow arguments
'''
def apply_lsystem_rules_with_args(axiom, rules, generations):
    current_string = axiom

    for _ in range(generations):
        next_string = ""
        for char in current_string:
            if char in rules:
                next_string += rules[char]
            else:
                next_string += char
        current_string = next_string

    return current_string

'''
Reads the L-System rules of a file that contains parametized rules
'''
def create_parametized_rules(file_path):
    rule_dict = {}
    
    with open(file_path, 'r') as file:
        for line in file:
            line = line.strip()
            if line:
                key, value = line.split(':', 1)
                rule_dict[key] = value
    
    return rule_dict


'''
Matches based off the first character...Not the best of ideas, but that should work for now

Compares the first character of the key to the first character of the parametized input and returns the key.

I.E,

key = F(a,b)
symbol_parameter = F(3,5)

Matches the F and then returns F(a,b)
'''
def get_parametized_key(rule_keys,symbol_parameters):
    
    for k in rule_keys:
        if k[0] == symbol_parameters[0]:
            return k

   
'''
Applies the L-System rules for a ruleset with parameters
'''
def apply_parametized_lsystem_rules(axiom, rules, generations):
    current_string = axiom
    symbol_parameters = "" #Used to store the parameters when we find parentheses 
    symbol = ""
    rule_keys = rules.keys()
    for i in range(generations):
        next_string = ""
        for char in current_string:
            
            if char == "(":
                symbol_parameters += char
            elif char == ")":
                symbol_parameters += ")"
           
                #Do parametized Rule lookup
                symbol_parameters = symbol + symbol_parameters
                param_key = get_parametized_key(rule_keys,symbol_parameters)
                
                #todo error checking for when key does not exist
                
                if param_key in rules:
                    next_string += rules[param_key]
                else:
                    next_string += symbol_parameters
                symbol_parameters = ""
                symbol = ""
            elif symbol_parameters:
                symbol_parameters+= char
                
            
            if not symbol_parameters and char != "(" and char != ")":
                
                if char in rules:
                    next_string += rules[char]
                else:
                    next_string += char
                symbol = char
        current_string = next_string

    return current_string


