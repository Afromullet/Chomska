//import * as fs from 'fs';


/*
Reads the L-System rules from the input file path and returns them as a dictionary
The key is the starting symbol and the value are the transformed symbols

Does not currently work in a browser due to using fs
*/
function ReadLSystemRules(ruleFilePath){
	const data = fs.readFileSync(ruleFilePath, 'utf8');
	

	var sep_rules = data.split("\n"); //Each index will be a transformation rules. Going to have to be split further for each rule
	var transformation_rules = {};
    sep_rules.forEach(rule => {
		
	var split_rule = rule.split(":");
		split_rule = split_rule.map(x => x.trim());
		transformation_rules[split_rule[0]] = split_rule[1];
	});	

	return transformation_rules
}


/*
Reads the L-System rules from the input string and returns them as a dictionary
The key is the starting symbol and the value are the transformed symbols
*/
function ReadLSystemRulesFromString(grammarDescription){


	var sep_rules = grammarDescription.split("\n"); //Each index will be a transformation rules. Going to have to be split further for each rule
	var transformation_rules = {};
    sep_rules.forEach(rule => {
		
	var split_rule = rule.split(":");
		split_rule = split_rule.map(x => x.trim());
		transformation_rules[split_rule[0]] = split_rule[1];
	});	

	return transformation_rules
}

/*
Applies the rules to an entire line and then returns the symbols as an array.

For example:

A -> BB
B-> C
C -> DD


Input("A") returns ["BB"]
Input("AA") returns ["BB","BB"]
Input("AADD") returns ["CC","CC", undefined, undefined]. Undefined means there are no more rules

*/
function ApplyRulesToLine(input_symbol,transformation_rules){

	
	var symbols = []
	var temp_symbol = []
	
	if (input_symbol == undefined)
	{
		return undefined
	}
	
	
	//console.log(input_symbol)
	input_symbol.split('').forEach(s=>
	{
		temp_symbol = transformation_rules[s];
		
		//If there is no rule for a symbol, just use the symbol itself. 
		if (temp_symbol == undefined){
			symbols = symbols.concat(s.split(''));
		}
		else
		{
			symbols = symbols.concat(temp_symbol.split(''));
		}
		
		
	});
	
	return symbols
}

/*
Takes as input an array of symbols
I.E, ["BB"] or ["BB","BB"]

If we get an undefined, then there are no more rules to evaluate.
We still need to add undefined to the array so we know when to stop
*/
function ApplyRulesToStringArray(symbol_array,transformation_rules)
{
	
	var transforms =   symbol_array.map(
		function(x) { return ApplyRulesToLine(x, transformation_rules); console.log("A");});
	
	var outputRules = []
	transforms.forEach(s => {
		outputRules = outputRules.concat(s);
		
	
	});	

	return outputRules;
}

/*
TransformationRules is a map, created by ReadLSystemRules
Axiom is the starting symbol, passed as an array. I.E ["S"]
*/
function StartEvaluation(axiom, transformationRules){
	var symbols = ApplyRulesToStringArray(axiom,transformationRules)
	return symbols
}

function* GetNextGeneration(symbols,transformationRules,numGenerations){	
	for(var i = 0; i < numGenerations; i++)
	{
		symbols = ApplyRulesToStringArray(symbols,transformationRules)
		yield symbols
	}
}

export 
{
	ReadLSystemRules,
	ApplyRulesToLine,
	ApplyRulesToStringArray,
	StartEvaluation,
	GetNextGeneration,
	ReadLSystemRulesFromString
};