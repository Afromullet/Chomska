
import * as GrammarModule from './ReadGrammar.js';
import * as turt from './Turtle.js';

var startSymbol = ['F']

/*
Setting up the data we need to parse the L-System
*/
var total_generations = 100;
var symbols = ""
//var grammarFunc =  GrammarModule.ReadLSystemRulesFromString("F: FF+[+F-F-F]-[-F+F+F]");

var grammarFunc =  GrammarModule.ReadLSystemRulesFromString("F:  F=FF++F++F+F++F-F");

var grammarGenerator = ""
var turtle = ""
var counter = 0

function setup() {
 createCanvas(600, 600);
 grammarGenerator = GrammarModule.GetNextGeneration(startSymbol,grammarFunc,total_generations) //Create the generator
 //turtle =  new turt.TurtleDraw(symbols, height/3, radians(25));
 turtle =  new turt.TurtleDraw(symbols, height/3, radians(60));
}

function draw() {
  background(51);
  fill(0);
  text("Click mouse to generate", 10, height-10);

  translate(width/2, height);
  rotate(-PI/2);
  turtle.render();
}


function mousePressed() {
	

  if (counter < 5) {
    push();
	
	symbols = grammarGenerator.next().value;
	symbols = symbols.join("")
	console.log('P')
    //println(lsys.getSentence());
	//console.log(symbols)
    turtle.setToDo(symbols);
    turtle.changeLen(0.5);
    pop();
    //redraw();
    counter++;
  }

}



window.setup = setup
window.draw = draw
window.mousePressed = mousePressed
