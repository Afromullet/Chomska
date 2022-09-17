'use strict';


class TurtleDraw
{
	constructor(s,l,t){
		this.todo = s;
		this.len = l;
		this.theta = t;
	}
	
  render () {
	  
    stroke(255);
	console.log(this.todo)
    for (var i = 0; i < this.todo.length; i++) {
      var c = this.todo.charAt(i);

      if (c === 'F' || c === 'G') {
        line(0,0,this.len,0);
        translate(this.len,0);
      }
      else if (c === '+') {

        rotate(this.theta);
      }
      else if (c === '-') {
        rotate(-this.theta);
      }
      else if (c === '[') {
        push();
      }
      else if (c === ']') {
        pop();
      }
    }
  };

  setLen (l) {
    this.len = l;
  };

 changeLen (percent) {
    this.len *= percent;
  };

  setToDo (s) {
    this.todo = s;
  };
}


export 
{
	TurtleDraw 


	
};
