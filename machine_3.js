// evaluator

//libs for evaluator
function setup(){
	var env = {};
	env.lessthan = function(left,right){
		return left < right;
	}
	env.equal = function(left,right){
		return left == right;
	}
	env.bigthan = function(left,right){
		return left > right;
	}
	env.add = function(left,right){
		return left + right;
	}
	env.rem = function(a,b){
		return a % b;
	}
	env.mul = function(a,b){
		return a * b;
	}
	env.sub = function(a,b){
		return a - b;
	}
	
	return env;
}

//
var libs = setup();
var machine_code = 'hello;';


