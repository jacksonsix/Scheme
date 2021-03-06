// evaluator


///////////////////////////////////////////////////////////  memory management //////////////////////////////////////////////
////////// car,cdr ,cons treat as prim procedure. It is in a higher level than machine code ///

function Memory(){
	var  the_cars = [];
	var  the_cdrs = [];
	var  new_cars = [];
	var  new_cdrs = [];
	var  free =0;
	var  scan = 0;
	var  root = 0;
	var  newreg = 0;
	var  old = root;
	var  the_stack = null;

	function vect_ref(vect,offset){
		 return vect[offset];
	}
	function vect_set(vect,offset,value){
		vect[offset] = value;
	}

	this.car = function  car(data){
		if(data[0] ==='p'){
			var i =  parseInt(data.slice(1));
			return  the_cars[i];
		}
		return null;
	}

	this.cdr = function cdr(data){
			if(data[0] ==='p'){
			var i = parseInt(data.slice(1));
			return  the_cdrs[i];
		}
		return null;
	}

	this.set_car = function set_car(reg,value){
		if(reg[0] ==='p'){
			var i =  parseInt(reg.slice(1));
			the_cars[i] = value;
		}else{
			return null;
		}	
	}
	this.set_cdr = function set_cdr(reg,value){
		if(reg[0] ==='p'){
			var i =  parseInt(reg.slice(1));
			the_cdrs[i] = value;
		}else{
			return null;
		}	
	}

	this.cons = function cons(x,y){
		the_cars[free] = x;
		the_cdrs[free] = y;
		var index = 'p' + free;
		free++;
		return index;
	}
	function inner(address){
			var result ='';
			if(address[0] ==='p'){
				var i =  parseInt(address.slice(1));
				result = '(' + inner(the_cars[i])  +',' + inner(the_cdrs[i])+')'; 
			}else{
				result = address;
			}
			return result;
	}
	this.display = inner;

	function init_stack(){
		the_stack = null;
	}

	function stack_push(val){
		var n = cons(val,the_stack);
		the_stack = n;
	}

	function stack_pop(){
		var element = car(the_stack);
		the_stack = cdr(the_stack);
		return element;
	}


	// garbage collection
	this.collect = function stop_and_copy(){
		free = 0;
		scan = 0;
		old = root = 'p2';
		relocate_to_new(old);
		root = newreg;
		copy_loop();
		gc_flip();
	}

	function copy_loop(){
		var i = 0;
		while(free != scan  && i< 50){
			old = vect_ref(new_cars,scan);
			relocate_to_new(old);
			vect_set(new_cars,scan,newreg);
			old = vect_ref(new_cdrs,scan);	
			relocate_to_new(old);
			vect_set(new_cdrs,scan, newreg);
			scan += 1;	
			i++;
		}
	}

	function gc_flip(){
		var tmp = the_cars;
		the_cars = new_cars;
		new_cars = tmp;
		tmp = the_cdrs;
		the_cdrs = new_cdrs;
		new_cdrs = tmp;
	}

	function brokenheart(val){
		return val === 'broken';
	}

	function relocate_to_new(oldvalue){
		if(oldvalue[0] === 'p'){
			var oldindex = oldvalue.slice(1);
			var oldcr = vect_ref(the_cars,oldindex);
			if(brokenheart(oldcr)){
				newreg = vect_ref(the_cdrs,oldindex);
			}else{
				var newindex =  free;
				newreg = 'p' + newindex;
				free += 1;
				vect_set(new_cars,newindex,oldcr);
				oldcr = vect_ref(the_cdrs,oldindex);
				vect_set(new_cdrs,newindex,oldcr);
				vect_set(the_cars,oldindex,'broken');
				vect_set(the_cdrs,oldindex,newreg);
				
			}
		}else{
			newreg = oldvalue;
		}
	}

	
}



//libs for evaluator
//prim procedures in environment
function setup_global_environment(){
	var env = {};
	var memory = new Memory();
	env.cons = memory.cons;
	env.car = memory.car;
	env.cdr = memory.cdr;
	env.set_car = memory.set_car;
	env.set_cdr = memory.set_cdr;
	env.display = memory.display;
	
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
		return parseFloat(left) + parseFloat(right);
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
	// convert  to procedure  object
	Object.keys(env).forEach(function(funcName){
		 var obj ={};
		 obj.type = 'prim';
		 obj.func = env[funcName];
		 env[funcName] = obj;
	});
	env.false = false;  // false as vaiable , its value is false
	env.true = true;
	return env;
}

function jsmapping(shortName){
	var longName=shortName;
	switch(shortName){
		case '+':
		longName='add';
		break;
		case '-':
		longName='sub';
		break;
		case '*':
		longName='mul';
		break;
		case '%':
		longName='rem';
		break;
		case '<':
		longName='lessthan';
		break;
		case '>':
		longName='bigthan';
		break;	
		case '=':
		longName='equal';
		break;			
		default:
		break;
	}
	return longName;
}

function setup_parser(){
var env = {};
env.read_exp_text = 
    // parser to  instruction object
	function read_exp_text(scheme_text){			
		var commands = [];
		// assume each command on each line;
		var line ='';
		for(var i=0; i< scheme_text.length;i++){
			line = scheme_text[i];
			var inst =  gen(line);
			inst.cmdtext = line;
			commands.push(inst);		
		}
		return commands;	
	}

// generate proc
env.gen  = function (info){
	var text = info;
	var seprator = ['(',')',' '];
	var stack =[];
	
	function getNextToken(){
		//eat up space	
		while(text.length>0 && (text[0] ===' ' || text[0] ==='\t')){
			text = text.substring(1);
		}
		if(text.length == 0) return null;
		var ch = text[0];
		if(seprator.indexOf(ch) != -1){
			text = text.substring(1);
			return ch;
		}else{
			var word ='';
			while(seprator.indexOf(ch) == -1){
				word += ch;				
				if(text.length ==0 ){
					break;
				}else if(text.length ==1 ){
					text = '';
					break;
				}else{
					text = text.substring(1);				
				    ch = text[0];
				} 
			}
			return word;
		}
	}

	function parse(){
		var token ='';		
		do{
				token = getNextToken();
				// return the last element on stack ?
				if(token == null) return;
				
				if(token ==='('){
				   // push to stack
				   // the first token is operator				 
				   stack.push(token);				   
				} else if(token===')'){
				   // begin to pop until operator, close
				   // then  apply operator on operands
				   var out = stack.pop();
				   var proc = [];
				   while(out !== '('){		
						proc.push(out);
						out = stack.pop();
				   }
				   var result = applyv(proc);
				   stack.push(result);
				   
				}else if(token[0] =='\''){
					var proc = [];					
					proc.push(token.slice(1));	
					proc.push(token[0]);					
					var result = applyv(proc);
					stack.push(result);
				} else{				
					stack.push(token);
				}
				
		}while(token != null )

	}
		function isNumeric(n) {
		  return !isNaN(parseFloat(n)) && isFinite(n);
		}
		function  applyv(proc){
		// return an object, as the parse result
		 // pop out operator, 	 	   
		 // apply again in case number, vaiable, check here
		 if(typeof(proc) =='object'){
			 if(proc.type){
				 return proc;  // already parsed
			 }
		 }
  		  var obj = {};
		  if(!Array.isArray(proc)){
			  // expression has only 1 part. ,  self_eval, variable.
			  if(isNumeric(proc)  ){
				  obj.type = 'self';
				  obj.value = parseFloat(proc);
			  }else{
				  obj.type = 'variable';
				  obj.value = proc;
			  }
			  
		  }else{
			   var op = proc.pop();
			   switch(op){
				 case 'if':
					 obj.type = 'if';
					 obj.pred = applyv(proc.pop());
					 obj.conseq = applyv(proc.pop()); 
					 obj.alt = applyv(proc.pop());
					 break;
				 case 'begin':
					 obj.type = 'begin';
					 obj.exps =[];
					 while(proc.length>0){
						 var p = proc.pop();
						 obj.exps.push(applyv(p));
					 }				 
					 break;		 
				 case 'lambda':
					 obj.type = 'lambda';
					 var tmp = proc.pop();
					 if(tmp.type ==='application'){
						 var ta =[];
						 ta.push(tmp.operator);
						 while(tmp.oprands.length>0){
							 ta.unshift(tmp.oprands.pop());  // keep the same sequence
						 }
						obj.parameters = ta;
					 }else{
						obj.parameters = proc.pop();
					 }					 
					 obj.body =[];
					 while(proc.length>0){
						 // check  exception: if exists  a string , as a single expression 
						 var exp = proc.pop();
						 if(typeof(exp) ==='string'){
							 var  v ={};
							 v.type='variable';
							 v.value = exp;
							 obj.body.push(v);
						 }else{
							  obj.body.push(exp);
						 }						
					 }					 
					 // for each expression
					 break;				 
				 case 'define':
					 obj.type ='definition';
					 obj.variable = proc.pop();
					 // test if variable part is application object, which means it is a definition of procedure
					 if(obj.variable.type && obj.variable.type ==='application'){
						 var tmp = obj.variable;
						 obj.variable = tmp.operator;
						 var convert ={};
						 convert.type ='lambda';
						 convert.parameters = [];
						 while(tmp.oprands.length>0){
							 convert.parameters.unshift(tmp.oprands.pop()); // keep the same sequence
						 }
						 convert.body = [];
						 while(proc.length>0){
							 convert.body.unshift(applyv(proc.pop()));  // keep the same sequence
						 }
						 obj.value = applyv(convert);
						 
					 }else{
						  obj.value = applyv(proc.pop());
						  var t = obj.variable;
                          obj.variable={};						 
						  obj.variable.value = t;
					 }					 
					 break;
				 case 'set!':
					 obj.type = 'assign';
					 obj.variable = proc.pop();
					 // test if variable part is application object, which means it is a definition of procedure
					 if(obj.variable.type && obj.variable.type ==='application'){
						 var tmp = obj.variable;
						 obj.variable = tmp.operator;
						 var convert ={};
						 convert.type ='lambda';
						 convert.parameters = [];
						 while(tmp.oprands.length>0){
							 convert.parameters.unshift(tmp.oprands.pop());  // keep the same sequence
						 }
						 convert.body = [];
						 while(proc.length>0){
							 convert.body.unshift(applyv(proc.pop())); // keep the same sequence
						 }
						 obj.value = applyv(convert);
						 
					 }else{
						 obj.value = applyv(proc.pop());						
					 }	
					 break;
				 case '\'':
					 obj.type ='quote';
					 obj.text = proc.pop();
					 break;		 
			   default:   // default as application (operator) (operator operands) , but parameters (para1)  or (para1,para2) 
			         obj.type ='application';
					 obj.operator = applyv(jsmapping(op));
					 obj.oprands = [];
					 while(proc.length>0){
						 var p = proc.pop();
						 obj.oprands.push(applyv(p));
					 }						
					break;	   
			   }  
		  }	
		  return  obj; 	   	   
	}
	
	parse();	
	var result = stack.pop();
	return applyv(result);
}

  return env;	
}


function setup_eval(){
	var env={};
	env.self_eval00 = function(exp){
		return  exp.type === 'self';
	}
	env.variable00 = function(exp){
		return exp.type ==='variable';
	}
	env.quoted00 = function(exp){
		return exp.type ==='quote';
	}
	env.assign00 = function(exp){
		return exp.type ==='assign';
	}
	env.define00 = function(exp){
		return exp.type ==='definition';
	}
	env.if00 = function(exp){
		return  exp.type ==='if';
	}
	env.lambda00 = function(exp){
		return exp.type ==='lambda';
	}	
	env.begin00 = function(exp){
		return exp.type ==='begin';
	}	
	env.application00 = function(exp){
		return exp.type ==='application';		
	}	
	env.self_value = function(exp){
		return exp.value;
	}
	env.quote_text = function(exp){
		return exp.text;
	}
	
	function search_variable(variable,env){
		for(var i=0; i< env.length;i++){
			var frame = env[i];    
			var keys = Object.keys(frame);
			for(var j=0; j< keys.length;j++){
				if(keys[j] === variable){
					return frame;
				}
			}
		}
		return null;
	}
	env.lookup_var_env = function(variable,env){		
		var frame = search_variable(variable.value,env);
		if(frame != null){
			return frame[variable.value]
		}else{
			return 'undefined';
		}		
	}
	env.define_var = function(exp){
		return exp.variable;
	}
	env.define_value = function(exp){
		return exp.value;
	}
	env.define_variable = function(variable,val,env){
		var frame = env[0];		
		frame[variable.value] = val;
		if(val.type ==='procedure'){  // modify  procedure env
			val.env[0] = frame;
		}
		return  'ok';
	}
	env.assign_var = function(exp){
		return exp.variable;
	}
	env.assign_value = function(exp){
		return exp.value;
	}
	env.set_var_value = function(variable,val,env){
		var frame = search_variable(variable,env);		
		frame[variable] = val;
		return  'ok';
	}
	env.begin_actions = function(exp){
		return exp.exps;
		
	}
	env.first_sequence = function(sequence){
		// expression should be evaluate to object
		if(sequence.length>0){
			return sequence[0];
		}else{
			return null;
		}
	}
	env.is_last_exp = function(sequence){
		return sequence.length == 1;
	}
	env.rest_sequence = function(sequence){
		if(sequence.length >1){
			return sequence.slice(1);
		}else{
			return null;
		}
	}
	env.lambda_parameters = function(exp){
		return exp.parameters;
	}
	env.lambda_body  = function(exp){
		return exp.body;
	}
	env.make_procedure = function(paras,body,env){
		var procedure = {};
		procedure.parameters = paras;
		procedure.body = body;
		//procedure.env = env;  // this will create cycle. break it.
		//procedure.env = JSON.stringify(env);   // stringfy does not work for function
		var copy = []; // copy each frame
		for(var i=0;i<env.length;i++){
			var frame = env[i];
			var cf = {};
			Object.keys(frame).forEach(function(key){
				cf[key] = frame[key];				
			});
			copy.push(cf);
		}
		procedure.env = copy;
		procedure.type = 'procedure';
		return procedure;
	}
	env.procedure_parameters = function(procedure){
		return procedure.parameters;
	}
	env.procedure_body = function(procedure){
		return procedure.body;
	}
	env.procedure_env = function(procedure){		
		return procedure.env;
	}
	env.is_no_operands = function(sequence){
		if(sequence == null || sequence.length<1){
			return true;
		}
		return false;
	}
	env.empty_list = function(){
		var s = [];
		return s;
	}
	env.extend_env = function(parameters,argl,env){
		var newframe = {};
		for(var i=0;i< parameters.length;i++){
			var para = parameters[i];
			newframe[para.value] = argl[i];
		}
		env.unshift(newframe);
		return env;
	}
	env.adjoin_arg = function(val, argl){
		argl.push(val);
		return argl;
	}
	function inEnvLibs(name,env){
		var keys = Object.keys(env);
		for(var i=0;i< keyslength;i++){
			var proc = keys[i];
			if(proc === name) return true;
		}
		return false;
	}
	env.is_prim_procedure = function(proc){		
		if(proc.type ==='prim'){
			return true;
		}else{
			return false;
		}		
	}
	env.is_compound_procedure = function(proc){
		if(proc.type ==='prim'){
			return false;
		}else{
			return true;
		}	
	}
	env.apply_prim_procedure = function(proc,argl){
		// test if the proc is a function name only as string		
		return proc.func.apply(this, argl);		
	}
	env.app_oprands = function(exp){
		return exp.oprands;
	}
	env.app_operator = function(exp){	
		return  exp.operator;
	}
	
	env.if_conseq = function(exp){
		return exp.conseq;
	}
	env.if_pred = function(exp){
		return exp.pred;
	}
	env.if_alt = function(exp){
		return exp.alt;
	}
	
	env.true00 = function(val){
		return val !== false;
	}
	
	return env;
}


//
var prims = setup_eval();
var parser = setup_parser();
var libs = Object.assign({},parser,prims);

var machine_code =
'eval_dispatch;\
(test (op self_eval00) (reg  exp));\
(branch  (label eval_self));\
(test (op variable00) (reg exp));\
(branch (label eval_variable));\
(test (op quoted00) (reg exp));\
(branch (label eval_quote));\
(test (op assign00) (reg exp));\
(branch (label ev_assign));\
(test (op define00) (reg exp));\
(branch (label ev_define));\
(test (op if00) (reg exp));\
(branch (label ev_if));\
(test (op lambda00) (reg exp));\
(branch (label eval_lambda));\
(test (op begin00) (reg exp));\
(branch (label ev_begin));\
(test (op application00) (reg exp));\
(branch (label eval_application));\
eval_self;\
(assign val (op self_value) (reg exp));\
(goto (reg continue));\
eval_variable;\
(assign val (op lookup_var_env) (reg exp) (reg env));\
(goto (reg continue));\
eval_quote;\
(assign val (op quote_text) (reg exp));\
(goto (reg continue));\
eval_lambda;\
(assign unev (op lambda_parameters) (reg exp));\
(assign exp (op lambda_body) (reg exp));\
(assign val (op make_procedure) (reg unev) (reg exp) (reg env));\
(goto (reg continue));\
eval_application;\
(save continue);\
(save env);\
(assign unev (op app_oprands) (reg exp));\
(save unev);\
(assign exp (op app_operator) (reg exp) );\
(assign  continue (label ev_app_did_operator));\
(goto (label eval_dispatch));\
ev_app_did_operator;\
(restore unev);\
(restore env);\
(assign argl (op empty_list));\
(assign proc (reg val));\
(test (op is_no_operands) (reg unev));\
(branch (label apply_dispatch));\
(save proc);\
ev_app_oprand_loop;\
(save argl);\
(assign exp (op first_sequence) (reg unev));\
(test (op is_last_exp) (reg unev));\
(branch (label ev_app_last_arg));\
(save env);\
(save unev);\
(assign continue (label ev_app_accumulate));\
(goto (label eval_dispatch));\
ev_app_accumulate;\
(restore unev);\
(restore env);\
(restore argl);\
(assign argl (op adjoin_arg) (reg val) (reg argl));\
(assign unev (op rest_sequence) (reg unev));\
(goto (label ev_app_oprand_loop));\
ev_app_last_arg;\
(assign continue (label ev_app_accum_last_arg));\
(goto (label eval_dispatch));\
ev_app_accum_last_arg;\
(restore argl);\
(assign argl (op adjoin_arg) (reg val) (reg argl));\
(restore proc);\
(goto (label apply_dispatch));\
apply_dispatch;\
(test (op is_prim_procedure) (reg proc));\
(branch (label prim_procedure));\
(test (op is_compound_procedure) (reg proc));\
(branch (label compound_procedure));\
(goto (label unknown_procedure));\
prim_procedure;\
(assign val (op apply_prim_procedure) (reg proc) (reg argl));\
(restore continue);\
(goto (reg continue));\
compound_procedure;\
(assign unev (op procedure_parameters) (reg proc));\
(assign env (op procedure_env) (reg proc));\
(assign env (op extend_env) (reg unev) (reg argl) (reg env));\
(assign unev (op procedure_body) (reg proc));\
(goto (label ev_sequence));\
ev_begin;\
(assign unev (op begin_actions) (reg exp));\
(save continue);\
(goto (label ev_sequence));\
ev_sequence;\
(assign exp (op first_sequence) (reg unev));\
(test (op is_last_exp) (reg unev));\
(branch (label ev_sequence_last));\
(save unev);\
(save env);\
(assign continue (label  ev_sequence_continue));\
(goto (label eval_dispatch));\
ev_sequence_continue;\
(restore env);\
(restore unev);\
(assign unev (op rest_sequence) (reg unev));\
(goto (label ev_sequence));\
ev_sequence_last;\
(restore continue);\
(goto (label eval_dispatch));\
ev_if;\
(save exp);\
(save env);\
(save continue);\
(assign continue (label ev_if_decide));\
(assign exp (op if_pred) (reg exp));\
(goto (label eval_dispatch));\
ev_if_decide;\
(restore continue);\
(restore env);\
(restore exp);\
(test (op true00) (reg val));\
(branch (label ev_if_conseq));\
(assign exp (op if_alt) (reg exp));\
(goto (label eval_dispatch));\
ev_if_conseq;\
(assign exp (op if_conseq) (reg exp));\
(goto (label eval_dispatch));\
ev_assign;\
(assign unev (op assign_var) (reg exp));\
(save unev);\
(save env);\
(save continue);\
(assign continue (label assign_continue));\
(assign exp (op assign_value) (reg exp));\
(goto (label eval_dispatch));\
assign_continue;\
(restore continue);\
(restore env);\
(restore unev);\
(perform (op set_var_value) (reg unev) (reg val) (reg env));\
(assign val (const ok));\
(goto (reg continue));\
ev_define;\
(assign unev (op define_var) (reg exp));\
(save unev);\
(save env);\
(save continue);\
(assign continue (label define_continue));\
(assign exp (op define_value) (reg exp));\
(goto (label eval_dispatch));\
define_continue;\
(restore continue);\
(restore env);\
(restore unev);\
(perform (op define_variable) (reg unev) (reg val) (reg env));\
(assign val (const ok));\
(goto (reg continue));\
done;\
'

