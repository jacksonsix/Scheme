// evaluator

//libs for evaluator
//prim procedures in environment
function setup_global_environment(){
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
	return env;
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
				  obj.value = proc;
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
							 ta.push(tmp.oprands.pop());
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
					 obj.value = applyv(proc.pop());
					 break;
				 case 'set!':
					 obj.type = 'assign';
					 obj.variable = proc.pop();
					 obj.value = applyv(proc.pop());
					 break;
				 case '\'':
					 obj.type ='quote';
					 obj.text = proc.pop();
					 break;		 
			   default:   // default as application (operator) (operator operands) , but parameters (para1)  or (para1,para2) 
			         obj.type ='application';
					 obj.operator = op;
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
		frame[variable] = val;
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
		procedure.env = env;
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
			newframe[para] = argl[i];
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
		// if operator is lambda expression. it will go eval . but if that is 
		// a name for prim, or compound procedrue defined before?
		// transform in lambda format
		//var proc = {};
		//proc.type = 'lambda';
		//proc.name = exp.operator;
		//proc.note = 'defined';
		//return  proc;
		
		// as variable, lookup in environment 
		var o = {};
		o.type='variable';
		o.value = exp.operator;
		return  o;   // this is a procedure name as string
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
(branch (label ev_if_conseq);\
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

