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
function gen(info){
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
				if(text.length ==0) break;
				text = text.substring(1);
				ch = text[0];
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
				   
				} else{				
					stack.push(token);
				}
				
		}while(token != null )

	}
	
		function  applyv(proc){
		// return an object, as the parse result
		 // pop out operator, 	 	   
		   var obj = {};
		  
		   var op = proc.pop();
		   switch(op){
			 case 'if':
				 obj.type = 'if';
				 obj.pred = proc.pop();
				 obj.conseq = proc.pop(); 
				 obj.alt = proc.pop();
				 break;
			 case 'begin':
				 obj.type = 'begin';
				 obj.exps = proc.pop();
				 break;		 
			 case 'lambda':
				 obj.type = 'lambda';
				 obj.parameters = proc.pop();
				 obj.body = proc.pop();
				 break;
			 case 'application':
				 obj.type ='application';
				 obj.operator = proc.pop();
				 obj.oprands = proc.pop();
				 obj.env = proc.pop();
				 break;
			 case 'define':
				 obj.type ='definition';
				 obj.variable = proc.pop();
				 obj.value = proc.pop();
				 break;
			 case 'set!':
				 obj.type = 'assign';
				 obj.variable = proc.pop();
				 obj.value = proc.pop();
				 break;
			 case '\'':
				 obj.type ='quote';
				 obj.text = proc.pop();
				 break;
			 case 'variable':
				 obj.type = 'variable';
				 obj.value = proc.pop();
				 break;
			 case 'self':
				 obj.type = 'self';
				 obj.value = proc.pop();
				 break;			 
		   default:
				break;	   
		   }  
		   
		   return  obj; 	   	   
	}
	
	parse();	
	var result = stack.pop();
	return result;
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
		return exp.type ==='define';
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
	env.quote_text = function(exp){
		return exp.value;
	}
	env.lookup_var_env = function(variable,env){
		// lookup variable in the env object.
		for(var i=0; i< env.length;i++){
			var frame = env[i];    
			var keys = Object.keys(frame);
			for(var j=0; j< keys.length;j++){
				if(keys[j] === variable){
					return frame[keys[j]];
				}
			}
		}
		return 'undefined';
	}
	return env;
}


//
var prims = setup_eval();
var parser = setup_parser();
var libs = Object.assign({},parser,prims);
var machine_code1 = 
'(assign n (const 4));\
(assign b (const 7));\
(assign continue (label done));\
expt_begin;\
(test (op equal) (reg n) (const 0));\
(branch (label base_case));\
(save n);\
(save continue);\
(assign n (op sub) (reg n) (const 1));\
(assign continue (label after_expt));\
(goto (label expt_begin));\
base_case;\
(assign val (const 1));\
(goto (reg continue));\
after_expt;\
(restore continue);\
(restore n);\
(assign val (op mul) (reg val) (reg b));\
(goto (reg continue));\
done';

var machine_code2=
'begin;\
(assign product (const 1));\
(assign n (const 4));\
(assign  b (const 7));\
(assign counter (reg n));\
expt_iter;\
(test (op equal) (reg counter) (const 0));\
(branch (label done));\
(assign counter (op sub) (reg counter) (const 1));\
(assign  product (op mul) (reg b) (reg product));\
(goto (label expt_iter));\
done';

var machine_code3=
'(assign n (const 6));\
(assign continue (label done));\
fib_begin;\
(test (op lessthan) (reg n) (const 2));\
(branch (label base_case));\
(save continue);\
(assign continue (label para1_done));\
(save n);\
(assign n (op sub) (reg n) (const 1));\
(goto (label fib_begin));\
para1_done;\
(restore n);\
(save val);\
(assign n (op sub) (reg n) (const 2));\
(assign continue (label para2_done));\
(goto (label fib_begin));\
para2_done;\
(assign t (reg val));\
(restore val);\
(restore continue);\
(assign val (op add) (reg t) (reg val));\
(goto (reg continue));\
base_case;\
(assign val (reg n));\
(goto (reg continue));\
done;\
';

var machine_code =
'eval_dispatch;\
(test (op self_eval00) (reg  exp));\
(branch  (label eval_self));\
(test (op variable00) (reg exp));\
(branch (label eval_variable));\
(test (op quoted00) (reg exp));\
(branch (label eval_quote));\
(test (op assign00) (reg exp));\
(branch (label eval_assgin));\
(test (op define00) (reg exp));\
(branch (label eval_define));\
(test (op if00) (reg exp));\
(branch (label eval_if));\
(test (op lambda00) (reg exp));\
(branch (label eval_lambda));\
(test (op begin00) (reg exp));\
(branch (label eval_begin));\
(test (op application00) (reg exp));\
(branch (label eval_application));\
eval_self;\
(assign val (reg exp));\
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
(test (op no_operands00) (reg unev));\
(branch (label apply_dispatch));\
(save proc);\
ev_app_oprand_loop;\
(save argl);\
(assign exp (op first_oprand) (reg unev));\
(test (op last_oprand00) (reg unev));\
(branch (label ev_app_last_arg));\
(save env);\
(save unev);\
(assign continue (label ev_app_accumulate));\
(goto (label ev_dispatch));\
ev_app_accumulate;\
(restore unev);\
(restore env);\
(restore argl);\
(assign argl (op adjoin_arg) (reg val) (reg argl));\
(assign unev (op rest_operands) (reg unev));\
(goto (label ev_app_oprand_loop));\
ev_app_last_arg;\
(assign continue (label ev_app_accum_last_arg));\
(goto (label ev_dispatch);\
ev_app_accum_last_arg;\
(restore argl);\
(assign argl (op adjoin_arg) (reg val) (reg argl));\
(restore proc);\
(goto (label apply_dispatch));\
apply_dispatch;\
(test (op prim_procedure00) (reg proc));\
(branch (label apply_prim));\
(test (op compound_procedure00) (reg proc));\
(branch (label apply_compound));\
(goto (label unknown_procedure));\
prim_procedure;\
(assign val (op apply_prim_procedure) (reg proc) (reg argl));\
(restore continue);\
(goto (reg continue));\
compound_procedure;\
(assign unev (op procedure_parameters) (reg proc));\
(assign env (op procedure_env) (reg proc));\
(assign env (op extend_env) (reg unev) (reg argl) (reg proc));\
(assign unev (op procedure_body) (reg proc));\
(goto (label ev_sequence));\
ev_begin;\
(assign unev (op begin_actions) (reg exp));\
(save continue);\
(goto (label ev_sequence));\
ev_sequence;\
(assign exp (op first_sequence) (reg unev));\
(test (op last_exp00) (reg unev));\
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
(goto (label ev_dispatch));\
ev_if_conseq;\
(assign exp (op if_conseq) (reg exp));\
(goto (label ev_dispatch));\
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
(perform (op set_var_value!) (reg unev) (reg val) (reg env));\
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
(perform (op define_variable!) (reg unev) (reg val) (reg env));\
(assign val (const ok));\
(goto (reg continue));\
done;\
'

