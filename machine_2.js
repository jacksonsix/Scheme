//machine

//import objToString
//unwrap object till n layer.
function deepcopy(source){
	var keeprecord = [];
	var flagrecord =[];
	var index = 0;
	function copy(sr){
		if(keeprecord.indexOf(sr) != -1){
			return sr;
		}
		if(typeof(sr) !=='object'){
			return sr;
		}
		var dst;
		if(Array.isArray(sr)){
			dst =[];
			
		}else{	
	       dst ={};		
		}	
		
		keeprecord.push(sr);
		flagrecord.push('gray');	
		var keys = Object.keys(sr);
		keys.forEach(function(key){			
			var ele = sr[key];
			if(typeof(ele) ==='object'){
				dst[key] = copy(ele);
			}else{
				dst[key] = ele;
			}
		});				
		
        var i = keeprecord.indexOf(sr);
		flagrecord[i] = 'black';
		return dst;
	}
	var nc = copy(source);
	return nc;
}

function counter(){
	var index = 0;
	return function(){
		return index++;
	}
}
var count = counter();
function trace(data){
	console.log(data);
	tohtml(0,data);
}

 function tohtml(index,cmd,reg,stack){	
    var nreg='';
	var nstack='';
    cmd = cmd.replace('\n','</br>');
	 if(typeof(reg) !=='undefined')
	    nreg =reg.replace(/\n/g,'</br>');
	 if(typeof(stack) !=='undefined')
	    nstack =stack.replace(/\n/g,'</br>');
	var a = document.createElement('li');
	a.innerHTML=index +':  '+ cmd;
	a.innerHTML += '</br>' +index + ':regs: ' + nreg;
	a.innerHTML += '</br>' + index + ':stack: ' +nstack;
	document.getElementById('log').append(a);	
	
	//var b = document.createElement('li');
	//b.innerHTML = count() + reg;
	//document.getElementById('regs').append(b);	
 }
 var trace_options = {
	 command:true,
	 regs:false,
	 stack:false,
	 reg_filter:['exp','val']
 };
 
function objToString(obj,n){	
	var type = typeof(obj);
	var strValue ='';
	if(type =='object'){
		if(Array.isArray(obj)){
				strValue += '[ ';
				strValue += allKeys(obj, n);
				strValue += ' ]';	
			
		}else{
				strValue +=  '{ ';
				strValue += allKeys(obj, n);
				strValue +=  ' }';				
		}
		
	}else if(type ==='function'){
		strValue = 'user_function';		
	} 
	else{
		strValue = obj;
	}
	return strValue;
}

function allKeys(parent,n){
	if(parent == null) return '';
	
	var keys = Object.keys(parent);
	var elements='';
	keys.forEach(function(key){
		var ele = parent[key];
		if(ele!=null){
			if(n > 6){ 
			     return 'cutoff';
			}else{				
				var s = objToString(key,n+1) +':' +objToString(parent[key], n+1) +' , ';
				elements += s;
			}
                      			
		}
	});
	elements = elements.substring(0, elements.length -3);
	return elements;
}
//
function make_machine(reg_names){
	var _machine = new Machine_base();
	// augment machine with memory
	_machine.memory = new Memory();
	
	reg_names.forEach(function(reg){
		_machine.registers[reg] = 'unassigned';
	});	
	return _machine;
}

//
function Machine_base(){
  this.registers = {};
  this.pc = null;
  this.pcindex = 0;
  this.flag = false;
  this.stack = [];
  this.ops = [];
  this.controller_text ='';
  this.libs = {};
  this.counting = 0;
  this.trace = true;
  this.maxStack = 0;
  this.numPush = 0;
  this.debugstack = [];
  
  this.get_reg = function get_reg(reg){	 
	  return this.registers[reg];
  }
  
  this.set_reg = function set_reg(reg,value){
	  this.registers[reg] = value;
  }

  this.start = function start(){
	  console.log('Machine started!');
	  this.pc = this.ops[this.pcindex];
	  var i = 0;
	  while(this.pc != null && i < 1000){	   	
		i++;
		if(this.trace){
			var m ='';
			var keys = Object.keys(this.registers);
			for(var i = 0; i< keys.length;i++){
				var key = keys[i];
				//if(key==='env') continue;   //skip env
				if(trace_options.reg_filter.indexOf(key) ==-1) continue;
				var val = this.get_reg(key);
				if(val !== 'unassigned'){
					var  content =  this.get_reg(key);					
					m += '\n' +key +':' + objToString(content, 0) + ';';	
				}				
			}
			var sta ='';
			for(var i=0;i< this.stack.length;i++){
				sta += '\n' + objToString(this.stack[i],0)  + '--> ' + objToString(this.debugstack[i],0) ;
			}
			//print pc command
			var sn = count();
			console.log(sn + 'command: ' + this.pc.cmdtext);			
			console.log(sn+'registers:     ' +m);
			console.log(sn+'     '+ 'stack' + sta);
			tohtml( sn, this.pc.cmdtext,m,sta);
		}
		if(this.stack.length > this.maxStack){
			this.maxStack = this.stack.length;
		}
		this.pc.func();
		this.counting++;
	  }

     // after the execution, print the statistics
        console.log('Total steps: ' + this.counting);
        console.log('Max stack depth: ' + this.maxStack);	
        console.log('Num of push: ' + this.numPush);		

  }
  
  this.get_stack = function getstack(){ return this.stack;}
  this.get_operations = function getops(){return this.ops;}	
  this.set_flag = function setflag(value){ this.flag = value;}
  this.advance_pc = function advance_pc(machine){	                           
								  this.pcindex += 1;
								  this.pc =  this.ops[this.pcindex];
                             }	
  this.install_libs  = function(libs){
	                                this.libs = libs;
                               }							 
						 
   this.printcounting = function(){ return this.counting;}	
   this.resetcounting = function(){  this.counting = 0; }   
}


///////////////////////////// Assembler 
function asseble(controller_text,machine,libs){
	var commands = read_controller_text(controller_text);	
	var analyzer = analyze(commands);
	// install libs to machine
	//var libs = setup();	
	machine.install_libs(libs);
	machine.ops = update_insts(commands, machine);
}

function update_insts(insts,machine){
	insts.forEach(function(inst){
		inst.func = make_exe_proc(inst,machine);
	});
	return insts;
}

// from instruction to procedure
function make_exe_proc(inst,machine){
	var operator = inst.type;
	switch(operator){
		case 'assign':
		   return make_assign(inst,machine);
		break;
		case 'test':
		   return make_test(inst,machine);
		break;
		case 'branch':
		   return make_branch(inst,machine);
		break;
		case 'goto':
		   return make_goto(inst,machine);
		break;
		case 'save':
		   return make_save(inst,machine);
		break;
		case 'restore':
		   return make_restore(inst,machine);
		break;
		case 'perform':
		   return  make_perform(inst,machine);
		break;		
		case 'label':
		   return  make_label(inst,machine);
		break;	
		default:
		   console.log('cannot find inst ' + inst);
		break;
	}
}


// parser to  instruction object
function read_controller_text(controller_text){
	function isLabel(line){
		return line.indexOf('(') ==-1;
	}

	function make_label(text){
		var obj ={};
		obj.type ='label';
		obj.name = text;
		return obj;
	}	
	var commands = [];
	// assume each command on each line;
	var line ='';
	for(var i=0; i< controller_text.length;i++){
		line = controller_text[i];
		if(line == null) break;
		// check is label or inst
		if(isLabel(line)){			
			var label = make_label(line);
			label.cmdtext = line;
			commands.push(label);
		}else{
			var inst =  gen(line);
			inst.cmdtext = line;
			commands.push(inst);		
		}
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
			 case 'assign':
				 obj.type = 'assign';
				 obj.dest = proc.pop();
				 obj.src = proc.pop(); 
				 if(obj.src.type ==='operation'){
					 obj.params =[];
					 while(proc.length>0){
						 obj.params.push(proc.pop());
					 }
				 }
				 break;
			 case 'perform':
                 obj.type = 'perform';
				 obj.operator = proc.pop();
				 obj.params =[];
				 while(proc.length>0){
					 obj.params.push(proc.pop());
				 }
                 break;				 
			 case 'reg':
				 obj.type = 'reg';
				 obj.name = proc.pop();
				 break;		 
			 case 'label':
				 obj.type = 'label';
				 obj.name = proc.pop();
				 break;
			 case 'test':
				 obj.type ='test';
				 obj.operation = proc.pop();
				 obj.left = proc.pop();
				 obj.right = proc.pop();
				 break;
			 case 'const':
				 obj.type ='const';
				 obj.value = proc.pop();
				 break;
			 case 'op':
				 obj.type = 'operation';
				 obj.functionName = proc.pop();
				 break;
			 case 'branch':
				 obj.type ='branch';
				 obj.dest = proc.pop();
				 break;
			 case 'save':
				 obj.type = 'save';
				 obj.reg = proc.pop();
				 break;
			 case 'restore':
				 obj.type = 'restore';
				 obj.reg = proc.pop();
				 break;
			 case 'goto':
				 obj.type = 'goto';
				 obj.dest = proc.pop();
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


function make_operation_exp(exp,params,machine){
	var  opfunc = machine.libs[exp.functionName];
	var operands = params.map(function(oprand){
		 return make_primitive_exp(oprand,machine);
	});

	return function(){
		var ss = operands.map(function(p){
		                return p();
	       });
		return opfunc.apply(this, ss);
	}
	
}

function make_primitive_exp(exp, machine){
	var operation = exp.type;
	var result ={};
	switch(exp.type){
		case 'const':
		   result = function(){
                 return exp.value;			   
		    }
			break;
      case 'label':
          result  = function(){
			  return exp.name;
		  }	  
		  break;
	 case 'reg':
         result  = function(){
			  return machine.get_reg(exp.name);
		 }
        break;
    default:
	   console.log('wrong exp ' + exp);
       break;	
	}
	
	return result;
}

//basic operation on machine
function make_assign(cmdobj,machine){
	var src = cmdobj.src;	
	var dest =  cmdobj.dest;	
	var calculate ={};
	
	if(src.type ==='operation'){
		calculate = make_operation_exp(src,cmdobj.params, machine);
	}else{
		calculate = make_primitive_exp(src,machine);
	}
	return function(){		
		var val = calculate();
		machine.set_reg(dest,val);
		machine.advance_pc();
	}
}
function make_test(inst,machine){	
	var operation = inst.operation;
	var func = machine.libs[operation.functionName];
	
	return function(){
		var left = inst.left;
		var right = inst.right;
		// valueOf  , if left is const, or reg.
		if(left.type ==='const'){
			left = left.value;
		}else{
			left = machine.get_reg(left.name);
		}
		
		if(typeof(right) ==='undefined'){
			// do nothing
		}else if(right.type ==='const'){
			right = right.value;
		}else {
			right = machine.get_reg(right.name);
		}
	
		if(func != null){
			 var cond = func(left,right);
			 if(cond){
				 machine.flag = true;
			 }else{
				 machine.flag = false;
			 }
		}		
		machine.advance_pc();
	};
}

function make_goto(inst,machine){
	// find label,
	var dest = inst.dest;	
		
	return function(){
		var index = -1;
		var staticname = '';
		if(dest.type==='reg'){			
			staticname = machine.get_reg(dest.name);
		}else if(dest.type ==='label'){
			staticname = dest.name;
		}
		for(var i=0; i< machine.ops.length;i++) {
			var cmd = machine.ops[i];
            if(cmd.type ==='label'){
				if(cmd.name === staticname){
					index = i;
					break;
				}
			}			
		}
		machine.pcindex = index;
	    machine.pc = machine.ops[index];
		trace('->' + staticname);
	}
}

function make_label(inst,machine){
	return function(){
		machine.advance_pc();
	};
}

function make_branch(inst,machine){
	var dst = inst.dest;
	return function(){
		if(machine.flag){
		   	var index = -1;
			for(var i=0; i< machine.ops.length;i++) {
				var cmd = machine.ops[i];
				if(cmd.type ==='label'){
					if(cmd.name === dst.name){
						index = i;
						break;
					}
				}			
			}
			machine.pcindex = index;
			machine.pc = machine.ops[index];
		}else{
			machine.advance_pc();
		}	
	};
}

function make_save(inst,machine){
	
	return function(){
		var val = machine.get_reg(inst.reg);
		//machine.stack.push(val);
		var copy = deepcopy(val);
		machine.stack.push(copy);
		machine.debugstack.push(inst);
		machine.numPush++;
		machine.advance_pc();
	}
}

function make_restore(inst,machine){
	var regName =  inst.reg;
	return function(){
		var old = machine.stack.pop();
		machine.debugstack.pop();
		machine.set_reg(regName,old);
		machine.advance_pc();
	}
}

function make_perform(inst,machine){
	var perform = inst.operator;
	var paras = inst.params;
	var proc = make_operation_exp(perform,paras,machine);

	return function(){
			if(proc != null){
		         proc();
	       }
		   machine.advance_pc();
	}
}


function analyze(cmds){
	var instructions = cmds.slice();
	result = {};
	result.insts = 	instructions.sort(function(a,b){
								  if( a.type < b.type) return -1;
								  else if (a.type > b.type) return 1;
								  else return 0;
							});
	result.entries = instructions.filter(function(inst){
		 return inst.type === 'goto';
	}).map(function(item){
		 return item.dest.name;
	});			
   	
	result.onstacks = instructions.filter(function(inst){
		return inst.type ==='save';
	}).map(function(item){
		return item.reg;
	});
	
	result.sources =  instructions.filter(function(inst){
		return inst.type ==='assign';
	}).sort(function(a,b){
		if(a.dest.name < b.dest.name ) return -1;
		else if (a.dest.name > b.dest.name) return 1;
		else return 0;
	});
	
	return result;
}

////////////////////////////////////////////////////////       test part     //////////////////////////////////////////////////////////////////

function test_evaluator(){	
	// write machine code here , and load into controller_text	
	var command_text= machine_code;		
    var ss = command_text.split(';');
	var controller_text=[];	
    for(var i=0;i< ss.length;i++){
		controller_text.push(ss[i]);
	}
	// setup machine
	var regs = ['val','continue','exp','env','proc','argl','unev'];	

	var m =  make_machine(regs);	
    
	// assemble machine with libs and machine code
	asseble(controller_text,m,libs);
	// env has frames list.  new frame in front, like stack
	// frame is key/value pairs.  implement by object
	var global_env = [];	
	var prims = setup_global_environment();
	global_env[0] = prims;
	
	m.set_reg('env',global_env);
	var done ={};
	done.type = 'label';
	done.name = 'done';
	m.set_reg('continue',done);
	var test_exp = m.libs.gen('(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))');
	
	m.set_reg('exp',test_exp);
	m.start();
	m.pcindex = 0;   // reset pcindex  to the beginning  index 0
    //test_exp = m.libs.gen('(fib 1)');
	test_exp = m.libs.gen('(cons 1 2)');
	m.set_reg('exp',test_exp);
	m.start();
	console.log('Machine finish!');
	console.log('result  is ' + m.get_reg('val'));
}
