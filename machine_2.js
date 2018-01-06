//machine
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
				var val = this.get_reg(key);
				if(val !== 'unassigned'){
					var  content =  this.get_reg(key);
					if(typeof(content) ==='object'){
						var  ckeys = Object.keys(content);
						var serialized = '';
						for(var j=0; j< ckeys.length;j++){
							var ckey = ckeys[j];
							serialized +=  ckey + ':' + content[ckey] + '|';
						}
						m += key +':' + serialized+'; ';
					}else{
						m += key +':' + content+'; ';
					}
					
				}				
			}
			var sta ='';
			for(var i=0;i< this.stack.length;i++){
				sta += ' |' + this.stack[i]  ;
			}
			//print pc command
			console.log(this.pc.cmdtext + '| '+m + 'stack' + sta);
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
		machine.stack.push(val);
		machine.numPush++;
		machine.advance_pc();
	}
}

function make_restore(inst,machine){
	var regName =  inst.reg;
	return function(){
		var old = machine.stack.pop();
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

///////////////////////////////////////////////////////////  memory management //////////////////////////////////////////////
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
	global_env[0] = {};
	m.set_reg('env',global_env);
	var done ={};
	done.type = 'label';
	done.name = 'done';
	m.set_reg('continue',done);
	var test_exp = m.libs.gen('(define a 78)');
	
	m.set_reg('exp',test_exp);
	//var www = m.libs.gen('\'abc');
	m.start();
	test_exp = m.libs.gen('a');
	m.set_reg('exp',test_exp);
	m.pcindex = 0;   // reset pcindex  to the beginning  index 0
	m.start();
	console.log('Machine finish!');
	console.log('result  is ' + m.get_reg('val'));
}
