'use strict';

const identity = (x) => x;

const reRegExpChar = /[\\^$.*+?()[\]{}|]/g;
const reHasRegExpChar = RegExp(reRegExpChar.source);

function escapeRegExp(string) {
	return (string && reHasRegExpChar.test(string))
		? string.replace(reRegExpChar, '\\$&')
		: string;
}

const create = (parent, body={}) => Object.assign(Object.create(parent), body);


const showError = (message, text, pos) => {
	let end = text.indexOf('\n', pos);
	end = end === -1 ? text.length : end;
	let start = Math.max(0, text.lastIndexOf('\n', end - 1));
	let line = (text.substr(0, start).match(/\n/mg) || []).length + 1;
	let col = pos - start;

	return {line, col, message}
};


const T = {
	WHITE_SPACE: '<white space>',

	//COMMENT: '<comment>',
	COMMENT_BLOCK: '<comment block>',

	BOOL: 'bool',
	NUMBER: 'number',
	STRING: 'string',
	ID: 'id',

	// keywords
	PROGRAM: 'PROGRAM',
	VAR: 'VAR',
	INTEGER_CONST: 'INTEGER',
	REAL_CONST: 'REAL',
	BEGIN: 'BEGIN',
	END: 'END',
	DIV: 'div',
	ASSIGN: ':=',

	// operators
	PLUS: '+',
	MINUS: '-',
	MUL: '*',
	FLOAT_DIV: '/',

	// separators
	L_PAREN: '(',
	R_PAREN: ')',
	COLON: ':',
	COMMA: ',',
	SEMI: ';',

	// endings
	DOT: '.',
	EOF: 'EOF'
};

const Token = (type, value) => create(null, {
	type, value, toString: () => `(${type} ${value})`
});


const Terminal = (type, start, match, parse=identity, containsValue=true, skippable=false, skipWrappers=false) => ({
	type, start, match, skippable, skipWrappers,
	token: (val) => Token(type, parse(val))
});

const SimpleTerminal = (type, str) =>
	Terminal(type, new RegExp(`${escapeRegExp(str[0])}`,'i'), str, identity, false);

let TERMINALS = [];
const excludeTokens = ['WHITE_SPACE', 'COMMENT_BLOCK', 'BOOL', 'NUMBER', 'STRING', 'ID'];
for (let type in T){
	if (!excludeTokens.includes(type)){
		TERMINALS.push(SimpleTerminal(T[type], T[type]));
	}
}
TERMINALS = TERMINALS.concat(TERMINALS, [
	Terminal(T.WHITE_SPACE, 	/\s/, 						/^\s+$/,					identity,			 false, true),
	Terminal(T.COMMENT_BLOCK, 	/\{/, 						/^\{[^}]*\}$/,				identity,			 false, true),
	Terminal(T.BOOL, 			/(t|b)/, 					/^(true|false)$/i, 			val => val.toLowerCase() === 'true'),
	Terminal(T.NUMBER, 			/\d/, 						/^(\d+(\.\d*)?)$/, 			parseFloat),
	Terminal(T.STRING, 			/"/, 						/^((?:[^"\\]|\\.)*)$/,		identity,			 true, false, true),
	Terminal(T.ID, 				/[=!_\-+*a-zA-Z]/, 		    /^([=!_\-+*a-zA-Z][!_\-+*\w]*)$/)
])

const RESERVED_KEYWORDS = {
	program: Token(T.PROGRAM, '/'),
	begin: Token(T.BEGIN, 'BEGIN'),
	end: Token(T.END, 'END'),
	var: Token(T.VAR, '/'),
	integer: Token(T.INTEGER_CONST, '/'),
	real: Token(T.REAL_CONST, '/'),
	div: Token(T.DIV, '/'),
};

const Lexer = (terminals, text) => create(null, {
	terminals,
	text,
	pos: 0,
	currentToken: null,
	currentChar: text[0],

	error(){
		const {line, col, message} = showError(this.text, this.pos);
		throw `Invalid character in line ${line}:${col}\n${message}`;
	},
	hasNext(){
		return this.currentChar !== undefined;
	},
	readNext(){
		this.currentChar = this.text[++this.pos];
	},
	peek(){
		return this.text[this.pos + 1];
	},
	readAhead(n){
		return this.text.substr(this.pos, n);
	},
	skip(n){
		this.pos += n;
		this.currentChar = this.text[this.pos];
	},
	readTerminal(terminal){
		if (terminal.skipWrappers) this.readNext();
		if (terminal.match instanceof RegExp){
			let result = this.currentChar;
			while (this.hasNext() && this.peek() !== undefined && terminal.match.test(result + this.peek())) {
				this.readNext();
				result += this.currentChar;
			}
			if (terminal.skipWrappers) this.readNext();
			this.readNext();
			return terminal.token(result);
		} else if(typeof terminal.match === 'string'){
			let len = terminal.match.length;
			let t = this.readAhead(len);
			if (t.toUpperCase() === terminal.match.toUpperCase()){
				this.skip(len);
				return terminal.token(t);
			} else {
				return null;
			}
		}
	},
	getNextToken(){
		while (this.hasNext()) {
			for (let terminal of this.terminals) {
				if (terminal.start.test(this.currentChar)) {
					const res = this.readTerminal(terminal);
					console.log(res);
					if (res === null || !terminal.skippable) return res;
				}
			}
			this.error();
		}
		return Token(T.EOF, null);
	}
});

const Lexer_OLD = (text) => create(null, {
	text,
	pos: 0,
	currentToken: null,
	currentChar: text[0],

	error(){
		throw showError('Invalid character', this.text, this.pos);
	},
	advance(){
		this.pos++;
		if (this.pos > this.text.length) {
			this.currentChar = null;
		} else {
			this.currentChar = this.text[this.pos];
		}
	},
	peek(){
		let peekPos = this.pos + 1;
		return peekPos > this.text.length ? null : this.text[peekPos];
	},
	_id(){
		let result = '';
		while(this.currentChar !== null && /(_|\w)/.test(this.currentChar)){
			result += this.currentChar;
			this.advance();
		}
		result = result.toLowerCase();
		return RESERVED_KEYWORDS[result] || Token(T.ID, result)
	},
	skipWhitespace(){
		while (this.currentChar !== null && /\s/.test(this.currentChar)) this.advance();
	},
	skipComment(){
		while (this.currentChar !== '}') this.advance();
		this.advance();
	},
	number(){
		let result = '';
		while (this.currentChar !== null && /\d/.test(this.currentChar)) {
			result += this.currentChar;
			this.advance();
		}
		if (this.currentChar === '.'){
			result += this.currentChar;
			this.advance();
			while (this.currentChar !== null && /\d/.test(this.currentChar)) {
				result += this.currentChar;
				this.advance();
			}
			return Token(T.REAL_CONST, parseFloat(result));
		} else {
			return Token(T.INTEGER_CONST, parseInt(result, 10));
		}
	},
	getNextToken(){
		while (this.currentChar !== undefined) {
			if (/\s/.test(this.currentChar)) {
				this.skipWhitespace();
				continue
			}

			if (this.currentChar === '{') {
				this.advance();
				this.skipComment();
				continue
			}

			if (/\d/.test(this.currentChar)) {
				return this.number();
			}


			if (this.currentChar === ':' && this.peek()!== '=') {
				this.advance();
				return Token(T.COLON, ':');
			}

			if (this.currentChar === ',') {
				this.advance();
				return Token(T.COMMA, ',');
			}

			if (this.currentChar === '+') {
				this.advance();
				return Token(T.PLUS, '+');
			}

			if (this.currentChar === '+') {
				this.advance();
				return Token(T.PLUS, '+');
			}

			if (this.currentChar === '-') {
				this.advance();
				return Token(T.MINUS, '-');
			}

			if (this.currentChar === '*') {
				this.advance();
				return Token(T.MUL, '*');
			}

			if (this.currentChar === '/') {
				this.advance();
				return Token(T.FLOAT_DIV, '/');
			}

			if (this.currentChar === '(') {
				this.advance();
				return Token(T.LPAREN, '(');
			}

			if (this.currentChar === ')') {
				this.advance();
				return Token(T.RPAREN, ')');
			}

			if (/\w/.test(this.currentChar)) {
				return this._id();
			}

			if (this.currentChar === ':' && this.peek() === '=') {
				this.advance();
				this.advance();
				return Token(T.ASSIGN, ':=');
			}

			if (this.currentChar === ';') {
				this.advance();
				return Token(T.SEMI, ')');
			}

			if (this.currentChar === '.') {
				this.advance();
				return Token(T.DOT, '.');
			}

			this.error();
		}
		return Token(T.EOF, null);
	}
});

/*
 PARSER
 */

const AST = create(null, {});

const Program = (name, block) => create(AST, {
	type: 'program',
	name, block
});

const Block = (declarations, compoundStatement) => create(AST, {
	type: 'block',
	declarations, compoundStatement
});

const VarDecl = (varNode, typeNode) => create(AST, {
	type: 'vardecl',
	varNode, typeNode
});

const Type = (token) => create(AST, {
	type: 'type',
	token, value: token.value
});

const BinOp = (left, op, right) => create(AST, {
	type: 'binop',
	left, op, right, token: op,
	toString: () => `(${left.toString()} / ${op.toString()} \\ ${right.toString()})`
});

const UnaryOp = (op, expr) => create(AST, {
	type: 'unaryop',
	op, expr, token: op
});

const Num = (token) => create(AST, {
	type: 'num',
	token, value: token.value, toString: () => token.toString()
});

const Compound = () => create(AST, {
	type: 'compound',
	children: []
});

const Assign = (left, op, right) => create(AST,{
	type: 'assign',
	left, op, right, token: op
});

const Var = (token) => create(AST, {
	type: 'var',
	token, value: token.value
});

const NoOp = () => create(AST, {type: 'noop'});



const Parser = (lexer) => create(null, {
	lexer,
	currentToken: lexer.getNextToken(),
	error(){
		throw showError('Invalid syntax', this.lexer.text, this.lexer.pos);
	},
	eat(tokenType){
		if (this.currentToken.type === tokenType) {
			this.currentToken = this.lexer.getNextToken();
		} else {
			this.error();
		}
	},

	program(){
		this.eat(T.PROGRAM);
		let varNode = this.variable();
		let programName = varNode.value;
		this.eat(T.SEMI);
		let blockNode = this.block();
		let programNode = Program(programName, blockNode);
		this.eat(T.DOT);
		return programNode;
	},

	block(){
		let declarationNodes = this.declarations();
		let compoundStatementNodes = this.compoundStatement();
		return Block(declarationNodes, compoundStatementNodes);
	},

	declarations(){
		let declarations = [];
		if (this.currentToken.type === T.VAR){
			this.eat(T.VAR);
			while (this.currentToken.type === T.ID){
				declarations = declarations.concat(this.variableDeclaration());
				this.eat(T.SEMI);
			}
		}
		return declarations;
	},

	variableDeclaration(){
		let varNodes = [Var(this.currentToken)];
		this.eat(T.ID);

		while (this.currentToken.type === T.COMMA){
			this.eat(T.COMMA);
			varNodes.push(Var(this.currentToken));
			this.eat(T.ID);
		}
		this.eat(T.COLON);

		let typeNode = this.typeSpec();
		return varNodes.map( varNode => VarDecl(varNode, typeNode) )
	},

	typeSpec(){
		let token = this.currentToken;
		if (this.currentToken.type === T.INTEGER_CONST){
			this.eat(T.INTEGER_CONST);
		} else {
			this.eat(T.REAL_CONST);
		}
		return Type(token);
	},

	compoundStatement() {
		this.eat(T.BEGIN);
		let nodes = this.statementList();
		this.eat(T.END);

		let root = Compound();
		for(let node of nodes) root.children.push(node);

		return root;
	},

	statementList(){
		let results = [this.statement()];

		while(this.currentToken.type === T.SEMI){
			this.eat(T.SEMI);
			results.push(this.statement());
		}

		if (this.currentToken.type === T.ID) this.error();

		return results;
	},

	statement(){
		switch (this.currentToken.type){
			case T.BEGIN: return this.compoundStatement();
			case T.ID: return this.assignmentStatement();
			default: return this.empty();
		}
	},

	assignmentStatement(){
		let left = this.variable();
		let token = this.currentToken;
		this.eat(T.ASSIGN);
		let right = this.exp();
		return Assign(left, token, right);
	},

	variable(){
		let node = Var(this.currentToken);
		this.eat(T.ID);
		return node;
	},

	empty(){ return NoOp(); },

	factor(){
		let token = this.currentToken;
		if (token.type === T.PLUS){
			this.eat(T.PLUS);
			return UnaryOp(token ,this.factor());
		} else if (token.type === T.MINUS){
			this.eat(T.MINUS);
			return UnaryOp(token ,this.factor());
		} else if (token.type === T.LPAREN) {
			this.eat(T.LPAREN);
			let node = this.exp();
			this.eat(T.RPAREN);
			return node
		} else if (token.type === T.INTEGER_CONST) {
			this.eat(T.INTEGER_CONST);
			return Num(token);
		} else if (token.type === T.REAL_CONST) {
			this.eat(T.REAL_CONST);
			return Num(token);
		} else {
			return this.variable();
		}
	},
	term(){
		let node = this.factor();

		while ([T.MUL, T.DIV, T.FLOAT_DIV].includes(this.currentToken.type)) {
			let token = this.currentToken;
			if (token.type === T.MUL) {
				this.eat(T.MUL);
			} else if (token.type === T.DIV) {
				this.eat(T.DIV);
			} else if (token.type === T.FLOAT_DIV) {
				this.eat(T.FLOAT_DIV);
			}
			node = BinOp(node, token, this.factor())
		}

		return node;
	},
	exp(){
		let node = this.term();

		while ([T.PLUS, T.MINUS].includes(this.currentToken.type)) {
			let token = this.currentToken;
			if (token.type === T.PLUS) {
				this.eat(T.PLUS);
			} else if (token.type === T.MINUS) {
				this.eat(T.MINUS);
			} else {
				this.error();
			}
			node = BinOp(node, token, this.term())
		}

		return node;
	},
	parse(){
		return this.program();
	}
});


const Interpreter = () => create(null, {
	GLOBAL_SCOPE: {},
	run(program){
		this.visit(program);
		return(this.GLOBAL_SCOPE);
	},
	visit(node){
		return this['visit_' + node.type](node);
	},
	visit_program(node){
		this.visit(node.block);
	},
	visit_block(node){
		for(let declaration of node.declarations) this.visit(declaration);
		this.visit(node.compoundStatement);
	},
	visit_vardecl(){},
	visit_type(){},
	visit_binop(node){
		switch(node.op.type){
			case T.PLUS: 	 	return this.visit(node.left) + this.visit(node.right);
			case T.MINUS:  		return this.visit(node.left) - this.visit(node.right);
			case T.MUL: 	 	return this.visit(node.left) * this.visit(node.right);
			case T.DIV: 	 	return Math.floor(this.visit(node.left) / this.visit(node.right));
			case T.FLOAT_DIV: 	return this.visit(node.left) / this.visit(node.right);
			default: throw 'Invalid operation';
		}
	},
	visit_unaryop(node){
		switch (node.op.type){
			case T.PLUS:  return (+this.visit(node.expr));
			case T.MINUS: return (-this.visit(node.expr));
		}
	},
	visit_num(node){
		return node.value;
	},
	visit_compound(node){
		for(let child of node.children) this.visit(child);
	},
	visit_noop(node){},
	visit_var(node){
		let name = node.value;
		let value = this.GLOBAL_SCOPE[name];
		if (value === undefined) throw `Name error ${name}`;
		else return value;
	},
	visit_assign(node){
		let varName = node.left.value;
		this.GLOBAL_SCOPE[varName] = this.visit(node.right);
	}
});
