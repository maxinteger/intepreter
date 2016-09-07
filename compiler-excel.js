'use strict';
const {create, escapeRegExp, showError} = require('./utils');

const ramda = require('ramda');
/*
 GRAMMAR:

 program		: list

 list			: L_PAREN term_list R_PAREN

 term_list		: term
 				| term term_list

 term			: factor
 				| list
 				| empty

 map 			: L_CPAREN (empty | ( KEYWORD factor ))* R_CPAREN

 factor			: NUMBER
 				| STRING
 				| BOOL
 				| KEYWORD
 				| ID
 				| map

 empty			:


 */

const T = {
	WHITE_SPACE: '<white space>',
	COMMENT: '<comment>',
	COMMENT_BLOCK: '<comment block>',
	L_PAREN: '(',
	R_PAREN: ')',
	L_CPAREN: '{',
	R_CPAREN: '}',
	L_SPAREN: '[',
	R_SPAREN: ']',
	BOOL: 'bool',
	NUMBER: 'number',
	STRING: 'string',
	ID: 'id',
	KEYWORD: 'keyword'
};

const Token = (type, value) => create(null, {
	type, value, toString: () => `(${type} ${value})`
});


const Terminal = (type, start, match, parse=ramda.identity, containsValue=true, skippable=false, skipWrappers=false) => ({
	type, start, match, skippable, skipWrappers,
	token: (val) => Token(type, parse(val))
});

const SingleCharTemninal = (type, char) =>
	Terminal(type, new RegExp(`${escapeRegExp(char)}`), new RegExp(`^(${escapeRegExp(char)})$`), ramda.identity, false);

const TERMINALS = {
	[T.WHITE_SPACE]: Terminal(T.WHITE_SPACE, 	/\s/, 						/^\s+$/,					ramda.identity,			 false, true),
	[T.BOOL]: 		 Terminal(T.BOOL, 			/(t|b)/, 					/^(true|false)$/, 			val => val === 'true'),
	[T.NUMBER]: 	 Terminal(T.NUMBER, 		/\d/, 						/^(\d+(\.\d*)?)$/, 			parseFloat),
	[T.STRING]: 	 Terminal(T.STRING, 		/"/, 						/^((?:[^"\\]|\\.)*)$/,		ramda.identity,			 true, false, true),
	[T.ID]: 		 Terminal(T.ID, 			/[=!_\-+*a-zA-Z]/, 		    /^([=!_\-+*a-zA-Z][!_\-+*\w]*)$/),
	[T.L_PAREN]: 	 SingleCharTemninal(T.L_PAREN, '('),
	[T.R_PAREN]: 	 SingleCharTemninal(T.R_PAREN, ')'),
};

const NON_TERMINALS = {
	program: 	[['list']],
	list: 		[[T.L_PAREN, 'term_list', T.R_PAREN]],
	term_list: 	[['term'], ['term', 'term_list']],
	term:		[['factor'], ['list'], ['empty']],
	factor:		[[T.NUMBER], [T.BOOL], [T.STRING], ['ref']],
	ref:		[[T.ID]],
	empty: 		[]
};

const TERMINAL_LIST = ramda.values(TERMINALS);


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
	readTerminal(terminal){
		if(terminal.skipWrappers) this.readNext();
		let result = this.currentChar;
		while (this.hasNext() && this.peek() !== undefined && terminal.match.test(result + this.peek())) {
			this.readNext();
			result += this.currentChar;
		}
		if(terminal.skipWrappers) this.readNext();
		this.readNext();
		return terminal.token(result);
	},
	getNextToken(){
		while (this.hasNext()) {
			for(let terminal of this.terminals){
				if (terminal.start.test(this.currentChar)){
					const res = this.readTerminal(terminal);
					if (!terminal.skippable) return res;
				}
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

const TerminalNode = (type, token) => create(AST, {type, token, value: token.value});

const NonTerminalNode = (type, children=[]) => create(AST, {type, children});

const ListNode = (children=[]) => create(AST, {type: 'list', children});

const ValueNode = (token) => create(AST, {type: 'value', token, value: token.value});

const NoOp = () => create(AST, {type: 'noop'});

const Parser = (terminals, nonTerminals, lexer) => create(null, {
	terminals,
	nonTerminals,
	lexer,
	currentToken: lexer.getNextToken(),
	error(){
		const {line,col,message} = showError(this.lexer.text, this.lexer.pos);
		throw `Invalid syntax in line ${line}:${col}\n${message}`;
	},

	eat(tokenType){
		if (this.currentToken.type === tokenType) {
			this.currentToken = this.lexer.getNextToken();
		} else {
			this.error();
		}
	},



	program(){
		return this.list();
	},

	list(){
		this.eat(T.L_PAREN);
		let nodes = this.termList();
		this.eat(T.R_PAREN);
		return ListNode(nodes);
	},

	termList(){
		let results = [this.term()];
		while(this.currentToken.type !== T.R_PAREN){
			results.push(this.term());
		}
		return results;
	},

	term(){
		switch (this.currentToken.type){
			case T.L_PAREN: return this.list();
			case T.ID:
			case T.BOOL:
			case T.NUMBER:
			case T.STRING:
			case T.KEYWORD:
				return this.factor();
			default:
				this.error();
		}
	},

	factor(){
		switch(this.currentToken.type){
			case T.ID:
			case T.BOOL:
			case T.NUMBER:
			case T.STRING:
			case T.KEYWORD:
				let node = ValueNode(this.currentToken);
				this.eat(this.currentToken.type);
				return node;
			default: this.error();
		}
	},

	empty(){
		return NoOp();
	},

	process(name){
		const nonTerminal = this.nonTerminals[name];
		if(nonTerminal.length === 0) return NoOp();
		return NonTerminalNode(name, nonTerminal.reduce( (acc, item) => {
			if (item in this.nonTerminals){
				return acc.concat(this.process(item));
			} else if (item in this.terminals){
				const terminal = this.terminals[item];
				if(terminal.containsValue){
					return TerminalNode(this.currentToken.type, this.currentToken);
				}
				this.eat(item);
			}
		}, []))
	},

	parse(){
		return this.program();
	}
});

const {reduce, sum, product, unapply} = ramda;

const Interpreter = () => create(null, {
	GLOBAL_SCOPE: create(null, {
		stdout: null,
		'+': unapply(sum),
		'*': unapply(product),
		'if': (p, t, f) => p ? t : f,
		'concat': (a, b) => a + b,
		'print': console.log.bind(console)
	}),
	run(program, additionalScope={}){
		let result;
		this.GLOBAL_SCOPE['='] = r => result = r;
		const scope = create(this.GLOBAL_SCOPE, additionalScope);
		this.visit(program, scope);
		return result;
	},
	visit(node, scope){
		return this['visit_' + node.type](node, scope);
	},
	visit_list(node, scope){
		if (node.children.length >= 1){
			let head = this.visit(node.children[0], scope);
			let tail = node.children.slice(1).map(x => this.visit(x, scope));
			if (typeof head === 'function'){
				return head.apply(null, tail);
			} else {
				return [head].concat(tail);
			}
		}
	},
	visit_value(node, scope){
		switch (node.token.type){
			case T.BOOL:
			case T.NUMBER:
			case T.STRING: return node.value;
			case T.ID:
			case T.KEYWORD: return scope[node.value];
		}
	},
	visit_noop(node){ }
});

const astReduce = (fn, obj, ast) => {
	obj = fn(obj, ast);
	if (ast.children){
		return ast.children.reduce(fn, obj)
	}
	return obj;
};

module.exports = {
	TOKENS: T,
	compile: (src) => Parser( TERMINALS, NON_TERMINALS, Lexer(TERMINAL_LIST, src) ).parse(),
	run: ramda.curry((ast, params) => Interpreter().run(ast, params)),
	astReduce
};
