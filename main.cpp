        // +--------------------------------------------------+ //
       // /                      SBLOC                       / //
      // /          Stack-Based Language Of Course          / //
     // /              Made by Somerandomdev49             / //
    // +--------------------------------------------------+ //
   //                                                      //
  // +--------------------------------------------------+ //
 // /          Written in < 30 hours (5d x ~6h)        / //
// +--------------------------------------------------+ //
//
//
// +--------------------------------------------------+ //
// |                                                  | //
// |       I avoided classes because I did not        | //
// |       want them. Not because I don't             | //
// |       understand them :)                         | //
// |                                                  | //
// +--------------------------------------------------+ //
#include <vector>
#include <string>
#include <iostream>
#include <utility>
#include <tuple>
#include <variant>
#include <algorithm>
#include <iterator>
#include <sstream>
#include <fstream>
#include <functional>
#include <map>
#include <stack>
#include <exception>
#include <numeric>
#include <chrono>
#define group_start() //GLOBAL_GROUP_INDENT++
#define group_end()// GLOBAL_GROUP_INDENT--
#define __debug_ALWAYS(x) std::cout << std::string("|   ") * GLOBAL_GROUP_INDENT << x << std::endl;
#define __debug(x) // __debug_ALWAYS(x)
#define debug(x) // __debug(x)
#define debug1(x) __debug(x)
#define debug2(x) __debug(x)
#define debug3(x) //__debug(x)
#define debug4(x) //__debug(x)
template<typename element>
using container = std::vector<element>;
int GLOBAL_GROUP_INDENT = 0;
bool debug_mode = false;


template<typename Char, typename Traits, typename Allocator>
std::basic_string<Char, Traits, Allocator> operator *
(const std::basic_string<Char, Traits, Allocator> s, size_t n)
{
	if(n == 1) return s;
	if(n == 0) return "";
   	std::basic_string<Char, Traits, Allocator> tmp = s;
   	for (size_t i = 0; i < n; ++i)
   	{
   	   tmp += s;
   	}
   	return tmp;
}

template<typename Char, typename Traits, typename Allocator>
std::basic_string<Char, Traits, Allocator> operator *
(size_t n, const std::basic_string<Char, Traits, Allocator>& s)
{
   return s * n;
}

enum class token_type
{
	var, num, str,
	err, eof,
	add, sub, mul, div, and_, xor_,
	or_, lt, gt, le, ge, eql, arw,
	colon, semicolon, mod, bin_or,
	bin_and, comma, is_eql,
	op, cp,
	os, cs,
	oc, cc,
};

enum class object_type
{
	num, str, arr, nil
};

using token = std::pair<token_type, std::string>;

// +--------------------------------------------------+ //
// |                    TOKENIZER                     | //
// +--------------------------------------------------+ //
// |                                                  | //
// |            Basically a state machine.            | //
// |                                                  | //
// +--------------------------------------------------+ //
auto tokenize(std::istream &input) -> container<token>
{
	container<token> tokens;

	token_type state = token_type::err;
	std::string state_buffer;
	const auto switch_state = [&](token_type type, char c = '\0')
	{
		if(c) state_buffer.push_back(c);
		state = type;
	};

	const auto done_state = [&](token_type rev = token_type::err)
	{
		tokens.push_back(std::make_pair(state, state_buffer));
		state = rev;
		state_buffer.clear();
	};

	const auto single_state = [&](char c, token_type type, token_type rev = token_type::err)
	{
		state = type;
		state_buffer.push_back(c);
		done_state(rev);
	};


	std::istreambuf_iterator<char> begin(input), end;
	for(bool iter; begin != end; iter ? begin++ : begin)
	{
		iter = true;
		auto c = *begin;
		if(state == token_type::err)
			switch(c)
			{
				case ' ' :
				case '\n':
				case '\t':
				case '\r':
				case '\v':
				case '\f': break;

				case 'a'...'z':
				case 'A'...'Z':
				case '_'      : switch_state(token_type::var, c);          break;
				case '0'...'9': switch_state(token_type::num, c);          break;
				case '&'      : switch_state(token_type::and_, c);         break;
				case '|'      : switch_state(token_type::or_, c);          break;
				case '>'      : switch_state(token_type::ge, c);           break;
				case '<'      : switch_state(token_type::le, c);           break;
				case '-'      : switch_state(token_type::arw, c);          break;
				case '='      : switch_state(token_type::is_eql, c);       break;
				case '"'      : switch_state(token_type::str, c); begin++; break;
 
				case '+': single_state('+', token_type::add); break;
				case '*': single_state('*', token_type::mul); break; // TODO: add ** (pow), [parser]: right precedence?
				case '/': single_state('/', token_type::div); break;
				case '(': single_state('(', token_type::op);  break;
				case ')': single_state(')', token_type::cp);  break;
				case '[': single_state('[', token_type::os);  break;
				case ']': single_state(']', token_type::cs);  break;
				case '{': single_state('{', token_type::oc);  break;
				case '}': single_state('}', token_type::cc);  break;
				case ';': single_state(';', token_type::semicolon);break;
				case ':': single_state(':', token_type::colon);  break;
				case ',': single_state(',', token_type::comma);  break;
				default: switch_state(token_type::err); done_state(); break;
			}
		else if(state == token_type::var)
			switch(c)
			{
				case 'a'...'z': 
				case 'A'...'Z': 
				case '0'...'9': 
				case '_'      : state_buffer.push_back(c); break;

				default: done_state(); iter = false; break;
			}
		else if(state == token_type::num)
			switch(c)
			{
				case '0'...'9': 
				case '_'      : state_buffer.push_back(c); break;

				default: done_state(); iter = false; break;
			}
		else if(state == token_type::arw)
			switch(c)
			{
				case '>': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::sub); done_state(); break;
			}
		else if(state == token_type::and_)
			switch(c)
			{
				case '&': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::bin_and); done_state(); break;
			}
		else if(state == token_type::or_)
			switch(c)
			{
				case '|': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::bin_or); done_state(); break;
			}
		else if(state == token_type::le)
			switch(c)
			{
				case '=': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::lt); done_state(); break;
			}
		else if(state == token_type::ge)
			switch(c)
			{
				case '=': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::gt); done_state(); break;
			}
		else if(state == token_type::is_eql)
			switch(c)
			{
				case '=': state_buffer.push_back(c); done_state(); break;
				default: switch_state(token_type::eql); done_state(); break;
			}
	};
	tokens.push_back(token(token_type::eof, ""));
	return tokens;
}



enum class instruction_type
{
	get, set, nop,
	add, sub, mul, div, and_, xor_, jpa,
	or_, lt, gt, le, ge, bin_and, bin_or, mod,
	pop, create, num, debug, jmp, neg, eql, dup
};

std::ostream &operator<<(std::ostream &os, const instruction_type &i)
{
	std::string s;
	switch(i)
	{
		case instruction_type::get     : s = "get    "; break;
		case instruction_type::set     : s = "set    "; break;
		case instruction_type::pop     : s = "pop    "; break;
		case instruction_type::num     : s = "num    "; break;
		case instruction_type::debug   : s = "debug  "; break;
		case instruction_type::create  : s = "create "; break;
		case instruction_type::nop     : s = "nop    "; break;
		case instruction_type::and_    : s = "and_   "; break;
		case instruction_type::or_     : s = "or_    "; break;
		case instruction_type::xor_    : s = "xor_   "; break;
		case instruction_type::mul     : s = "mul    "; break;
		case instruction_type::div     : s = "div    "; break;
		case instruction_type::sub     : s = "sub    "; break;
		case instruction_type::add     : s = "add    "; break;
		case instruction_type::bin_and : s = "bin_and"; break;
		case instruction_type::bin_or  : s = "bin_or "; break;
		case instruction_type::ge      : s = "ge     "; break;
		case instruction_type::le      : s = "le     "; break;
		case instruction_type::gt      : s = "gt     "; break;
		case instruction_type::lt      : s = "lt     "; break;
		case instruction_type::mod     : s = "mod    "; break;
		case instruction_type::neg     : s = "neg    "; break;
		case instruction_type::jmp     : s = "jmp    "; break;
		case instruction_type::jpa     : s = "jpa    "; break;
		case instruction_type::eql     : s = "eql    "; break;
		case instruction_type::dup     : s = "dup    "; break;
	}
	os << s;
	return os;
}

enum class expression_type
{
	atom, expr, stmt
};

struct tree
{
	token_type type;
	std::variant<std::vector<tree>, std::string> value;
};

using instruction = std::pair<instruction_type, std::variant<float, int, std::string, std::tuple<float, float>, std::tuple<float, float, float>, std::monostate>>;

// +--------------------------------------------------+ //
// |                      PARSER                      | //
// +--------------------------------------------------+ //
// |                                                  | //
// |            A recursive-descent parser.           | //
// |                                                  | //
// +--------------------------------------------------+ //
auto parse(const container<token> &input) -> std::pair<container<int>, container<instruction>>
{
	std::map<token_type, std::pair<int, bool>> precedence_table = // precedence, is_right_assoc
	{
		{ token_type::add, std::make_pair(1, false) },
		{ token_type::sub, std::make_pair(1, false) },
		{ token_type::mul, std::make_pair(2, false) },
		{ token_type::div, std::make_pair(2, false) },
		{ token_type::lt, std::make_pair(3, false) },
		{ token_type::gt, std::make_pair(3, false) },
		{ token_type::le, std::make_pair(4, false) },
		{ token_type::ge, std::make_pair(4, false) },
		{ token_type::is_eql, std::make_pair(5, false) },
	};
	
	auto is_operator = [&](token_type type) -> bool
	{
		return type == token_type::and_    ||
			   type == token_type::or_     ||
			   type == token_type::xor_    ||
			   type == token_type::mul     ||
			   type == token_type::div     ||
			   type == token_type::sub     ||
			   type == token_type::add     ||
			   type == token_type::bin_and ||
			   type == token_type::bin_or  ||
			   type == token_type::ge      ||
			   type == token_type::le      ||
			   type == token_type::gt      ||
			   type == token_type::lt      ||
			   type == token_type::is_eql  ||
			   type == token_type::mod     ;;
	};

	auto token_operator_to_instruction = [&](token_type type) -> instruction_type
	{
		if(!is_operator(type)) throw std::runtime_error("Wrong operator: " + std::to_string((int)type));
		switch(type)
		{
			case token_type::and_    : return instruction_type::and_    ;
			case token_type::or_     : return instruction_type::or_     ;
			case token_type::xor_    : return instruction_type::xor_    ;
			case token_type::mul     : return instruction_type::mul     ;
			case token_type::div     : return instruction_type::div     ;
			case token_type::sub     : return instruction_type::sub     ;
			case token_type::add     : return instruction_type::add     ;
			case token_type::bin_and : return instruction_type::bin_and ;
			case token_type::bin_or  : return instruction_type::bin_or  ;
			case token_type::ge      : return instruction_type::ge      ;
			case token_type::le      : return instruction_type::le      ;
			case token_type::gt      : return instruction_type::gt      ;
			case token_type::lt      : return instruction_type::lt      ;
			case token_type::mod     : return instruction_type::mod     ;
			case token_type::is_eql  : return instruction_type::eql     ;
			default: throw std::runtime_error("Not implemented operator: " + std::to_string((int)type));
		}
	};

	auto make_instruction     = [&](instruction_type type)                         { return instruction(type, std::monostate()); };
	auto make_instruction_arg = [&](instruction_type type, int arg)                { return instruction(type, arg); };
	auto make_instruction_str = [&](instruction_type type, const std::string &arg) { return instruction(type, arg); };
	auto make_instruction_num = [&](instruction_type type, float arg)              { return instruction(type, arg); };

	if(input.size() == 0) return std::make_pair<container<int>, container<instruction>>({}, {});
	
	container<std::map<std::string, int>> scopes;
	container<instruction> code;
	scopes.push_back(std::map<std::string, int>()); // initial scope;

	int index = 0;
	token current;
	auto next = [&]() -> void
	{
		if(index < input.size())
			current = input[index++];
		else
			current = std::make_pair(token_type::eof, "");
	};
	next();

	
	std::function<void(expression_type type, int min_prec)> parse; // will c++23 allow recursive lambdas? please...
	parse = [&](expression_type type, int min_prec) -> void
	{
		//debug("parse")
		group_start();
		switch(type)
		{
			case expression_type::stmt:
			{
				debug1("Parse statement");
				if(current.first == token_type::oc)
				{
					next();
					while(current.first != token_type::cc)
					{
						parse(expression_type::stmt, 0);
					}
					next();
					break;
				}
				else if(current.first == token_type::var && current.second == "var")
				{
					//debug1("var");
					next();
					//debug("-> type:")
					//if(current.first != token_type::var) throw std::runtime_error("Expected a type!");
					//object_type type;
					//if(current.second == "num") type = object_type::num;
					//if(current.second == "str") type = object_type::str;
					//if(current.second == "arr") type = object_type::arr;
					//if(current.second == "nil") type = object_type::nil;
					//next();
					//debug("-> name:")
					if(current.first != token_type::var) throw std::runtime_error("Expected a name!");
					//debug("-> scopes:")
					auto &s = scopes[scopes.size() - 1];
					if(s.find(current.second) != s.end()) throw std::runtime_error("Variable " + current.second + " already defined!");
					s[current.second] = s.size();
					next();
				}
				else if(current.first == token_type::var && current.second == "print")
				{
					//debug1("print");
					next();
					parse(expression_type::expr, 0);
					code.push_back(make_instruction(instruction_type::debug));
				}
				else if(current.first == token_type::var && current.second == "if")
				{
					debug3("if");
					next();
					parse(expression_type::expr, 0);
					code.push_back(make_instruction(instruction_type::dup));
					
					code.push_back(make_instruction(instruction_type::neg));

					int jmp_index = code.size();
					code.push_back(make_instruction(instruction_type::nop));

					parse(expression_type::stmt, 0);
					
					code[jmp_index] = make_instruction_arg(instruction_type::jmp, code.size());

					if(current.first == token_type::var && current.second == "else")
					{
						int jmp_index_else = code.size();
						code.push_back(make_instruction(instruction_type::nop));
						next();
						parse(expression_type::stmt, 0);
						code[jmp_index_else] = make_instruction_arg(instruction_type::jmp, code.size());
					}
					break; // skip semicolon check.
				}
				else if(current.first == token_type::var && current.second == "while")
				{
					next();
					int jmp_cond = code.size(); // no empty conditions.
					parse(expression_type::expr, 0);
					code.push_back(make_instruction(instruction_type::neg));
					int jmp_exit_loc = code.size();
					code.push_back(make_instruction(instruction_type::nop));
					parse(expression_type::stmt, 0);

					code.push_back(make_instruction_arg(instruction_type::jpa, jmp_cond));
					code[jmp_exit_loc] = (make_instruction_arg(instruction_type::jmp, code.size()));
					break;
				}
				else
				{
					//debug1("else");
					parse(expression_type::expr, 0);
				}
				debug3("end");
				if(current.first != token_type::semicolon) throw std::runtime_error("Expected a semicolon, but got " + input[index-2].second + current.second);
				next();
				break;
			}
			case expression_type::expr:
			{
				debug3("parse expr")
				debug1("Parse expr, current min_prec: " << min_prec);
				parse(expression_type::atom, -1);
				if(current.first == token_type::eql && input[index-2].first == token_type::var) { // lookback?
					//debug1("set?")
					next();
					auto tmp = code[code.size()-1];
					code.erase(code.end()-1);
					parse(expression_type::expr, 0);
					code.push_back(make_instruction_arg(instruction_type::set, std::get<float>(tmp.second)));
					break;
				}
				//debug1("...");
				for(int i=0; true; i++)
				{
					if(!is_operator(current.first)) { group_end(); break; }
					//debug("expression #" << i);
					group_start();
					
					auto tmp = current;
					//debug("operator: " << tmp.second);
					auto op_info = precedence_table[tmp.first];
					if(op_info.first < min_prec) { group_end(); break; }

					auto next_min_prec = op_info.second ? op_info.first : op_info.first + 1;
					//debug("Next min_prec: " << next_min_prec);
					next(); // skip the op.
					parse(expression_type::expr, next_min_prec);
					code.push_back(make_instruction(token_operator_to_instruction(tmp.first)));
					group_end();
				}
				break;
			}
			case expression_type::atom:
			{
				debug("Parse atom");
				if(current.first == token_type::op) {
					next();
					parse(expression_type::expr, 0);
					if(current.first != token_type::cp) throw std::runtime_error("Expected ')' to close an expression.");
				}
				if(current.first == token_type::var)
				{
					//debug("variable");
					
					std::reverse(scopes.begin(), scopes.end());
					int i = 0; for(const auto &a : scopes)
					{
						auto it = a.find(current.second);
						if(it != a.end()) break;
						i++;
					}
					std::reverse(scopes.begin(), scopes.end());
					if(i == scopes.size()) throw std::runtime_error("No such variable: " + current.second);
					code.push_back(make_instruction_arg(instruction_type::get, scopes[i][current.second]));
					next();
				}
				if(current.first == token_type::num)
				{
					debug("number");
					code.push_back(make_instruction_num(instruction_type::num, std::stof(current.second)));
					next();
				}
				if(current.first == token_type::str) { debug("Str not implemented..."); }
				break;
			}
			default: debug("no parser for " << (int)type); break;
		}
		debug("end parse2");
		group_end();
		debug("end parse");
	};
		while(index < input.size() && input[index].first != token_type::eof)
		parse(expression_type::stmt, 0);
	
	
	//debug1("End");
	for(const auto &x : code)
	{
		debug((int)x.first << " : ");
	}
	//debug1(code.size());
	std::vector<int> variables;
	std::transform(scopes[0].begin(), scopes[0].end(), std::back_inserter(variables), [](auto p){return p.second;});
	return std::make_pair(variables, code);
}




class object
{
public:
	object_type type;
	bool marked;
	std::variant<float, std::string, std::vector<object>, std::monostate> value;

	object(float n) : type(object_type::num), marked(false), value(n) {}
	object(std::string s) : type(object_type::str), marked(false), value(s) {}
	object(std::vector<object> a) : type(object_type::arr), marked(false), value(a) {}
	object() : type(object_type::nil), marked(false), value(std::monostate()) {}
	object(std::monostate n) : type(object_type::nil), marked(false), value(n) {}
};

#define op(name, type, op) case instruction_type::name: \
						   {\
						   	   auto rhs = pop();\
						   	   auto lhs = pop();\
						   	   assert_type(lhs, object_type::type);\
						   	   assert_type(rhs, object_type::type);\
						   	   stack.push_back(object(std::get<0>(lhs.value) op std::get<0>(rhs.value)));\
						   	   break;\
						   }

// +--------------------------------------------------+ //
// |                      THE VM                      | //
// +--------------------------------------------------+ //
// |                                                  | //
// |           A stack-based (of course) vm.          | //
// |                                                  | //
// +--------------------------------------------------+ //
auto eval(std::pair<container<int>, container<instruction>> &input) -> container<object>
{
	//debug1("eval")
	if(input.second.size() == 0) return container<object>();
	//debug1("ok")
	
	//debug1("oof")

	const auto assert_type = [&](const object &o, object_type type) -> bool
	{
		if(o.type != type)
			throw std::runtime_error("Wrong type, expected " + std::to_string((int)type) + std::to_string((int)o.type));
		return true;
	};
	
	std::function<std::string(const object &o)> to_string;
	to_string = [&](const object &o) -> std::string
	{
		switch(o.type)
		{
			case object_type::nil: return "nil";
			case object_type::num: return std::to_string((int)std::get<float>(o.value));
			case object_type::str: return std::get<std::string>(o.value);
			case object_type::arr:
			{
				auto container = std::get<std::vector<object>>(o.value);
				auto begin = container.begin();
				std::string s = to_string(*begin);
				for (auto &it = ++begin; it != container.end(); ++it) s += ", " + to_string(*it);
				return s;
			}
			default: throw std::runtime_error("Not implemented to_string for type" + std::to_string((int)o.type));
		}
	};

	container<std::map<int, object>> scopes;
	scopes.push_back(std::map<int, object>());
	for(const auto &x : input.first) { scopes[0][x] = object(); if(debug_mode) debug3("var: " << x); }
	debug4(scopes.size())

	container<object> stack;
	auto pop = [&]()
	{
		if(stack.size() == 0) throw std::runtime_error("empty stack!");
		auto tmp = stack[stack.size()-1];
		stack.pop_back();
		return tmp;
	};

	//debug1("run")
	int index = 0;
	while(index < input.second.size())
	{
		const auto &ins = input.second[index];
		if(debug_mode) std::cout << "[running]: " << ins.first << std::endl;
		switch(ins.first)
		{
			op(add  , num, +  );
			op(sub  , num, -  );
			op(mul  , num, *  );
			op(div  , num, /  );
			op(eql  , num, == );
			op(gt   , num, >  );
			op(lt   , num, <  );
			op(ge   , num, >= );
			op(le   , num, <= );
			op(or_  , num, || );
			op(and_ , num, && );
			case instruction_type::neg: 
			{
				auto x = pop();
				assert_type(x, object_type::num);
				stack.push_back(object(!std::get<0>(x.value)));
				break;
			}
			case instruction_type::jmp: 
			{
				if(std::get<0>(pop().value))
				{
					index = (int)std::get<float>(ins.second);
					goto nextLoop;
				}
				break;
			}
			case instruction_type::jpa: 
			{
				index = (int)std::get<float>(ins.second);
				goto nextLoop;
			}
			case instruction_type::dup:
			{
				stack.push_back(stack[stack.size()-1]);
				break;
			}
			case instruction_type::num:
			{
				stack.push_back(object(std::get<float>(ins.second)));
				break;
			}
			case instruction_type::get:
			{
				debug1("get " << (int)std::get<float>(ins.second));
				if(scopes.size() == 0) throw std::runtime_error("scope stack collapsed.");
				int i = scopes.size() - 1;
				for(; i >= 0; i--)
					if(scopes[i].find((int)std::get<float>(ins.second)) != scopes[i].end())
						break;
				if(i == -1) throw std::runtime_error("no such variable");
				auto &s = scopes[i];
				stack.push_back(s[(int)std::get<float>(ins.second)]);
				break;
			}
			case instruction_type::set:
			{
				debug1("set " << (int)std::get<float>(ins.second));
				int i = scopes.size() - 1;
				for(; i >= 0; i--)
					if(scopes[i].find((int)std::get<float>(ins.second)) != scopes[i].end())
						break;
				if(i == -1) throw std::runtime_error("no such variable");
				auto &s = scopes.at(i);
				s[(int)std::get<float>(ins.second)] = pop();
				break;
			}
			case instruction_type::debug:
			{
				debug3("print")
				// to_string(pop()).c_str();
				puts(to_string(pop()).c_str());
				break;
			}
			default:
			{
				throw std::runtime_error("Unknown instruction: " + std::to_string((int)ins.first));
			}
		}
		if(debug_mode)
		{
			char c;
			std::cin >> c;
			if(c == 's')
			{
				std::cout << "[============]" << std::endl;
				for(const auto &x : stack)
				{
					std::cout << to_string(x) << std::endl;
				}
				std::cout << "[============]" << std::endl;
				std::cin >> c;
			}
			if(c == 'v')
			{
				for(const auto &x : scopes[0]) { debug3("var: " << x.first); }
				std::cin >> c;
			}
		}
		index++;
		nextLoop:;
	}
	return stack;
}

// +--------------------------------------------------+ //
// |                   ENTRY POINT                    | //
// +--------------------------------------------------+ //
// |                                                  | //
// |         int main(int argc, char *argv[])         | //
// |                                                  | //
// +--------------------------------------------------+ //
int main(int argc, char *argv[])
{
	if(argc < 2) { std::cout << "No input files" << std::endl; return 1; }
	debug_mode = false; // std::find(argv, argv + argc, "-d");
	std::ifstream inp(argv[1]);
	std::chrono::high_resolution_clock::time_point tokTimeStart = std::chrono::high_resolution_clock::now();
	auto toks = tokenize(inp);
	std::chrono::high_resolution_clock::time_point tokTimeEnd = std::chrono::high_resolution_clock::now();
	// for(const auto &tok : toks)
	// {
	// 	std::cout << "Token: " << (int)tok.first << " : " << tok.second << std::endl;
	// }
	std::chrono::high_resolution_clock::time_point prsTimeStart = std::chrono::high_resolution_clock::now();
	try {
		auto code = parse(toks);
		std::chrono::high_resolution_clock::time_point prsTimeEnd = std::chrono::high_resolution_clock::now();
		// for(const auto &ins : code.second)
		// {
		// 	std::cout << "Instruction: " << ins.first;
		// 	if(std::holds_alternative<std::monostate>(ins.second)) // int
		// 		std::cout << " ;" << std::endl;
		// 	else
		// 		std::cout << " : " << std::get<0>(ins.second) << std::endl;
		// }
		std::chrono::high_resolution_clock::time_point runTimeStart = std::chrono::high_resolution_clock::now();
		auto s = eval(code);
		std::chrono::high_resolution_clock::time_point runTimeEnd = std::chrono::high_resolution_clock::now();

		std::chrono::duration<double> tokTime = std::chrono::duration_cast<std::chrono::duration<double>>(tokTimeEnd - tokTimeStart);
		std::chrono::duration<double> prsTime = std::chrono::duration_cast<std::chrono::duration<double>>(prsTimeEnd - prsTimeStart);
		std::chrono::duration<double> runTime = std::chrono::duration_cast<std::chrono::duration<double>>(runTimeEnd - runTimeStart);
		std::chrono::duration<double> allTime = std::chrono::duration_cast<std::chrono::duration<double>>(tokTimeEnd - runTimeStart);
		std::cout << std::endl << "[Finished in " << tokTime.count() << "s]" << std::endl;
		std::cout << "======================" << std::endl;
		std::cout << "Tokenizer: " << tokTime.count() << std::endl;
		std::cout << "Token count: " << toks.size() << std::endl;
		std::cout << "Parser: " << prsTime.count() << std::endl;
		std::cout << "Instruction count: " << code.second.size() << std::endl;
		std::cout << "Eval: " << runTime.count() << std::endl;
	}
	catch(const std::runtime_error& e)
	{
		std::cerr << "[ERROR]: " << e.what() << std::endl;
	}
	return 0;
}
