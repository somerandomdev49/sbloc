<h1 align="center">SBLOC</h1>
<h3 align="center">Stack-Based Language Of Course</h3>
Dynamically typed, because it's easier. (in future may be statically typed)

I making this language in preparation for the [Bear](https://github.com/bear-lang/Bear) language.

# Example:
```ruby
var this_will_not_do_anything_but_arguments_are_parsed;
print(11, this_will_not_do_anything_but_arguments_are_parsed);
if 21 == 12
{
    print(1);
    print(2);
}
else
{
    print(0);
}
print(90123);


var x;
x = 10;
while x > 0
{
    print(x);
    x = x - 1;
}
print(x);
```

# Syntax:
* print statement: `'print' expression ';'`
* variable declaration: `'var' name ';'`
* if statement: `'if' expression statement [ 'else' statement ]`
* while statement: `'while' expression statement`
* block statement: `'{' statement... '}'`
* set expression: `name '=' expression`
* expression: `expression postfix`
* postfix: `[ '(' expression... ')' ]...`

# Bytecode:
| Name | Description |
|---|---|
| get N | pushes value of variable with id N |
| set N | pops a value and sets variable with id N |
| nop | ... |
|add, sub, mul, div, and_, eql, or_, lt, gt, le, ge | Math `push(pop() X pop())` |
| num N | pushes number N onto the stack |
| debug | pops a value and prints it.  (temp. will be removed after functions) |
| dup   | `push(top())` |
| neg   | `push(!pop())` |
| jpa N | set current instruction index to N |
| jmp N | if popped value is not 0 then set current instruction index to N |

# Code:
Pasta
