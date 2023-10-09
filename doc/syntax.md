# aimakakrúgáの構文
バージョン0.1.0時点予定

``` ebnf
programme = { functions };
functions = line_comment | def_function;
line_comment = "--", ? all characters ?;
def_function = def_function_name, "\n", statements;
def_function_name = "[", (function_name | private_function_name), "]";
function_name = variable;
private_function_name = "#" variable;
statements = (line_comment | condition), statements | (statement, { ";" statement }, [ ";" ]);
statement = define_variable | shift_word | call_function;
condition = "if", compare, "{", statements, "}", [ { "elif", compare, "{", statements, "}" } ], [ "else", "{" statements "}" ]; 

define_variable = variable, "=", pattern, { "|", pattern };
pattern = value, { value };
value = literal | variable;

shift_word = match_patterns, [ "when", logic_or ], "->", convert_pattern;
match_patterns = match_pattern, { "|", match_pattern };
match_pattern = [ "^" ], match_value, { match_value }, [ "$" ];
match_value = literal | variable | any_char | "(", match_patterns, ")";
logic_or = logic_and, { "or", logic_and };
logic_and = logic_not, { "and", logic_not };
compare = ([ "not" ], compare_value, compare_op, compare_value) | (left_like_value, "like", right_like_pattern) | ([ "not" ], "(", logic_or, ")");
compare_op = "==" | "/=";
compare_value = literal | reference | match_part | original_word | now_form;
left_like_value = match_part | original_word | now_form;
right_like_pattern = [ '^' ], right_like_value, { right_like_value }, [ '$' ];
right_like_value = literal | variable | reference | any_char;
convert_pattern = convert_value, { convert_value };
convert_value = literal | reference;

call_function = "call", function_name;

variable = alphabet, { alpha_num };
literal = '"' ? all visible characters ? '"';
match_part = "@0";
reference = "@" ? plus number ?;
original_word = "@@";
now_form = "@n";
any_char = ".";
alphabet = ? ascii alphabet and underline ?;
alpha_num = ? ascii alphabet, number and underline ? ;
```