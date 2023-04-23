# aimakakrúgáの構文
バージョン0.0.6時点

``` ebnf
programme = { statements };
statements = line_comment | statement , { [ ";" ] statement }, [ ";" ], "\n";
line_comment = "--", ? all characters ?;
statement = define_variable | shift_word;

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

variable = alpha_num, { alpha_num };
literal = '"' ? all visible characters ? '"';
match_part = "@0";
reference = "@" ? plus number ?;
original_word = "@@";
now_form = "@n";
any_char = ".";
alpha_num = ? ascii alphabet, number and underline ? ;
```