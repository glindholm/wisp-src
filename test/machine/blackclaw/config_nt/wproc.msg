101
a filename immediately following the drive designator\
102
MESSAGE 102\
103
'='\
104
'assign' or some other statement name\
105
MESSAGE 105\
106
a statement\
107
a closing ')'\
108
an expression\
109
a variable name\
110
either 'integer' or 'string'\
111
an array size enclosed in parentheses after 'array'\
112
a string size enclosed in parentheses\
113
a closing ')' after an array index\
114
MESSAGE 114\
115
a closing ')' after an error condition\
116
'begin' or 'end' following 'trace'\
117
a parameter for the Using clause\
118
'=' after the arithmetic operator in an abbreviated assignment\
119
a label name\
120
'from'\
121
either 'to' or 'appended'\
122
'exit' after 'error' or 'cancel'\
123
'input' or 'output'\
124
'to'\
125
"yes" or "no"\
126
'loop'\
127
'to'\
128
'when' or 'end'\
129
'=' or an arithmetic operator\
130
'='.  Note that the abbreviated form of the Assign statement cannot be used
with substrings or arrays\
131
a comma after last option\
132
a field expression\
133
a subroutine name\
134
a color\
135
a foreground color and an optional background color enclosed in parentheses \
136
a list of function keys to be enabled enclosed in parentheses\
137
a field attribute or a variable name\
138
a conditional expression enclosed in parentheses\
139
a conditional expression\
140
a corner specification enclosed in parentheses\
141
a comma and a row number after the column specification\
142
a message line\
143
a file name\
144
a value to be assigned\
145
an expression after 'case'\
146
an expression after 'when'\
147
the lower range of a For loop\
148
the upper range of a For loop\
149
a conditional expression\
150
a value to be returned\
151
an initial value\
152
an array size\
153
a string size\
154
a parameter\
155
a string or key to be typed\
156
a system command string\
157
a string of procedure language statements\
158
an option value\
159
a function key\
160
a coordinate\
161
a loop increment\
162
an environment variable name\
163
a value to be set\
164
'procedure' after 'destroy'\
165
an option after comma\
166
a protection option\
167
the name of variable to be read into\
168
a string to be written\
169
'reading' or 'writing' to specify an open mode\
170
'clear', 'save', 'restore', 'cursor on' or 'cursor off' after 'screen'\
171
'when' following 'show'\
172
a closing ')' after the keyword in a partial backward reference\
173
a library name\
174
a volume name\
175
'file' or 'print'\
176
a variable name or 'type' followed by a permission specification\
177
'message' after 'operator'\
178
'procmsg' after 'options'\
179
'run' or 'all'\
180
proper statement syntax (see your reference manual)\
181
'file', 'library' or 'volume' after 'exists'\
182
a valid keyword (see your reference manual for a list of keywords)\
183
a value in the range 'a' - 'z' or 'A' - 'Z'\
184
'spool' or 'hold'\
185
'scratch', 'requeue' or 'save'\
186
'run' or 'hold'\
187
'program', 'yes' or 'no'\
188
'cancel', 'warn' or 'pause'\
189
'requeue'\
190
':'\
191
a prname\
192
a keyword or a pfkey number\
193
a keyword after comma\
201
Variable ? has not been declared\
202
Integer required\
203
? exceeds the bounds of an integer.  Integer values must be in the
range -2147483647 to 2147483647\
204
Variable ? has already been declared in this procedure\
205
A declaration for variable ? does not appear in the source prior to
this statement.  If it is not visible at runtime as a global variable,
a runtime error will occur here\
206
The Input clause has already been specified\
207
The Cancel Exit clause has already been specified\
208
The Output clause has already been specified\
209
The Error Exit clause has already been specified\
210
The Swap option has already been specified\
211
The Type clause has already been specified\
212
An Exit statement cannot be used here because there is no enclosing
loop to exit from\
213
A Using statement may not appear here\
214
The control variable in a For statement must be declared in this procedure\
215
The control variable in a For statement cannot be an array\
216
The declaration of variable ? is not in the scope of this statement and
cannot be referenced from here\
217
This case is not allowed here because a 'when others' case has already
been specified\
218
? is not a subroutine and cannot be called from here.  Only
a subroutine can be called from another subroutine or from a For loop\
219
This '?' is not terminated by 'end'\
220
? has already been declared as a label in this procedure\
221
Label ? has not been declared\
222
The ? option has already been specified\
223
The ? attribute cannot be used with the Message statement\
224
Control cannot be transferred out of a subroutine or For loop\
225
The declaration of label ? is not in the scope of this statement and
cannot be referenced from here\
226
Label ? has not been declared\
227
A misplaced 'end' was found\
228
Label ? is too long.  Label names must be 1 to 31 characters\
229
The ? option cannot be used with the Message statement\
230
Subroutine ? can only be accessed by using a Call statement\
231
Statements are too deeply nested\
232
Label ? has not been declared\
233
? has already been declared as a subroutine in this procedure\
234
? is invalid as a variable name.  Variable names must be 2 to 31 characters\
235
The ? attribute has already been specified\
236
An array variable cannot be used as a modifiable field in a Prompt statement\
237
Formal parameters cannot be global\
238
An array cannot be specified as a formal parameter\
239
Target label '?' of an Exit clause was not found within the sequence
of statements containing the Run statement\
240
Misplaced Error Exit or Cancel Exit clause.  These clauses must appear
after the Using clause and before the Input, Output, Type or Swap clauses\
241
Invalid syntax.  '?' is not part of the previous statement and is not
the beginning of a new statement\
242
This comment is not terminated by a closing ']'.  The rest of the procedure
has been ignored\
243
The string "?" is not terminated by a closing quote or is too long\
244
A Leave statement cannot be used here because there is no enclosing
subroutine to leave\
245
An incorrect number of parameters have been specified for the ? built-in
function.  Ignore this warning if you have declared a variable with this
same name\
246
"?" is an abbreviated or improper spelling of a statement name\
247
A procedure must begin with either 'procedure' or 'proc'\
248
The ? statement is not supported by WISP and will have no effect at runtime\
249
The syntax beginning with '?' is not supported by WISP and will have no effect
at runtime\
250
A full backward reference is not allowed in this context\
251
Label ? is longer than 8 characters.  The extra characters will be ignored\
252
File name ? is longer than 8 characters.  The extra characters will be
ignored\
253
Library name ? is longer than 8 characters.  The extra characters will be
ignored\
254
Volume name ? is longer than 6 characters.  The extra characters will be
ignored\
255
'?' is not supported by WISP and will be ignored at runtime\
256
The Transparent option has already been specified\
257
Prname ? is longer than 8 characters.  The extra characters will be ignored\
258
Keyword ? is longer than 8 characters.  The extra characters will be ignored\
259
Partial backward references in Display and Enter clauses are not yet
supported by WISP\
401
Division by zero was attempted because a value used as the ? for
the '/' operator is 0\
402
The ? of an expression is outside the range of an integer.  Integer
values must be in the range -2147483647 to 2147483647\
403
The value ? used as the ? is too large for the amount of memory available\
404
The value "?" was specified for the ?, but "yes" or "no" is required\
405
The value ? is not a valid ?.  A value in the range ? to ? is required\
406
Variable ? has not been declared\
407
The global array variable ? is missing an array index\
408
An array index was specified for the global variable ? which is not an array\
409
An unexpected parameter list was found after the array index for the global
integer array variable ?\
410
Substring of the variable ? contains too many parameters\
411
Parameters were specified for the global
integer variable ?, but none are allowed\
412
The value "?" could not be converted to the boolean ? required by
a logical operator\
413
The value "?" could not be converted to the integer ? required by
an arithmetic operator\
414
The value "?" could not be converted to the boolean ? required by
a control statement\
415
The value "?" could not be converted to the ? required by the Return statement\
416
The value "?" could not be converted to the ? required by an array variable\
417
The value "?" could not be converted to the ? required
when assigning to an integer variable\
418
The value "?" could not be converted to the integer ? required\
in a declaration\
419
The value "?" could not be converted to the ? required by this substring\
420
File "?" was not found\
421
File "?" could not be opened for output\
422
File "?" could not be opened for input\
423
Assignment to a built-in function is not allowed\
424
? parameter(s) were specified for a built-in function
that requires ? parameter(s)\
425
The value "?" could not be converted to the integer ? required by
a built-in function\
426
The second operand of a relational expression is an integer type, but the
first operand is a string type.  Note that the &integer built-in function can
be used to change a string type to an integer type\
427
The second operand of a relational expression is a string type, but the first
operand is an integer type.  Note that the &integer built-in function can
be used to change a string type to an integer type\
428
An integer 'when' value cannot be used because the case selection
value is a string\
429
A string 'when' value cannot be used because the case selection
value is an integer\
430
Variable ? cannot be used because its Declare statement has not been executed\
431
Control has fallen into a subroutine.  Subroutines can only executed by
calling them with the Call statement\
432
The maximum call depth of ? has been reached\
433
The value "?" is not a supported ?\
434
The value "?" could not be converted to the integer ? required by
this statement\
435
The field value "?" cannot be displayed because part of it would appear
beyond the right edge of the display area\
436
Only 'numeric' or 'digits' can be used with an integer variable to
indicate that it should be a modifiable field\
437
The value "?" could not be converted to the boolean ? required\
438
The value "?" could not be converted to the ? required by
the &integer built-in function\
439
The field value "?" cannot be displayed because it would appear beyond the
last row of the display area\
440
The field value "?" could not be converted to an integer as required
when an integer variable is used for a modifiable field\
441
The value "?" could not be converted to an integer ? as required
by the 'corners' option\
442
Variable ? was specified as a modifiable field, but its
value is null.  Modifiable fields must contain at least one character\
443
? is not a string array variable.  Only a string array can be used to
receive the names read by the &read_dir built-in function\
444
The maximum For loop depth of ? has been reached\
445
The value "?" could not be converted to an integer ? as required by a For loop\
446
The value "?" could not be converted to an ? as required by
this option\
447
The value "?" was read, but could not be converted to an integer as
required when reading into an integer variable\
448
A substring of a null string is not allowed\
449
The maximum number (?) of screens have already been saved\
450
There is no saved screen to restore\
451
There is not enough memory available to save the screen.  Note
that ? screen(s) are currently saved\
452
The actual parameter value "?" could not be converted an integer
formal parameter\
453
The value being assigned to a dynamic string variable
is too large for the amount of memory available\
454
An incorrect number of parameters were specified for a built-in function
that requires ? to ? parameters\
455
? is not an array.  Only arrays can be sorted\
456
The array ? must be specified without an index when used as a parameter
to the &sort built-in function\
457
The array ? must be specified without an index when used as the second
parameter to the &read_dir built-in function\
458
Assignment to an array variable without an index is not allowed\
459
There is not enough memory to run ?\
460
'end' has been encountered, but there is no corresponding Call to return from\
461
File "?" could not be opened (check access permissions)\
462
Swap error ? occurred while attempting to run ?.  See Appendix B, page 232 of
the Run Reference Manual for an explanation of the error number\
463
This built-in function requires a variable name as a parameter, but
"?" was found\
464
Built-in function ? was specified as a modifiable or tab field, but only
variables are allowed\
465
RunRT cannot run source procedure ?\
466
RunRT does not support the Interpret statement\
467
There is not enough memory to issue system command "?"\
468
The value "?" is not a valid ?.  A class must be in the
range 'a' - 'z' or 'A' - 'Z'\
469
The value "?" was specified for the ?, but "spool" or "hold" is required\
470
The value "?" was specified for the ?, but "scratch",
"requeue" or "save" is required\
471
The value "?" was specified for the ?, but "run" or "hold" is required\
472
The value "?" was specified for the ?, but "program", "yes" or "no" is
required\
473
The value "?" was specified for the ?, but "cancel", "warn" or "pause" is
required\
474
The value "?" was specified for the ?, but "requeue"\
475
Label "?" was not declared in this procedure\
476
Label "?" is not accessible from this location\
477
Link to file "?" failed\
480
Redeclared Global variable can not be re-initialized\
481
Type or size mismatch on redeclaration of Global variable\
501
There is not enough memory to run ?\
502
Procedure ? was cancelled\
503
Could not open file ?\
504
File ? was not found\
505
This software is not installed properly\
506
RunRT cannot interpret source procedure ?\
507
Demo has expired\
508
Demo version of RunRT can only run a demo procedure\
601
Procedure contains a line longer than 255 characters\
602
Insufficient memory to continue\
603
Unable to create file ?\
604
Unable to read work file (possible disk error)\
605
Unable to write work file (disk may be full)\
606
Monitor must be in 80x25 text mode for Message and Prompt\
607
Insufficient memory is available for Run\
608
Procedure contains more than 65535 lines\
609
DOS version 3.0 or greater is required\
610
Invalid .RUN file specified as input\
611
Memory exhausted generating intermediate code (?)\
612
Insufficient memory to load procedure (?)\
613
Procedure is too large (intermediate code exceeds 64K)\
614
No file specified to run\
615
The -c and -s options cannot be used with ?\
616
Extra characters specified after the -? option\
617
Only one option can be specified\
618
Unknown option -? specified\
619
The -t option cannot be used with ?\
620
RunRT does not support option -c or -s\
621
Error ? occurred issuing putparm\
622
Too few parameters passed to procedure\
623
Too many parameters passed to procedure\
631
Unable to create original GLOBALS data file\
632
Unable to open original GLOBALS data file\
633
Unable to create updated GLOBALS data file\
634
Unable to open updated GLOBALS data file\

