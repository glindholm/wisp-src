// Copyright (c) Lexical Software, 1991.  All rights reserved.
// Derived from IBMKEYS.H, (c) South Mountain Software
//
// Module : ibmkeys.hpp
// Author : George Soules
// Date   : 29 May 1991

#ifndef IBMKEYS__HPP
#define IBMKEYS__HPP

// Classes
// (none)

// Definitions and subprograms
// (none)


#if DOS || DOS_HOST

#define KEY_CTRLA        1
#define KEY_CTRLB        2
#define KEY_CTRLC        3
#define KEY_CTRLD        4
#define KEY_CTRLE        5
#define KEY_CTRLF        6
#define KEY_CTRLG        7
#define KEY_CTRLH        8
#define KEY_BACKSPACE    8
#define KEY_CTRLI        9
#define KEY_TAB          9
#define KEY_CTRLRETURN  10
#define KEY_CTRLJ       10
#define KEY_CTRLK       11
#define KEY_CTRLL       12
#define KEY_CTRLM       13
#define KEY_RETURN      13
#define KEY_CTRLN       14
#define KEY_CTRLO       15
#define KEY_CTRLP       16
#define KEY_CTRLQ       17
#define KEY_CTRLR       18
#define KEY_CTRLS       19
#define KEY_CTRLT       20
#define KEY_CTRLU       21
#define KEY_CTRLV       22
#define KEY_CTRLW       23
#define KEY_CTRLX       24
#define KEY_CTRLY       25
#define KEY_CTRLZ       26
#define KEY_ESCAPE      27

#define KEY_SPACE       32
#define KEY_EXCLAMATION 33
#define KEY_DBLQUOTE    34
#define KEY_LBSIGN      35
#define KEY_DOLLARSIGN  36
#define KEY_PCTSIGN     37
#define KEY_AMPERSAND   38
#define KEY_SGLQUOTE    39
#define KEY_LFPAREN     40
#define KEY_RTPAREN     41
#define KEY_ASTERISK    42
#define KEY_PLUSSIGN    43
#define KEY_COMMA       44
#define KEY_DASH        45
#define KEY_PERIOD      46
#define KEY_SLASH       47
#define KEY_ZERO        48
#define KEY_ONE         49
#define KEY_TWO         50
#define KEY_THREE       51
#define KEY_FOUR        52
#define KEY_FIVE        53
#define KEY_SIX         54
#define KEY_SEVEN       55
#define KEY_EIGHT       56
#define KEY_NINE        57
#define KEY_COLON       58
#define KEY_SEMICOLON   59
#define KEY_LTH         60
#define KEY_EQUAL       61
#define KEY_EQU         61
#define KEY_GTH         62
#define KEY_QUESTIONMK  63
#define KEY_ATSIGN      64
#define KEY_UPPER_A     65
#define KEY_UPPER_B     66
#define KEY_UPPER_C     67
#define KEY_UPPER_D     68
#define KEY_UPPER_E     69
#define KEY_UPPER_F     70
#define KEY_UPPER_G     71
#define KEY_UPPER_H     72
#define KEY_UPPER_I     73
#define KEY_UPPER_J     74
#define KEY_UPPER_K     75
#define KEY_UPPER_L     76
#define KEY_UPPER_M     77
#define KEY_UPPER_N     78
#define KEY_UPPER_O     79
#define KEY_UPPER_P     80
#define KEY_UPPER_Q     81
#define KEY_UPPER_R     82
#define KEY_UPPER_S     83
#define KEY_UPPER_T     84
#define KEY_UPPER_U     85
#define KEY_UPPER_V     86
#define KEY_UPPER_W     87
#define KEY_UPPER_X     88
#define KEY_UPPER_Y     89
#define KEY_UPPER_Z     90
#define KEY_LTBRACKET   91
#define KEY_BKSLASH     92
#define KEY_RTBRACKET   93
#define KEY_CAROT       94
#define KEY_UNDERSCORE  95
#define KEY_REVQUOTE    96
#define KEY_LOWER_A     97
#define KEY_LOWER_B     98
#define KEY_LOWER_C     99
#define KEY_LOWER_D    100
#define KEY_LOWER_E    101
#define KEY_LOWER_F    102
#define KEY_LOWER_G    103
#define KEY_LOWER_H    104
#define KEY_LOWER_I    105
#define KEY_LOWER_J    106
#define KEY_LOWER_K    107
#define KEY_LOWER_L    108
#define KEY_LOWER_M    109
#define KEY_LOWER_N    110
#define KEY_LOWER_O    111
#define KEY_LOWER_P    112
#define KEY_LOWER_Q    113
#define KEY_LOWER_R    114
#define KEY_LOWER_S    115
#define KEY_LOWER_T    116
#define KEY_LOWER_U    117
#define KEY_LOWER_V    118
#define KEY_LOWER_W    119
#define KEY_LOWER_X    120
#define KEY_LOWER_Y    121
#define KEY_LOWER_Z    122
#define KEY_LTBRACE    123
#define KEY_OR         124
#define KEY_RTBRACE    125
#define KEY_TILDE      126
#define KEY_CTRLBACKSP 127


// Extended keys

#define KEY_REVTAB      -15
#define KEY_ALTQ        -16
#define KEY_ALTW        -17
#define KEY_ALTE        -18
#define KEY_ALTR        -19
#define KEY_ALTT        -20
#define KEY_ALTY        -21
#define KEY_ALTU        -22
#define KEY_ALTI        -23
#define KEY_ALTO        -24
#define KEY_ALTP        -25

#define KEY_ALTA        -30
#define KEY_ALTS        -31
#define KEY_ALTD        -32
#define KEY_ALTF        -33
#define KEY_ALTG        -34
#define KEY_ALTH        -35
#define KEY_ALTJ        -36
#define KEY_ALTK        -37
#define KEY_ALTL        -38

#define KEY_ALTZ        -44
#define KEY_ALTX        -45
#define KEY_ALTC        -46
#define KEY_ALTV        -47
#define KEY_ALTB        -48
#define KEY_ALTN        -49
#define KEY_ALTM        -50

#define KEY_F1          -59
#define KEY_F2          -60
#define KEY_F3          -61
#define KEY_F4          -62
#define KEY_F5          -63
#define KEY_F6          -64
#define KEY_F7          -65
#define KEY_F8          -66
#define KEY_F9          -67
#define KEY_F10         -68
#define KEY_HOME        -71
#define KEY_CURUP       -72
#define KEY_PGUP        -73
#define KEY_CURLF       -75
#define KEY_CURRT       -77
#define KEY_END         -79
#define KEY_CURDN       -80
#define KEY_PGDN        -81
#define KEY_INSERT      -82
#define KEY_DELETE      -83

#define KEY_SF1         -84
#define KEY_SF2         -85
#define KEY_SF3         -86
#define KEY_SF4         -87
#define KEY_SF5         -88
#define KEY_SF6         -89
#define KEY_SF7         -90
#define KEY_SF8         -91
#define KEY_SF9         -92
#define KEY_SF10        -93

#define KEY_CF1         -94
#define KEY_CF2         -95
#define KEY_CF3         -96
#define KEY_CF4         -97
#define KEY_CF5         -98
#define KEY_CF6         -99
#define KEY_CF7        -100
#define KEY_CF8        -101
#define KEY_CF9        -102
#define KEY_CF10       -103

#define KEY_AF1        -104
#define KEY_AF2        -105
#define KEY_AF3        -106
#define KEY_AF4        -107
#define KEY_AF5        -108
#define KEY_AF6        -109
#define KEY_AF7        -110
#define KEY_AF8        -111
#define KEY_AF9        -112
#define KEY_AF10       -113

#define KEY_CTRLPRTSC  -114
#define KEY_CTRLCURLF  -115
#define KEY_CTRLCURRT  -116
#define KEY_CTRLEND    -117
#define KEY_CTRLPGDN   -118
#define KEY_CTRLHOME   -119
#define KEY_ALT1       -120
#define KEY_ALT2       -121
#define KEY_ALT3       -122
#define KEY_ALT4       -123
#define KEY_ALT5       -124
#define KEY_ALT6       -125
#define KEY_ALT7       -126
#define KEY_ALT8       -127
#define KEY_ALT9       -128
#define KEY_ALT0       -129
#define KEY_ALTMINUS   -130
#define KEY_ALTEQUAL   -131
#define KEY_CTRLPGUP   -132
#define KEY_F11        -133
#define KEY_F12        -134
#define KEY_SF11       -135
#define KEY_SF12       -136

#else /* DOS || DOS_HOST */
#if WANG

/*
**	The value of the VIDEO MetaCharacters are normally
**	provided in "vdata.h".  This however was not used
**	because of name conflicts etc.
*/
#define VMBIAS 256
#define FKBIAS 39

#define KEY_CTRLA        1
#define KEY_CTRLB        2
#define KEY_CTRLC        3
#define KEY_CTRLD        4
#define KEY_CTRLE        5
#define KEY_CTRLF        6
#define KEY_CTRLG        7
#define KEY_CTRLH        8
#define KEY_BACKSPACE    (79+VMBIAS)
#define KEY_CTRLI        9
#define KEY_TAB          (78+VMBIAS)
#define KEY_CTRLRETURN  10
#define KEY_CTRLJ       10
#define KEY_CTRLK       11
#define KEY_CTRLL       12
#define KEY_CTRLM       13
#define KEY_RETURN      (92+VMBIAS)
#define KEY_CTRLN       14
#define KEY_CTRLO       15
#define KEY_CTRLP       16
#define KEY_CTRLQ       17
#define KEY_CTRLR       18
#define KEY_CTRLS       19
#define KEY_CTRLT       20
#define KEY_CTRLU       21
#define KEY_CTRLV       22
#define KEY_CTRLW       23
#define KEY_CTRLX       24
#define KEY_CTRLY       25
#define KEY_CTRLZ       26
#define KEY_ESCAPE      27

#define KEY_SPACE       32
#define KEY_EXCLAMATION 33
#define KEY_DBLQUOTE    34
#define KEY_LBSIGN      35
#define KEY_DOLLARSIGN  36
#define KEY_PCTSIGN     37
#define KEY_AMPERSAND   38
#define KEY_SGLQUOTE    39
#define KEY_LFPAREN     40
#define KEY_RTPAREN     41
#define KEY_ASTERISK    42
#define KEY_PLUSSIGN    43
#define KEY_COMMA       44
#define KEY_DASH        45
#define KEY_PERIOD      46
#define KEY_SLASH       47
#define KEY_ZERO        48
#define KEY_ONE         49
#define KEY_TWO         50
#define KEY_THREE       51
#define KEY_FOUR        52
#define KEY_FIVE        53
#define KEY_SIX         54
#define KEY_SEVEN       55
#define KEY_EIGHT       56
#define KEY_NINE        57
#define KEY_COLON       58
#define KEY_SEMICOLON   59
#define KEY_LTH         60
#define KEY_EQUAL       61
#define KEY_EQU         61
#define KEY_GTH         62
#define KEY_QUESTIONMK  63
#define KEY_ATSIGN      64
#define KEY_UPPER_A     65
#define KEY_UPPER_B     66
#define KEY_UPPER_C     67
#define KEY_UPPER_D     68
#define KEY_UPPER_E     69
#define KEY_UPPER_F     70
#define KEY_UPPER_G     71
#define KEY_UPPER_H     72
#define KEY_UPPER_I     73
#define KEY_UPPER_J     74
#define KEY_UPPER_K     75
#define KEY_UPPER_L     76
#define KEY_UPPER_M     77
#define KEY_UPPER_N     78
#define KEY_UPPER_O     79
#define KEY_UPPER_P     80
#define KEY_UPPER_Q     81
#define KEY_UPPER_R     82
#define KEY_UPPER_S     83
#define KEY_UPPER_T     84
#define KEY_UPPER_U     85
#define KEY_UPPER_V     86
#define KEY_UPPER_W     87
#define KEY_UPPER_X     88
#define KEY_UPPER_Y     89
#define KEY_UPPER_Z     90
#define KEY_LTBRACKET   91
#define KEY_BKSLASH     92
#define KEY_RTBRACKET   93
#define KEY_CAROT       94
#define KEY_UNDERSCORE  95
#define KEY_REVQUOTE    96
#define KEY_LOWER_A     97
#define KEY_LOWER_B     98
#define KEY_LOWER_C     99
#define KEY_LOWER_D    100
#define KEY_LOWER_E    101
#define KEY_LOWER_F    102
#define KEY_LOWER_G    103
#define KEY_LOWER_H    104
#define KEY_LOWER_I    105
#define KEY_LOWER_J    106
#define KEY_LOWER_K    107
#define KEY_LOWER_L    108
#define KEY_LOWER_M    109
#define KEY_LOWER_N    110
#define KEY_LOWER_O    111
#define KEY_LOWER_P    112
#define KEY_LOWER_Q    113
#define KEY_LOWER_R    114
#define KEY_LOWER_S    115
#define KEY_LOWER_T    116
#define KEY_LOWER_U    117
#define KEY_LOWER_V    118
#define KEY_LOWER_W    119
#define KEY_LOWER_X    120
#define KEY_LOWER_Y    121
#define KEY_LOWER_Z    122
#define KEY_LTBRACE    123
#define KEY_OR         124
#define KEY_RTBRACE    125
#define KEY_TILDE      126
#define KEY_CTRLBACKSP 127


// Extended keys

#define KEY_REVTAB      (88+VMBIAS)
#define KEY_ALTQ        -16
#define KEY_ALTW        -17
#define KEY_ALTE        -18
#define KEY_ALTR        -19
#define KEY_ALTT        -20
#define KEY_ALTY        -21
#define KEY_ALTU        -22
#define KEY_ALTI        -23
#define KEY_ALTO        -24
#define KEY_ALTP        -25

#define KEY_ALTA        -30
#define KEY_ALTS        -31
#define KEY_ALTD        -32
#define KEY_ALTF        -33
#define KEY_ALTG        -34
#define KEY_ALTH        -35
#define KEY_ALTJ        -36
#define KEY_ALTK        -37
#define KEY_ALTL        -38

#define KEY_ALTZ        -44
#define KEY_ALTX        -45
#define KEY_ALTC        -46
#define KEY_ALTV        -47
#define KEY_ALTB        -48
#define KEY_ALTN        -49
#define KEY_ALTM        -50

#define KEY_F1          (1+FKBIAS+VMBIAS)
#define KEY_F2          (2+FKBIAS+VMBIAS)
#define KEY_F3          (3+FKBIAS+VMBIAS)
#define KEY_F4          (4+FKBIAS+VMBIAS)
#define KEY_F5          (5+FKBIAS+VMBIAS)
#define KEY_F6          (6+FKBIAS+VMBIAS)
#define KEY_F7          (7+FKBIAS+VMBIAS)
#define KEY_F8          (8+FKBIAS+VMBIAS)
#define KEY_F9          (9+FKBIAS+VMBIAS)
#define KEY_F10         (10+FKBIAS+VMBIAS)

#define KEY_HOME        (72+VMBIAS)
#define KEY_HELP	(73+VMBIAS)
#define KEY_CURUP       (74+VMBIAS)
#define KEY_PGUP        (82+VMBIAS)
#define KEY_CURLF       (76+VMBIAS)
#define KEY_CURRT       (77+VMBIAS)
#define KEY_END         -79
#define KEY_CURDN       (75+VMBIAS)
#define KEY_PGDN        (81+VMBIAS)
#define KEY_INSERT      (80+VMBIAS)
#define KEY_DELETE      (89+VMBIAS)

#define KEY_SF1         (17+FKBIAS+VMBIAS)
#define KEY_SF2         (18+FKBIAS+VMBIAS)
#define KEY_SF3         (19+FKBIAS+VMBIAS)
#define KEY_SF4         (20+FKBIAS+VMBIAS)
#define KEY_SF5         (21+FKBIAS+VMBIAS)
#define KEY_SF6         (22+FKBIAS+VMBIAS)
#define KEY_SF7         (23+FKBIAS+VMBIAS)
#define KEY_SF8         (24+FKBIAS+VMBIAS)
#define KEY_SF9         (25+FKBIAS+VMBIAS)
#define KEY_SF10        (26+FKBIAS+VMBIAS)

#define KEY_CF1         (27+FKBIAS+VMBIAS)
#define KEY_CF2         (28+FKBIAS+VMBIAS)
#define KEY_CF3         (29+FKBIAS+VMBIAS)
#define KEY_CF4         (30+FKBIAS+VMBIAS)
#define KEY_CF5         (31+FKBIAS+VMBIAS)
#define KEY_CF6         (32+FKBIAS+VMBIAS)
#define KEY_CF7        -100
#define KEY_CF8        -101
#define KEY_CF9        -102
#define KEY_CF10       -103

#define KEY_AF1        (11+FKBIAS+VMBIAS)
#define KEY_AF2        (12+FKBIAS+VMBIAS)
#define KEY_AF3        (13+FKBIAS+VMBIAS)
#define KEY_AF4        (14+FKBIAS+VMBIAS)
#define KEY_AF5        (15+FKBIAS+VMBIAS)
#define KEY_AF6        (16+FKBIAS+VMBIAS)
#define KEY_AF7        -110
#define KEY_AF8        -111
#define KEY_AF9        -112
#define KEY_AF10       -113

#define KEY_CTRLPRTSC  -114
#define KEY_CTRLCURLF  -115
#define KEY_CTRLCURRT  -116
#define KEY_CTRLEND    -117
#define KEY_CTRLPGDN   -118
#define KEY_CTRLHOME   -119
#define KEY_ALT1       -120
#define KEY_ALT2       -121
#define KEY_ALT3       -122
#define KEY_ALT4       -123
#define KEY_ALT5       -124
#define KEY_ALT6       -125
#define KEY_ALT7       -126
#define KEY_ALT8       -127
#define KEY_ALT9       -128
#define KEY_ALT0       -129
#define KEY_ALTMINUS   -130
#define KEY_ALTEQUAL   -131
#define KEY_CTRLPGUP   -132
#define KEY_F11        (11+FKBIAS+VMBIAS)
#define KEY_F12        (12+FKBIAS+VMBIAS)
#define KEY_SF11       (27+FKBIAS+VMBIAS)
#define KEY_SF12       (28+FKBIAS+VMBIAS)

#define KEY_MOUSE_CLICK (224+VMBIAS)

#endif /* WANG */
#endif /* !(DOS || DOS_HOST) */

#endif
