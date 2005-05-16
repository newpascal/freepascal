{
   $Id: gdkkeysyms.pp,v 1.3 2005/02/14 17:13:20 peter Exp $
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

const
   GDK_VoidSymbol = $FFFFFF;
   GDK_BackSpace = $FF08;
   GDK_Tab = $FF09;
   GDK_Linefeed = $FF0A;
   GDK_Clear_Key = $FF0B;
   GDK_Return = $FF0D;
   GDK_Pause = $FF13;
   GDK_Scroll_Lock = $FF14;
   GDK_Sys_Req = $FF15;
   GDK_Escape = $FF1B;
   GDK_Delete_Key = $FFFF;
   GDK_Multi_key = $FF20;
   GDK_SingleCandidate = $FF3C;
   GDK_MultipleCandidate = $FF3D;
   GDK_PreviousCandidate = $FF3E;
   GDK_Kanji = $FF21;
   GDK_Muhenkan = $FF22;
   GDK_Henkan_Mode = $FF23;
   GDK_Henkan = $FF23;
   GDK_Romaji = $FF24;
   GDK_Hiragana = $FF25;
   GDK_Katakana = $FF26;
   GDK_Hiragana_Katakana = $FF27;
   GDK_Zenkaku = $FF28;
   GDK_Hankaku = $FF29;
   GDK_Zenkaku_Hankaku = $FF2A;
   GDK_Touroku = $FF2B;
   GDK_Massyo = $FF2C;
   GDK_Kana_Lock = $FF2D;
   GDK_Kana_Shift = $FF2E;
   GDK_Eisu_Shift = $FF2F;
   GDK_Eisu_toggle = $FF30;
   GDK_Zen_Koho = $FF3D;
   GDK_Mae_Koho = $FF3E;
   GDK_Home = $FF50;
   GDK_Left = $FF51;
   GDK_Up = $FF52;
   GDK_Right = $FF53;
   GDK_Down = $FF54;
   GDK_Prior = $FF55;
   GDK_Page_Up = $FF55;
   GDK_Next = $FF56;
   GDK_Page_Down = $FF56;
   GDK_End = $FF57;
   GDK_Begin = $FF58;
   GDK_Select = $FF60;
   GDK_Print = $FF61;
   GDK_Execute = $FF62;
   GDK_Insert = $FF63;
   GDK_Undo = $FF65;
   GDK_Redo = $FF66;
   GDK_Menu = $FF67;
   GDK_Find = $FF68;
   GDK_Cancel = $FF69;
   GDK_Help = $FF6A;
   GDK_Break = $FF6B;
   GDK_Mode_switch = $FF7E;
   GDK_script_switch = $FF7E;
   GDK_Num_Lock = $FF7F;
   GDK_KP_Space = $FF80;
   GDK_KP_Tab = $FF89;
   GDK_KP_Enter = $FF8D;
   GDK_KP_F1 = $FF91;
   GDK_KP_F2 = $FF92;
   GDK_KP_F3 = $FF93;
   GDK_KP_F4 = $FF94;
   GDK_KP_Home = $FF95;
   GDK_KP_Left = $FF96;
   GDK_KP_Up = $FF97;
   GDK_KP_Right = $FF98;
   GDK_KP_Down = $FF99;
   GDK_KP_Prior = $FF9A;
   GDK_KP_Page_Up = $FF9A;
   GDK_KP_Next = $FF9B;
   GDK_KP_Page_Down = $FF9B;
   GDK_KP_End = $FF9C;
   GDK_KP_Begin = $FF9D;
   GDK_KP_Insert = $FF9E;
   GDK_KP_Delete = $FF9F;
   GDK_KP_Equal = $FFBD;
   GDK_KP_Multiply = $FFAA;
   GDK_KP_Add = $FFAB;
   GDK_KP_Separator = $FFAC;
   GDK_KP_Subtract = $FFAD;
   GDK_KP_Decimal = $FFAE;
   GDK_KP_Divide = $FFAF;
   GDK_KP_0 = $FFB0;
   GDK_KP_1 = $FFB1;
   GDK_KP_2 = $FFB2;
   GDK_KP_3 = $FFB3;
   GDK_KP_4 = $FFB4;
   GDK_KP_5 = $FFB5;
   GDK_KP_6 = $FFB6;
   GDK_KP_7 = $FFB7;
   GDK_KP_8 = $FFB8;
   GDK_KP_9 = $FFB9;
   GDK_F1 = $FFBE;
   GDK_F2 = $FFBF;
   GDK_F3 = $FFC0;
   GDK_F4 = $FFC1;
   GDK_F5 = $FFC2;
   GDK_F6 = $FFC3;
   GDK_F7 = $FFC4;
   GDK_F8 = $FFC5;
   GDK_F9 = $FFC6;
   GDK_F10 = $FFC7;
   GDK_F11 = $FFC8;
   GDK_L1 = $FFC8;
   GDK_F12 = $FFC9;
   GDK_L2 = $FFC9;
   GDK_F13 = $FFCA;
   GDK_L3 = $FFCA;
   GDK_F14 = $FFCB;
   GDK_L4 = $FFCB;
   GDK_F15 = $FFCC;
   GDK_L5 = $FFCC;
   GDK_F16 = $FFCD;
   GDK_L6 = $FFCD;
   GDK_F17 = $FFCE;
   GDK_L7 = $FFCE;
   GDK_F18 = $FFCF;
   GDK_L8 = $FFCF;
   GDK_F19 = $FFD0;
   GDK_L9 = $FFD0;
   GDK_F20 = $FFD1;
   GDK_L10 = $FFD1;
   GDK_F21 = $FFD2;
   GDK_R1 = $FFD2;
   GDK_F22 = $FFD3;
   GDK_R2 = $FFD3;
   GDK_F23 = $FFD4;
   GDK_R3 = $FFD4;
   GDK_F24 = $FFD5;
   GDK_R4 = $FFD5;
   GDK_F25 = $FFD6;
   GDK_R5 = $FFD6;
   GDK_F26 = $FFD7;
   GDK_R6 = $FFD7;
   GDK_F27 = $FFD8;
   GDK_R7 = $FFD8;
   GDK_F28 = $FFD9;
   GDK_R8 = $FFD9;
   GDK_F29 = $FFDA;
   GDK_R9 = $FFDA;
   GDK_F30 = $FFDB;
   GDK_R10 = $FFDB;
   GDK_F31 = $FFDC;
   GDK_R11 = $FFDC;
   GDK_F32 = $FFDD;
   GDK_R12 = $FFDD;
   GDK_F33 = $FFDE;
   GDK_R13 = $FFDE;
   GDK_F34 = $FFDF;
   GDK_R14 = $FFDF;
   GDK_F35 = $FFE0;
   GDK_R15 = $FFE0;
   GDK_Shift_L = $FFE1;
   GDK_Shift_R = $FFE2;
   GDK_Control_L = $FFE3;
   GDK_Control_R = $FFE4;
   GDK_Caps_Lock = $FFE5;
   GDK_Shift_Lock = $FFE6;
   GDK_Meta_L = $FFE7;
   GDK_Meta_R = $FFE8;
   GDK_Alt_L = $FFE9;
   GDK_Alt_R = $FFEA;
   GDK_Super_L = $FFEB;
   GDK_Super_R = $FFEC;
   GDK_Hyper_L = $FFED;
   GDK_Hyper_R = $FFEE;
   GDK_ISO_Lock = $FE01;
   GDK_ISO_Level2_Latch = $FE02;
   GDK_ISO_Level3_Shift = $FE03;
   GDK_ISO_Level3_Latch = $FE04;
   GDK_ISO_Level3_Lock = $FE05;
   GDK_ISO_Group_Shift = $FF7E;
   GDK_ISO_Group_Latch = $FE06;
   GDK_ISO_Group_Lock = $FE07;
   GDK_ISO_Next_Group = $FE08;
   GDK_ISO_Next_Group_Lock = $FE09;
   GDK_ISO_Prev_Group = $FE0A;
   GDK_ISO_Prev_Group_Lock = $FE0B;
   GDK_ISO_First_Group = $FE0C;
   GDK_ISO_First_Group_Lock = $FE0D;
   GDK_ISO_Last_Group = $FE0E;
   GDK_ISO_Last_Group_Lock = $FE0F;
   GDK_ISO_Left_Tab = $FE20;
   GDK_ISO_Move_Line_Up = $FE21;
   GDK_ISO_Move_Line_Down = $FE22;
   GDK_ISO_Partial_Line_Up = $FE23;
   GDK_ISO_Partial_Line_Down = $FE24;
   GDK_ISO_Partial_Space_Left = $FE25;
   GDK_ISO_Partial_Space_Right = $FE26;
   GDK_ISO_Set_Margin_Left = $FE27;
   GDK_ISO_Set_Margin_Right = $FE28;
   GDK_ISO_Release_Margin_Left = $FE29;
   GDK_ISO_Release_Margin_Right = $FE2A;
   GDK_ISO_Release_Both_Margins = $FE2B;
   GDK_ISO_Fast_Cursor_Left = $FE2C;
   GDK_ISO_Fast_Cursor_Right = $FE2D;
   GDK_ISO_Fast_Cursor_Up = $FE2E;
   GDK_ISO_Fast_Cursor_Down = $FE2F;
   GDK_ISO_Continuous_Underline = $FE30;
   GDK_ISO_Discontinuous_Underline = $FE31;
   GDK_ISO_Emphasize = $FE32;
   GDK_ISO_Center_Object = $FE33;
   GDK_ISO_Enter = $FE34;
   GDK_dead_grave = $FE50;
   GDK_dead_acute = $FE51;
   GDK_dead_circumflex = $FE52;
   GDK_dead_tilde = $FE53;
   GDK_dead_macron = $FE54;
   GDK_dead_breve = $FE55;
   GDK_dead_abovedot = $FE56;
   GDK_dead_diaeresis = $FE57;
   GDK_dead_abovering = $FE58;
   GDK_dead_doubleacute = $FE59;
   GDK_dead_caron = $FE5A;
   GDK_dead_cedilla = $FE5B;
   GDK_dead_ogonek = $FE5C;
   GDK_dead_iota = $FE5D;
   GDK_dead_voiced_sound = $FE5E;
   GDK_dead_semivoiced_sound = $FE5F;
   GDK_dead_belowdot = $FE60;
   GDK_First_Virtual_Screen = $FED0;
   GDK_Prev_Virtual_Screen = $FED1;
   GDK_Next_Virtual_Screen = $FED2;
   GDK_Last_Virtual_Screen = $FED4;
   GDK_Terminate_Server = $FED5;
   GDK_AccessX_Enable = $FE70;
   GDK_AccessX_Feedback_Enable = $FE71;
   GDK_RepeatKeys_Enable = $FE72;
   GDK_SlowKeys_Enable = $FE73;
   GDK_BounceKeys_Enable = $FE74;
   GDK_StickyKeys_Enable = $FE75;
   GDK_MouseKeys_Enable = $FE76;
   GDK_MouseKeys_Accel_Enable = $FE77;
   GDK_Overlay1_Enable = $FE78;
   GDK_Overlay2_Enable = $FE79;
   GDK_AudibleBell_Enable = $FE7A;
   GDK_Pointer_Left = $FEE0;
   GDK_Pointer_Right = $FEE1;
   GDK_Pointer_Up = $FEE2;
   GDK_Pointer_Down = $FEE3;
   GDK_Pointer_UpLeft = $FEE4;
   GDK_Pointer_UpRight = $FEE5;
   GDK_Pointer_DownLeft = $FEE6;
   GDK_Pointer_DownRight = $FEE7;
   GDK_Pointer_Button_Dflt = $FEE8;
   GDK_Pointer_Button1 = $FEE9;
   GDK_Pointer_Button2 = $FEEA;
   GDK_Pointer_Button3 = $FEEB;
   GDK_Pointer_Button4 = $FEEC;
   GDK_Pointer_Button5 = $FEED;
   GDK_Pointer_DblClick_Dflt = $FEEE;
   GDK_Pointer_DblClick1 = $FEEF;
   GDK_Pointer_DblClick2 = $FEF0;
   GDK_Pointer_DblClick3 = $FEF1;
   GDK_Pointer_DblClick4 = $FEF2;
   GDK_Pointer_DblClick5 = $FEF3;
   GDK_Pointer_Drag_Dflt = $FEF4;
   GDK_Pointer_Drag1 = $FEF5;
   GDK_Pointer_Drag2 = $FEF6;
   GDK_Pointer_Drag3 = $FEF7;
   GDK_Pointer_Drag4 = $FEF8;
   GDK_Pointer_Drag5 = $FEFD;
   GDK_Pointer_EnableKeys = $FEF9;
   GDK_Pointer_Accelerate = $FEFA;
   GDK_Pointer_DfltBtnNext = $FEFB;
   GDK_Pointer_DfltBtnPrev = $FEFC;
   GDK_3270_Duplicate = $FD01;
   GDK_3270_FieldMark = $FD02;
   GDK_3270_Right2 = $FD03;
   GDK_3270_Left2 = $FD04;
   GDK_3270_BackTab = $FD05;
   GDK_3270_EraseEOF = $FD06;
   GDK_3270_EraseInput = $FD07;
   GDK_3270_Reset = $FD08;
   GDK_3270_Quit = $FD09;
   GDK_3270_PA1 = $FD0A;
   GDK_3270_PA2 = $FD0B;
   GDK_3270_PA3 = $FD0C;
   GDK_3270_Test = $FD0D;
   GDK_3270_Attn = $FD0E;
   GDK_3270_CursorBlink = $FD0F;
   GDK_3270_AltCursor = $FD10;
   GDK_3270_KeyClick = $FD11;
   GDK_3270_Jump = $FD12;
   GDK_3270_Ident = $FD13;
   GDK_3270_Rule = $FD14;
   GDK_3270_Copy = $FD15;
   GDK_3270_Play = $FD16;
   GDK_3270_Setup = $FD17;
   GDK_3270_Record = $FD18;
   GDK_3270_ChangeScreen = $FD19;
   GDK_3270_DeleteWord = $FD1A;
   GDK_3270_ExSelect = $FD1B;
   GDK_3270_CursorSelect = $FD1C;
   GDK_3270_PrintScreen = $FD1D;
   GDK_3270_Enter = $FD1E;
   GDK_space = $020;
   GDK_exclam = $021;
   GDK_quotedbl = $022;
   GDK_numbersign = $023;
   GDK_dollar = $024;
   GDK_percent = $025;
   GDK_ampersand = $026;
   GDK_apostrophe = $027;
   GDK_quoteright = $027;
   GDK_parenleft = $028;
   GDK_parenright = $029;
   GDK_asterisk = $02a;
   GDK_plus_key = $02b;
   GDK_comma = $02c;
   GDK_minus = $02d;
   GDK_period = $02e;
   GDK_slash = $02f;
   GDK_0 = $030;
   GDK_1 = $031;
   GDK_2 = $032;
   GDK_3 = $033;
   GDK_4 = $034;
   GDK_5 = $035;
   GDK_6 = $036;
   GDK_7 = $037;
   GDK_8 = $038;
   GDK_9 = $039;
   GDK_colon = $03a;
   GDK_semicolon = $03b;
   GDK_less = $03c;
   GDK_equal = $03d;
   GDK_greater = $03e;
   GDK_question = $03f;
   GDK_at = $040;
   GDK_Capital_A = $041;
   GDK_Capital_B = $042;
   GDK_Capital_C = $043;
   GDK_Capital_D = $044;
   GDK_Capital_E = $045;
   GDK_Capital_F = $046;
   GDK_Capital_G = $047;
   GDK_Capital_H = $048;
   GDK_Capital_I = $049;
   GDK_Capital_J = $04a;
   GDK_Capital_K = $04b;
   GDK_Capital_L = $04c;
   GDK_Capital_M = $04d;
   GDK_Capital_N = $04e;
   GDK_Capital_O = $04f;
   GDK_Capital_P = $050;
   GDK_Capital_Q = $051;
   GDK_Capital_R = $052;
   GDK_Capital_S = $053;
   GDK_Capital_T = $054;
   GDK_Capital_U = $055;
   GDK_Capital_V = $056;
   GDK_Capital_W = $057;
   GDK_Capital_X = $058;
   GDK_Capital_Y = $059;
   GDK_Capital_Z = $05a;
   GDK_bracketleft = $05b;
   GDK_backslash = $05c;
   GDK_bracketright = $05d;
   GDK_asciicircum = $05e;
   GDK_underscore = $05f;
   GDK_grave = $060;
   GDK_quoteleft = $060;
   GDK_a = $061;
   GDK_b = $062;
   GDK_c = $063;
   GDK_d = $064;
   GDK_e = $065;
   GDK_f = $066;
   GDK_g = $067;
   GDK_h = $068;
   GDK_i = $069;
   GDK_j = $06a;
   GDK_k = $06b;
   GDK_l = $06c;
   GDK_m = $06d;
   GDK_n = $06e;
   GDK_o = $06f;
   GDK_p = $070;
   GDK_q = $071;
   GDK_r = $072;
   GDK_s = $073;
   GDK_t = $074;
   GDK_u = $075;
   GDK_v = $076;
   GDK_w = $077;
   GDK_x = $078;
   GDK_y = $079;
   GDK_z = $07a;
   GDK_braceleft = $07b;
   GDK_bar = $07c;
   GDK_braceright = $07d;
   GDK_asciitilde = $07e;
   GDK_nobreakspace = $0a0;
   GDK_exclamdown = $0a1;
   GDK_cent = $0a2;
   GDK_sterling = $0a3;
   GDK_currency = $0a4;
   GDK_yen = $0a5;
   GDK_brokenbar = $0a6;
   GDK_section = $0a7;
   GDK_diaeresis = $0a8;
   GDK_copyright = $0a9;
   GDK_ordfeminine = $0aa;
   GDK_guillemotleft = $0ab;
   GDK_notsign = $0ac;
   GDK_hyphen = $0ad;
   GDK_registered = $0ae;
   GDK_macron = $0af;
   GDK_degree = $0b0;
   GDK_plusminus = $0b1;
   GDK_twosuperior = $0b2;
   GDK_threesuperior = $0b3;
   GDK_acute = $0b4;
   GDK_mu = $0b5;
   GDK_paragraph = $0b6;
   GDK_periodcentered = $0b7;
   GDK_cedilla = $0b8;
   GDK_onesuperior = $0b9;
   GDK_masculine = $0ba;
   GDK_guillemotright = $0bb;
   GDK_onequarter = $0bc;
   GDK_onehalf = $0bd;
   GDK_threequarters = $0be;
   GDK_questiondown = $0bf;
   GDK_Capital_Agrave = $0c0;
   GDK_Capital_Aacute = $0c1;
   GDK_Capital_Acircumflex = $0c2;
   GDK_Capital_Atilde = $0c3;
   GDK_Capital_Adiaeresis = $0c4;
   GDK_Capital_Aring = $0c5;
   GDK_Capital_AE = $0c6;
   GDK_Capital_Ccedilla = $0c7;
   GDK_Capital_Egrave = $0c8;
   GDK_Capital_Eacute = $0c9;
   GDK_Capital_Ecircumflex = $0ca;
   GDK_Capital_Ediaeresis = $0cb;
   GDK_Capital_Igrave = $0cc;
   GDK_Capital_Iacute = $0cd;
   GDK_Capital_Icircumflex = $0ce;
   GDK_Capital_Idiaeresis = $0cf;
   GDK_Capital_ETH = $0d0;
   GDK_Capital_Ntilde = $0d1;
   GDK_Capital_Ograve = $0d2;
   GDK_Capital_Oacute = $0d3;
   GDK_Capital_Ocircumflex = $0d4;
   GDK_Capital_Otilde = $0d5;
   GDK_Capital_Odiaeresis = $0d6;
   GDK_Capital_multiply = $0d7;
   GDK_Capital_Ooblique = $0d8;
   GDK_Capital_Ugrave = $0d9;
   GDK_Capital_Uacute = $0da;
   GDK_Capital_Ucircumflex = $0db;
   GDK_Capital_Udiaeresis = $0dc;
   GDK_Capital_Yacute = $0dd;
   GDK_Capital_THORN = $0de;
   GDK_Thorn = $0de;
   GDK_ssharp = $0df;
   GDK_agrave = $0e0;
   GDK_aacute = $0e1;
   GDK_acircumflex = $0e2;
   GDK_atilde = $0e3;
   GDK_adiaeresis = $0e4;
   GDK_aring = $0e5;
   GDK_ae = $0e6;
   GDK_ccedilla = $0e7;
   GDK_egrave = $0e8;
   GDK_eacute = $0e9;
   GDK_ecircumflex = $0ea;
   GDK_ediaeresis = $0eb;
   GDK_igrave = $0ec;
   GDK_iacute = $0ed;
   GDK_icircumflex = $0ee;
   GDK_idiaeresis = $0ef;
   GDK_eth = $0f0;
   GDK_ntilde = $0f1;
   GDK_ograve = $0f2;
   GDK_oacute = $0f3;
   GDK_ocircumflex = $0f4;
   GDK_otilde = $0f5;
   GDK_odiaeresis = $0f6;
   GDK_division = $0f7;
   GDK_oslash = $0f8;
   GDK_ugrave = $0f9;
   GDK_uacute = $0fa;
   GDK_ucircumflex = $0fb;
   GDK_udiaeresis = $0fc;
   GDK_yacute = $0fd;
   GDK_small_thorn = $0fe;
   GDK_ydiaeresis = $0ff;
   GDK_Capital_Aogonek = $1a1;
   GDK_Capital_breve = $1a2;
   GDK_Capital_Lstroke = $1a3;
   GDK_Capital_Lcaron = $1a5;
   GDK_Capital_Sacute = $1a6;
   GDK_Capital_Scaron = $1a9;
   GDK_Capital_Scedilla = $1aa;
   GDK_Capital_Tcaron = $1ab;
   GDK_Capital_Zacute = $1ac;
   GDK_Capital_Zcaron = $1ae;
   GDK_Capital_Zabovedot = $1af;
   GDK_aogonek = $1b1;
   GDK_ogonek = $1b2;
   GDK_lstroke = $1b3;
   GDK_lcaron = $1b5;
   GDK_sacute = $1b6;
   GDK_caron = $1b7;
   GDK_scaron = $1b9;
   GDK_scedilla = $1ba;
   GDK_tcaron = $1bb;
   GDK_zacute = $1bc;
   GDK_doubleacute = $1bd;
   GDK_zcaron = $1be;
   GDK_zabovedot = $1bf;
   GDK_Capital_Racute = $1c0;
   GDK_Capital_Abreve = $1c3;
   GDK_Capital_Lacute = $1c5;
   GDK_Capital_Cacute = $1c6;
   GDK_Capital_Ccaron = $1c8;
   GDK_Capital_Eogonek = $1ca;
   GDK_Capital_Ecaron = $1cc;
   GDK_Capital_Dcaron = $1cf;
   GDK_Capital_Dstroke = $1d0;
   GDK_Capital_Nacute = $1d1;
   GDK_Capital_Ncaron = $1d2;
   GDK_Capital_Odoubleacute = $1d5;
   GDK_Capital_Rcaron = $1d8;
   GDK_Capital_Uring = $1d9;
   GDK_Capital_Udoubleacute = $1db;
   GDK_Capital_Tcedilla = $1de;
   GDK_racute = $1e0;
   GDK_abreve = $1e3;
   GDK_lacute = $1e5;
   GDK_cacute = $1e6;
   GDK_ccaron = $1e8;
   GDK_eogonek = $1ea;
   GDK_ecaron = $1ec;
   GDK_dcaron = $1ef;
   GDK_dstroke = $1f0;
   GDK_nacute = $1f1;
   GDK_ncaron = $1f2;
   GDK_odoubleacute = $1f5;
   GDK_udoubleacute = $1fb;
   GDK_rcaron = $1f8;
   GDK_uring = $1f9;
   GDK_tcedilla = $1fe;
   GDK_abovedot = $1ff;
   GDK_Capital_Hstroke = $2a1;
   GDK_Capital_Hcircumflex = $2a6;
   GDK_Capital_Iabovedot = $2a9;
   GDK_Capital_Gbreve = $2ab;
   GDK_Capital_Jcircumflex = $2ac;
   GDK_hstroke = $2b1;
   GDK_hcircumflex = $2b6;
   GDK_idotless = $2b9;
   GDK_gbreve = $2bb;
   GDK_jcircumflex = $2bc;
   GDK_Capital_Cabovedot = $2c5;
   GDK_Capital_Ccircumflex = $2c6;
   GDK_Capital_Gabovedot = $2d5;
   GDK_Capital_Gcircumflex = $2d8;
   GDK_Capital_Ubreve = $2dd;
   GDK_Capital_Scircumflex = $2de;
   GDK_cabovedot = $2e5;
   GDK_ccircumflex = $2e6;
   GDK_gabovedot = $2f5;
   GDK_gcircumflex = $2f8;
   GDK_ubreve = $2fd;
   GDK_scircumflex = $2fe;
   GDK_kra = $3a2;
   GDK_kappa = $3a2;
   GDK_Capital_Rcedilla = $3a3;
   GDK_Capital_Itilde = $3a5;
   GDK_Capital_Lcedilla = $3a6;
   GDK_Capital_Emacron = $3aa;
   GDK_Capital_Gcedilla = $3ab;
   GDK_Capital_Tslash = $3ac;
   GDK_rcedilla = $3b3;
   GDK_itilde = $3b5;
   GDK_lcedilla = $3b6;
   GDK_emacron = $3ba;
   GDK_gcedilla = $3bb;
   GDK_tslash = $3bc;
   GDK_Capital_ENG = $3bd;
   GDK_eng = $3bf;
   GDK_Capital_Amacron = $3c0;
   GDK_Capital_Iogonek = $3c7;
   GDK_Capital_Eabovedot = $3cc;
   GDK_Capital_Imacron = $3cf;
   GDK_Capital_Ncedilla = $3d1;
   GDK_Capital_Omacron = $3d2;
   GDK_Capital_Kcedilla = $3d3;
   GDK_Capital_Uogonek = $3d9;
   GDK_Capital_Utilde = $3dd;
   GDK_Capital_Umacron = $3de;
   GDK_amacron = $3e0;
   GDK_iogonek = $3e7;
   GDK_eabovedot = $3ec;
   GDK_imacron = $3ef;
   GDK_ncedilla = $3f1;
   GDK_omacron = $3f2;
   GDK_kcedilla = $3f3;
   GDK_uogonek = $3f9;
   GDK_utilde = $3fd;
   GDK_umacron = $3fe;
   GDK_overline = $47e;
   GDK_kana_fullstop = $4a1;
   GDK_kana_openingbracket = $4a2;
   GDK_kana_closingbracket = $4a3;
   GDK_kana_comma = $4a4;
   GDK_kana_conjunctive = $4a5;
   GDK_kana_middledot = $4a5;
   GDK_kana_WO = $4a6;
   GDK_kana_a = $4a7;
   GDK_kana_i = $4a8;
   GDK_kana_u = $4a9;
   GDK_kana_e = $4aa;
   GDK_kana_o = $4ab;
   GDK_kana_ya = $4ac;
   GDK_kana_yu = $4ad;
   GDK_kana_yo = $4ae;
   GDK_kana_tsu = $4af;
   GDK_kana_tu = $4af;
   GDK_prolongedsound = $4b0;
   GDK_kana_Capital__A = $4b1;
   GDK_kana_Capital__I = $4b2;
   GDK_kana_Capital__U = $4b3;
   GDK_kana_Capital__E = $4b4;
   GDK_kana_Capital__O = $4b5;
   GDK_kana_Capital__KA = $4b6;
   GDK_kana_Capital__KI = $4b7;
   GDK_kana_Capital__KU = $4b8;
   GDK_kana_Capital__KE = $4b9;
   GDK_kana_Capital__KO = $4ba;
   GDK_kana_Capital__SA = $4bb;
   GDK_kana_Capital__SHI = $4bc;
   GDK_kana_Capital__SU = $4bd;
   GDK_kana_Capital__SE = $4be;
   GDK_kana_Capital__SO = $4bf;
   GDK_kana_Capital__TA = $4c0;
   GDK_kana_Capital__CHI = $4c1;
   GDK_kana_Capital__TI = $4c1;
   GDK_kana_Capital__TSU = $4c2;
   GDK_kana_Capital__TU = $4c2;
   GDK_kana_Capital__TE = $4c3;
   GDK_kana_Capital__TO = $4c4;
   GDK_kana_Capital__NA = $4c5;
   GDK_kana_Capital__NI = $4c6;
   GDK_kana_Capital__NU = $4c7;
   GDK_kana_Capital__NE = $4c8;
   GDK_kana_Capital__NO = $4c9;
   GDK_kana_Capital__HA = $4ca;
   GDK_kana_Capital__HI = $4cb;
   GDK_kana_Capital__FU = $4cc;
   GDK_kana_Capital__HU = $4cc;
   GDK_kana_Capital__HE = $4cd;
   GDK_kana_Capital__HO = $4ce;
   GDK_kana_Capital__MA = $4cf;
   GDK_kana_Capital__MI = $4d0;
   GDK_kana_Capital__MU = $4d1;
   GDK_kana_Capital__ME = $4d2;
   GDK_kana_Capital__MO = $4d3;
   GDK_kana_Capital__YA = $4d4;
   GDK_kana_Capital__YU = $4d5;
   GDK_kana_Capital__YO = $4d6;
   GDK_kana_Capital__RA = $4d7;
   GDK_kana_Capital__RI = $4d8;
   GDK_kana_Capital__RU = $4d9;
   GDK_kana_Capital__RE = $4da;
   GDK_kana_Capital__RO = $4db;
   GDK_kana_Capital__WA = $4dc;
   GDK_kana_Capital__N = $4dd;
   GDK_voicedsound = $4de;
   GDK_semivoicedsound = $4df;
   GDK_kana_switch = $FF7E;
   GDK_arabic_comma = $5ac;
   GDK_arabic_semicolon = $5bb;
   GDK_arabic_question_mark = $5bf;
   GDK_arabic_hamza = $5c1;
   GDK_arabic_maddaonalef = $5c2;
   GDK_arabic_hamzaonalef = $5c3;
   GDK_arabic_hamzaonwaw = $5c4;
   GDK_arabic_hamzaunderalef = $5c5;
   GDK_arabic_hamzaonyeh = $5c6;
   GDK_arabic_alef = $5c7;
   GDK_arabic_beh = $5c8;
   GDK_arabic_tehmarbuta = $5c9;
   GDK_arabic_teh = $5ca;
   GDK_arabic_theh = $5cb;
   GDK_arabic_jeem = $5cc;
   GDK_arabic_hah = $5cd;
   GDK_arabic_khah = $5ce;
   GDK_arabic_dal = $5cf;
   GDK_arabic_thal = $5d0;
   GDK_arabic_ra = $5d1;
   GDK_arabic_zain = $5d2;
   GDK_arabic_seen = $5d3;
   GDK_arabic_sheen = $5d4;
   GDK_arabic_sad = $5d5;
   GDK_arabic_dad = $5d6;
   GDK_arabic_tah = $5d7;
   GDK_arabic_zah = $5d8;
   GDK_arabic_ain = $5d9;
   GDK_arabic_ghain = $5da;
   GDK_arabic_tatweel = $5e0;
   GDK_arabic_feh = $5e1;
   GDK_arabic_qaf = $5e2;
   GDK_arabic_kaf = $5e3;
   GDK_arabic_lam = $5e4;
   GDK_arabic_meem = $5e5;
   GDK_arabic_noon = $5e6;
   GDK_arabic_ha = $5e7;
   GDK_arabic_heh = $5e7;
   GDK_arabic_waw = $5e8;
   GDK_arabic_alefmaksura = $5e9;
   GDK_arabic_yeh = $5ea;
   GDK_arabic_fathatan = $5eb;
   GDK_arabic_dammatan = $5ec;
   GDK_arabic_kasratan = $5ed;
   GDK_arabic_fatha = $5ee;
   GDK_arabic_damma = $5ef;
   GDK_arabic_kasra = $5f0;
   GDK_arabic_shadda = $5f1;
   GDK_arabic_sukun = $5f2;
   GDK_arabic_switch = $FF7E;
   GDK_serbian_dje = $6a1;
   GDK_macedonia_gje = $6a2;
   GDK_cyrillic_io = $6a3;
   GDK_ukrainian_ie = $6a4;
   GDK_ukrainian_je = $6a4;
   GDK_macedonia_dse = $6a5;
   GDK_ukrainian_i = $6a6;
   GDK_ukrainian_yi = $6a7;
   GDK_cyrillic_je = $6a8;
   GDK_serbian_je = $6a8;
   GDK_cyrillic_lje = $6a9;
   GDK_serbian_lje = $6a9;
   GDK_cyrillic_nje = $6aa;
   GDK_serbian_nje = $6aa;
   GDK_serbian_tshe = $6ab;
   GDK_macedonia_kje = $6ac;
   GDK_byelorussian_shortu = $6ae;
   GDK_cyrillic_dzhe = $6af;
   GDK_serbian_dze = $6af;
   GDK_numerosign = $6b0;
   GDK_serbian_Capital_DJE = $6b1;
   GDK_macedonia_Capital_GJE = $6b2;
   GDK_cyrillic_Capital_IO = $6b3;
   GDK_ukrainian_Capital_IE = $6b4;
   GDK_ukrainian_Capital_JE = $6b4;
   GDK_macedonia_Capital_DSE = $6b5;
   GDK_ukrainian_Capital_I = $6b6;
   GDK_ukrainian_Capital_YI = $6b7;
   GDK_cyrillic_Capital_JE = $6b8;
   GDK_serbian_Capital_JE = $6b8;
   GDK_cyrillic_Capital_LJE = $6b9;
   GDK_serbian_Capital_LJE = $6b9;
   GDK_cyrillic_Capital_NJE = $6ba;
   GDK_serbian_Capital_NJE = $6ba;
   GDK_serbian_Capital_TSHE = $6bb;
   GDK_macedonia_Capital_KJE = $6bc;
   GDK_byelorussian_Capital_SHORTU = $6be;
   GDK_cyrillic_Capital_DZHE = $6bf;
   GDK_serbian_Capital_DZE = $6bf;
   GDK_cyrillic_yu = $6c0;
   GDK_cyrillic_a = $6c1;
   GDK_cyrillic_be = $6c2;
   GDK_cyrillic_tse = $6c3;
   GDK_cyrillic_de = $6c4;
   GDK_cyrillic_ie = $6c5;
   GDK_cyrillic_ef = $6c6;
   GDK_cyrillic_ghe = $6c7;
   GDK_cyrillic_ha = $6c8;
   GDK_cyrillic_i = $6c9;
   GDK_cyrillic_shorti = $6ca;
   GDK_cyrillic_ka = $6cb;
   GDK_cyrillic_el = $6cc;
   GDK_cyrillic_em = $6cd;
   GDK_cyrillic_en = $6ce;
   GDK_cyrillic_o = $6cf;
   GDK_cyrillic_pe = $6d0;
   GDK_cyrillic_ya = $6d1;
   GDK_cyrillic_er = $6d2;
   GDK_cyrillic_es = $6d3;
   GDK_cyrillic_te = $6d4;
   GDK_cyrillic_u = $6d5;
   GDK_cyrillic_zhe = $6d6;
   GDK_cyrillic_ve = $6d7;
   GDK_cyrillic_softsign = $6d8;
   GDK_cyrillic_yeru = $6d9;
   GDK_cyrillic_ze = $6da;
   GDK_cyrillic_sha = $6db;
   GDK_cyrillic_e = $6dc;
   GDK_cyrillic_shcha = $6dd;
   GDK_cyrillic_che = $6de;
   GDK_cyrillic_hardsign = $6df;
   GDK_cyrillic_Capital_YU = $6e0;
   GDK_cyrillic_Capital_A = $6e1;
   GDK_cyrillic_Capital_BE = $6e2;
   GDK_cyrillic_Capital_TSE = $6e3;
   GDK_cyrillic_Capital_DE = $6e4;
   GDK_cyrillic_Capital_IE = $6e5;
   GDK_cyrillic_Capital_EF = $6e6;
   GDK_cyrillic_Capital_GHE = $6e7;
   GDK_cyrillic_Capital_HA = $6e8;
   GDK_cyrillic_Capital_I = $6e9;
   GDK_cyrillic_Capital_SHORTI = $6ea;
   GDK_cyrillic_Capital_KA = $6eb;
   GDK_cyrillic_Capital_EL = $6ec;
   GDK_cyrillic_Capital_EM = $6ed;
   GDK_cyrillic_Capital_EN = $6ee;
   GDK_cyrillic_Capital_O = $6ef;
   GDK_cyrillic_Capital_PE = $6f0;
   GDK_cyrillic_Capital_YA = $6f1;
   GDK_cyrillic_Capital_ER = $6f2;
   GDK_cyrillic_Capital_ES = $6f3;
   GDK_cyrillic_Capital_TE = $6f4;
   GDK_cyrillic_Capital_U = $6f5;
   GDK_cyrillic_Capital_ZHE = $6f6;
   GDK_cyrillic_Capital_VE = $6f7;
   GDK_cyrillic_Capital_SOFTSIGN = $6f8;
   GDK_cyrillic_Capital_YERU = $6f9;
   GDK_cyrillic_Capital_ZE = $6fa;
   GDK_cyrillic_Capital_SHA = $6fb;
   GDK_cyrillic_Capital_E = $6fc;
   GDK_cyrillic_Capital_SHCHA = $6fd;
   GDK_cyrillic_Capital_CHE = $6fe;
   GDK_cyrillic_Capital_HARDSIGN = $6ff;
   GDK_greek_Capital_ALPHAaccent = $7a1;
   GDK_greek_Capital_EPSILONaccent = $7a2;
   GDK_greek_Capital_ETAaccent = $7a3;
   GDK_greek_Capital_IOTAaccent = $7a4;
   GDK_greek_Capital_IOTAdiaeresis = $7a5;
   GDK_greek_Capital_OMICRONaccent = $7a7;
   GDK_greek_Capital_UPSILONaccent = $7a8;
   GDK_greek_Capital_UPSILONdieresis = $7a9;
   GDK_greek_Capital_OMEGAaccent = $7ab;
   GDK_greek_accentdieresis = $7ae;
   GDK_greek_horizbar = $7af;
   GDK_greek_alphaaccent = $7b1;
   GDK_greek_epsilonaccent = $7b2;
   GDK_greek_etaaccent = $7b3;
   GDK_greek_iotaaccent = $7b4;
   GDK_greek_iotadieresis = $7b5;
   GDK_greek_iotaaccentdieresis = $7b6;
   GDK_greek_omicronaccent = $7b7;
   GDK_greek_upsilonaccent = $7b8;
   GDK_greek_upsilondieresis = $7b9;
   GDK_greek_upsilonaccentdieresis = $7ba;
   GDK_greek_omegaaccent = $7bb;
   GDK_greek_Capital_ALPHA = $7c1;
   GDK_greek_Capital_BETA = $7c2;
   GDK_greek_Capital_GAMMA = $7c3;
   GDK_greek_Capital_DELTA = $7c4;
   GDK_greek_Capital_EPSILON = $7c5;
   GDK_greek_Capital_ZETA = $7c6;
   GDK_greek_Capital_ETA = $7c7;
   GDK_greek_Capital_THETA = $7c8;
   GDK_greek_Capital_IOTA = $7c9;
   GDK_greek_Capital_KAPPA = $7ca;
   GDK_greek_Capital_LAMDA = $7cb;
   GDK_greek_Capital_LAMBDA = $7cb;
   GDK_greek_Capital_MU = $7cc;
   GDK_greek_Capital_NU = $7cd;
   GDK_greek_Capital_XI = $7ce;
   GDK_greek_Capital_OMICRON = $7cf;
   GDK_greek_Capital_PI = $7d0;
   GDK_greek_Capital_RHO = $7d1;
   GDK_greek_Capital_SIGMA = $7d2;
   GDK_greek_Capital_TAU = $7d4;
   GDK_greek_Capital_UPSILON = $7d5;
   GDK_greek_Capital_PHI = $7d6;
   GDK_greek_Capital_CHI = $7d7;
   GDK_greek_Capital_PSI = $7d8;
   GDK_greek_Capital_OMEGA = $7d9;
   GDK_greek_alpha = $7e1;
   GDK_greek_beta = $7e2;
   GDK_greek_gamma = $7e3;
   GDK_greek_delta = $7e4;
   GDK_greek_epsilon = $7e5;
   GDK_greek_zeta = $7e6;
   GDK_greek_eta = $7e7;
   GDK_greek_theta = $7e8;
   GDK_greek_iota = $7e9;
   GDK_greek_kappa = $7ea;
   GDK_greek_lamda = $7eb;
   GDK_greek_lambda = $7eb;
   GDK_greek_mu = $7ec;
   GDK_greek_nu = $7ed;
   GDK_greek_xi = $7ee;
   GDK_greek_omicron = $7ef;
   GDK_greek_pi = $7f0;
   GDK_greek_rho = $7f1;
   GDK_greek_sigma = $7f2;
   GDK_greek_finalsmallsigma = $7f3;
   GDK_greek_tau = $7f4;
   GDK_greek_upsilon = $7f5;
   GDK_greek_phi = $7f6;
   GDK_greek_chi = $7f7;
   GDK_greek_psi = $7f8;
   GDK_greek_omega = $7f9;
   GDK_greek_switch = $FF7E;
   GDK_leftradical = $8a1;
   GDK_topleftradical = $8a2;
   GDK_horizconnector = $8a3;
   GDK_topintegral = $8a4;
   GDK_botintegral = $8a5;
   GDK_vertconnector = $8a6;
   GDK_topleftsqbracket = $8a7;
   GDK_botleftsqbracket = $8a8;
   GDK_toprightsqbracket = $8a9;
   GDK_botrightsqbracket = $8aa;
   GDK_topleftparens = $8ab;
   GDK_botleftparens = $8ac;
   GDK_toprightparens = $8ad;
   GDK_botrightparens = $8ae;
   GDK_leftmiddlecurlybrace = $8af;
   GDK_rightmiddlecurlybrace = $8b0;
   GDK_topleftsummation = $8b1;
   GDK_botleftsummation = $8b2;
   GDK_topvertsummationconnector = $8b3;
   GDK_botvertsummationconnector = $8b4;
   GDK_toprightsummation = $8b5;
   GDK_botrightsummation = $8b6;
   GDK_rightmiddlesummation = $8b7;
   GDK_lessthanequal = $8bc;
   GDK_notequal = $8bd;
   GDK_greaterthanequal = $8be;
   GDK_integral = $8bf;
   GDK_therefore = $8c0;
   GDK_variation = $8c1;
   GDK_infinity = $8c2;
   GDK_nabla = $8c5;
   GDK_approximate = $8c8;
   GDK_similarequal = $8c9;
   GDK_ifonlyif = $8cd;
   GDK_implies = $8ce;
   GDK_identical = $8cf;
   GDK_radical = $8d6;
   GDK_includedin = $8da;
   GDK_includes = $8db;
   GDK_intersection = $8dc;
   GDK_union = $8dd;
   GDK_logicaland = $8de;
   GDK_logicalor = $8df;
   GDK_partialderivative = $8ef;
   GDK_function = $8f6;
   GDK_leftarrow = $8fb;
   GDK_uparrow = $8fc;
   GDK_rightarrow = $8fd;
   GDK_downarrow = $8fe;
   GDK_blank = $9df;
   GDK_soliddiamond = $9e0;
   GDK_checkerboard = $9e1;
   GDK_ht = $9e2;
   GDK_ff = $9e3;
   GDK_cr = $9e4;
   GDK_lf = $9e5;
   GDK_nl = $9e8;
   GDK_vt = $9e9;
   GDK_lowrightcorner = $9ea;
   GDK_uprightcorner = $9eb;
   GDK_upleftcorner = $9ec;
   GDK_lowleftcorner = $9ed;
   GDK_crossinglines = $9ee;
   GDK_horizlinescan1 = $9ef;
   GDK_horizlinescan3 = $9f0;
   GDK_horizlinescan5 = $9f1;
   GDK_horizlinescan7 = $9f2;
   GDK_horizlinescan9 = $9f3;
   GDK_leftt = $9f4;
   GDK_rightt = $9f5;
   GDK_bott = $9f6;
   GDK_topt = $9f7;
   GDK_vertbar = $9f8;
   GDK_emspace = $aa1;
   GDK_enspace = $aa2;
   GDK_em3space = $aa3;
   GDK_em4space = $aa4;
   GDK_digitspace = $aa5;
   GDK_punctspace = $aa6;
   GDK_thinspace = $aa7;
   GDK_hairspace = $aa8;
   GDK_emdash = $aa9;
   GDK_endash = $aaa;
   GDK_signifblank = $aac;
   GDK_ellipsis = $aae;
   GDK_doubbaselinedot = $aaf;
   GDK_onethird = $ab0;
   GDK_twothirds = $ab1;
   GDK_onefifth = $ab2;
   GDK_twofifths = $ab3;
   GDK_threefifths = $ab4;
   GDK_fourfifths = $ab5;
   GDK_onesixth = $ab6;
   GDK_fivesixths = $ab7;
   GDK_careof = $ab8;
   GDK_figdash = $abb;
   GDK_leftanglebracket = $abc;
   GDK_decimalpoint = $abd;
   GDK_rightanglebracket = $abe;
   GDK_marker = $abf;
   GDK_oneeighth = $ac3;
   GDK_threeeighths = $ac4;
   GDK_fiveeighths = $ac5;
   GDK_seveneighths = $ac6;
   GDK_trademark = $ac9;
   GDK_signaturemark = $aca;
   GDK_trademarkincircle = $acb;
   GDK_leftopentriangle = $acc;
   GDK_rightopentriangle = $acd;
   GDK_emopencircle = $ace;
   GDK_emopenrectangle = $acf;
   GDK_leftsinglequotemark = $ad0;
   GDK_rightsinglequotemark = $ad1;
   GDK_leftdoublequotemark = $ad2;
   GDK_rightdoublequotemark = $ad3;
   GDK_prescription = $ad4;
   GDK_minutes = $ad6;
   GDK_seconds = $ad7;
   GDK_latincross = $ad9;
   GDK_hexagram = $ada;
   GDK_filledrectbullet = $adb;
   GDK_filledlefttribullet = $adc;
   GDK_filledrighttribullet = $add;
   GDK_emfilledcircle = $ade;
   GDK_emfilledrect = $adf;
   GDK_enopencircbullet = $ae0;
   GDK_enopensquarebullet = $ae1;
   GDK_openrectbullet = $ae2;
   GDK_opentribulletup = $ae3;
   GDK_opentribulletdown = $ae4;
   GDK_openstar = $ae5;
   GDK_enfilledcircbullet = $ae6;
   GDK_enfilledsqbullet = $ae7;
   GDK_filledtribulletup = $ae8;
   GDK_filledtribulletdown = $ae9;
   GDK_leftpointer = $aea;
   GDK_rightpointer = $aeb;
   GDK_club = $aec;
   GDK_diamond = $aed;
   GDK_heart_key = $aee;
   GDK_maltesecross = $af0;
   GDK_dagger = $af1;
   GDK_doubledagger = $af2;
   GDK_checkmark = $af3;
   GDK_ballotcross = $af4;
   GDK_musicalsharp = $af5;
   GDK_musicalflat = $af6;
   GDK_malesymbol = $af7;
   GDK_femalesymbol = $af8;
   GDK_telephone = $af9;
   GDK_telephonerecorder = $afa;
   GDK_phonographcopyright = $afb;
   GDK_caret = $afc;
   GDK_singlelowquotemark = $afd;
   GDK_doublelowquotemark = $afe;
   GDK_cursor = $aff;
   GDK_leftcaret = $ba3;
   GDK_rightcaret = $ba6;
   GDK_downcaret = $ba8;
   GDK_upcaret = $ba9;
   GDK_overbar = $bc0;
   GDK_downtack = $bc2;
   GDK_upshoe = $bc3;
   GDK_downstile = $bc4;
   GDK_underbar = $bc6;
   GDK_jot = $bca;
   GDK_quad = $bcc;
   GDK_uptack = $bce;
   GDK_circle_key = $bcf;
   GDK_upstile = $bd3;
   GDK_downshoe = $bd6;
   GDK_rightshoe = $bd8;
   GDK_leftshoe = $bda;
   GDK_lefttack = $bdc;
   GDK_righttack = $bfc;
   GDK_hebrew_doublelowline = $cdf;
   GDK_hebrew_aleph = $ce0;
   GDK_hebrew_bet = $ce1;
   GDK_hebrew_beth = $ce1;
   GDK_hebrew_gimel = $ce2;
   GDK_hebrew_gimmel = $ce2;
   GDK_hebrew_dalet = $ce3;
   GDK_hebrew_daleth = $ce3;
   GDK_hebrew_he = $ce4;
   GDK_hebrew_waw = $ce5;
   GDK_hebrew_zain = $ce6;
   GDK_hebrew_zayin = $ce6;
   GDK_hebrew_chet = $ce7;
   GDK_hebrew_het = $ce7;
   GDK_hebrew_tet = $ce8;
   GDK_hebrew_teth = $ce8;
   GDK_hebrew_yod = $ce9;
   GDK_hebrew_finalkaph = $cea;
   GDK_hebrew_kaph = $ceb;
   GDK_hebrew_lamed = $cec;
   GDK_hebrew_finalmem = $ced;
   GDK_hebrew_mem = $cee;
   GDK_hebrew_finalnun = $cef;
   GDK_hebrew_nun = $cf0;
   GDK_hebrew_samech = $cf1;
   GDK_hebrew_samekh = $cf1;
   GDK_hebrew_ayin = $cf2;
   GDK_hebrew_finalpe = $cf3;
   GDK_hebrew_pe = $cf4;
   GDK_hebrew_finalzade = $cf5;
   GDK_hebrew_finalzadi = $cf5;
   GDK_hebrew_zade = $cf6;
   GDK_hebrew_zadi = $cf6;
   GDK_hebrew_qoph = $cf7;
   GDK_hebrew_kuf = $cf7;
   GDK_hebrew_resh = $cf8;
   GDK_hebrew_shin = $cf9;
   GDK_hebrew_taw = $cfa;
   GDK_hebrew_taf = $cfa;
   GDK_Hebrew_switch = $FF7E;
   GDK_Thai_kokai = $da1;
   GDK_Thai_khokhai = $da2;
   GDK_Thai_khokhuat = $da3;
   GDK_Thai_khokhwai = $da4;
   GDK_Thai_khokhon = $da5;
   GDK_Thai_khorakhang = $da6;
   GDK_Thai_ngongu = $da7;
   GDK_Thai_chochan = $da8;
   GDK_Thai_choching = $da9;
   GDK_Thai_chochang = $daa;
   GDK_Thai_soso = $dab;
   GDK_Thai_chochoe = $dac;
   GDK_Thai_yoying = $dad;
   GDK_Thai_dochada = $dae;
   GDK_Thai_topatak = $daf;
   GDK_Thai_thothan = $db0;
   GDK_Thai_thonangmontho = $db1;
   GDK_Thai_thophuthao = $db2;
   GDK_Thai_nonen = $db3;
   GDK_Thai_dodek = $db4;
   GDK_Thai_totao = $db5;
   GDK_Thai_thothung = $db6;
   GDK_Thai_thothahan = $db7;
   GDK_Thai_thothong = $db8;
   GDK_Thai_nonu = $db9;
   GDK_Thai_bobaimai = $dba;
   GDK_Thai_popla = $dbb;
   GDK_Thai_phophung = $dbc;
   GDK_Thai_fofa = $dbd;
   GDK_Thai_phophan = $dbe;
   GDK_Thai_fofan = $dbf;
   GDK_Thai_phosamphao = $dc0;
   GDK_Thai_moma = $dc1;
   GDK_Thai_yoyak = $dc2;
   GDK_Thai_rorua = $dc3;
   GDK_Thai_ru = $dc4;
   GDK_Thai_loling = $dc5;
   GDK_Thai_lu = $dc6;
   GDK_Thai_wowaen = $dc7;
   GDK_Thai_sosala = $dc8;
   GDK_Thai_sorusi = $dc9;
   GDK_Thai_sosua = $dca;
   GDK_Thai_hohip = $dcb;
   GDK_Thai_lochula = $dcc;
   GDK_Thai_oang = $dcd;
   GDK_Thai_honokhuk = $dce;
   GDK_Thai_paiyannoi = $dcf;
   GDK_Thai_saraa = $dd0;
   GDK_Thai_maihanakat = $dd1;
   GDK_Thai_saraaa = $dd2;
   GDK_Thai_saraam = $dd3;
   GDK_Thai_sarai = $dd4;
   GDK_Thai_saraii = $dd5;
   GDK_Thai_saraue = $dd6;
   GDK_Thai_sarauee = $dd7;
   GDK_Thai_sarau = $dd8;
   GDK_Thai_sarauu = $dd9;
   GDK_Thai_phinthu = $dda;
   GDK_Thai_maihanakat_maitho = $dde;
   GDK_Thai_baht = $ddf;
   GDK_Thai_sarae = $de0;
   GDK_Thai_saraae = $de1;
   GDK_Thai_sarao = $de2;
   GDK_Thai_saraaimaimuan = $de3;
   GDK_Thai_saraaimaimalai = $de4;
   GDK_Thai_lakkhangyao = $de5;
   GDK_Thai_maiyamok = $de6;
   GDK_Thai_maitaikhu = $de7;
   GDK_Thai_maiek = $de8;
   GDK_Thai_maitho = $de9;
   GDK_Thai_maitri = $dea;
   GDK_Thai_maichattawa = $deb;
   GDK_Thai_thanthakhat = $dec;
   GDK_Thai_nikhahit = $ded;
   GDK_Thai_leksun = $df0;
   GDK_Thai_leknung = $df1;
   GDK_Thai_leksong = $df2;
   GDK_Thai_leksam = $df3;
   GDK_Thai_leksi = $df4;
   GDK_Thai_lekha = $df5;
   GDK_Thai_lekhok = $df6;
   GDK_Thai_lekchet = $df7;
   GDK_Thai_lekpaet = $df8;
   GDK_Thai_lekkao = $df9;
   GDK_Hangul = $ff31;
   GDK_Hangul_Start = $ff32;
   GDK_Hangul_End = $ff33;
   GDK_Hangul_Hanja = $ff34;
   GDK_Hangul_Jamo = $ff35;
   GDK_Hangul_Romaja = $ff36;
   GDK_Hangul_Codeinput = $ff37;
   GDK_Hangul_Jeonja = $ff38;
   GDK_Hangul_Banja = $ff39;
   GDK_Hangul_PreHanja = $ff3a;
   GDK_Hangul_PostHanja = $ff3b;
   GDK_Hangul_SingleCandidate = $ff3c;
   GDK_Hangul_MultipleCandidate = $ff3d;
   GDK_Hangul_PreviousCandidate = $ff3e;
   GDK_Hangul_Special = $ff3f;
   GDK_Hangul_switch = $FF7E;
   GDK_Hangul_Kiyeog = $ea1;
   GDK_Hangul_SsangKiyeog = $ea2;
   GDK_Hangul_KiyeogSios = $ea3;
   GDK_Hangul_Nieun = $ea4;
   GDK_Hangul_NieunJieuj = $ea5;
   GDK_Hangul_NieunHieuh = $ea6;
   GDK_Hangul_Dikeud = $ea7;
   GDK_Hangul_SsangDikeud = $ea8;
   GDK_Hangul_Rieul = $ea9;
   GDK_Hangul_RieulKiyeog = $eaa;
   GDK_Hangul_RieulMieum = $eab;
   GDK_Hangul_RieulPieub = $eac;
   GDK_Hangul_RieulSios = $ead;
   GDK_Hangul_RieulTieut = $eae;
   GDK_Hangul_RieulPhieuf = $eaf;
   GDK_Hangul_RieulHieuh = $eb0;
   GDK_Hangul_Mieum = $eb1;
   GDK_Hangul_Pieub = $eb2;
   GDK_Hangul_SsangPieub = $eb3;
   GDK_Hangul_PieubSios = $eb4;
   GDK_Hangul_Sios = $eb5;
   GDK_Hangul_SsangSios = $eb6;
   GDK_Hangul_Ieung = $eb7;
   GDK_Hangul_Jieuj = $eb8;
   GDK_Hangul_SsangJieuj = $eb9;
   GDK_Hangul_Cieuc = $eba;
   GDK_Hangul_Khieuq = $ebb;
   GDK_Hangul_Tieut = $ebc;
   GDK_Hangul_Phieuf = $ebd;
   GDK_Hangul_Hieuh = $ebe;
   GDK_Hangul_A = $ebf;
   GDK_Hangul_AE = $ec0;
   GDK_Hangul_YA = $ec1;
   GDK_Hangul_YAE = $ec2;
   GDK_Hangul_EO = $ec3;
   GDK_Hangul_E = $ec4;
   GDK_Hangul_YEO = $ec5;
   GDK_Hangul_YE = $ec6;
   GDK_Hangul_O = $ec7;
   GDK_Hangul_WA = $ec8;
   GDK_Hangul_WAE = $ec9;
   GDK_Hangul_OE = $eca;
   GDK_Hangul_YO = $ecb;
   GDK_Hangul_U = $ecc;
   GDK_Hangul_WEO = $ecd;
   GDK_Hangul_WE = $ece;
   GDK_Hangul_WI = $ecf;
   GDK_Hangul_YU = $ed0;
   GDK_Hangul_EU = $ed1;
   GDK_Hangul_YI = $ed2;
   GDK_Hangul_I = $ed3;
   GDK_Hangul_J_Kiyeog = $ed4;
   GDK_Hangul_J_SsangKiyeog = $ed5;
   GDK_Hangul_J_KiyeogSios = $ed6;
   GDK_Hangul_J_Nieun = $ed7;
   GDK_Hangul_J_NieunJieuj = $ed8;
   GDK_Hangul_J_NieunHieuh = $ed9;
   GDK_Hangul_J_Dikeud = $eda;
   GDK_Hangul_J_Rieul = $edb;
   GDK_Hangul_J_RieulKiyeog = $edc;
   GDK_Hangul_J_RieulMieum = $edd;
   GDK_Hangul_J_RieulPieub = $ede;
   GDK_Hangul_J_RieulSios = $edf;
   GDK_Hangul_J_RieulTieut = $ee0;
   GDK_Hangul_J_RieulPhieuf = $ee1;
   GDK_Hangul_J_RieulHieuh = $ee2;
   GDK_Hangul_J_Mieum = $ee3;
   GDK_Hangul_J_Pieub = $ee4;
   GDK_Hangul_J_PieubSios = $ee5;
   GDK_Hangul_J_Sios = $ee6;
   GDK_Hangul_J_SsangSios = $ee7;
   GDK_Hangul_J_Ieung = $ee8;
   GDK_Hangul_J_Jieuj = $ee9;
   GDK_Hangul_J_Cieuc = $eea;
   GDK_Hangul_J_Khieuq = $eeb;
   GDK_Hangul_J_Tieut = $eec;
   GDK_Hangul_J_Phieuf = $eed;
   GDK_Hangul_J_Hieuh = $eee;
   GDK_Hangul_RieulYeorinHieuh = $eef;
   GDK_Hangul_SunkyeongeumMieum = $ef0;
   GDK_Hangul_SunkyeongeumPieub = $ef1;
   GDK_Hangul_PanSios = $ef2;
   GDK_Hangul_KkogjiDalrinIeung = $ef3;
   GDK_Hangul_SunkyeongeumPhieuf = $ef4;
   GDK_Hangul_YeorinHieuh = $ef5;
   GDK_Hangul_AraeA = $ef6;
   GDK_Hangul_AraeAE = $ef7;
   GDK_Hangul_J_PanSios = $ef8;
   GDK_Hangul_J_KkogjiDalrinIeung = $ef9;
   GDK_Hangul_J_YeorinHieuh = $efa;
   GDK_Korean_Won = $eff;

{$endif read_interface}

{
  $Log: gdkkeysyms.pp,v $
  Revision 1.3  2005/02/14 17:13:20  peter
    * truncate log

}
