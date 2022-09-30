module FramaC.PrecisaPrelude where

import PPExt

precisaPreludeContent :: Doc
precisaPreludeContent =
  text "#include<stdbool.h>\n"
  $$
  text "#define round(X) (double) (X)\n"
  $$
  text "/*@"
  $$
  -- text"axiomatic fp_funs {"
  -- $$
  text "logic double Dadd(double X, double Y) = round(X+Y);\n"
  $$
  text "logic double Dsub(double X, double Y) = round(X-Y);\n"
  $$
  text "logic double Dmul(double X, double Y) = round(X*Y);\n"
  $$
  text "logic double Dneg(double X) = round(0-X);\n"
  $$
  text "logic double Dabs(double X) = round(X);\n"
  $$
  text "logic double Ddiv(double X, double Y) = (Y != 0.0 ? round(X/Y) : 0.0);\n"
--   X;
-- //  (Y != 0.0 ? round(X/Y) : 0.0) ;

-- // }

-- // axiomatic int_funs {
  $$
  text "logic integer Iadd(integer X, integer Y) = X+Y;\n"
  $$
  text "logic integer Isub(integer X, integer Y) = X-Y;\n"
  $$
  text "logic integer Imul(integer X, integer Y) = X*Y;\n"
  $$
  text "logic integer Ineg(integer X) = 0-X;\n"
--   // logic integer Idiv(integer X, integer Y) = (Y != 0 ? X/Y : 0) ;
-- // }
-- // axiomatic error_bounds {
  $$
  text "logic real ulp_dp(real X) = \\round_double(\\Up,X)-\\round_double(\\Down,X);\n"
  $$
  text "logic real errAdd_dp(real X, real E_X, real Y, real E_Y)"
  $$
  text "= E_X + E_Y + ulp_dp(\\abs(X + Y) + E_X + E_Y)/2;\n"
  $$
  text "logic real errSub_dp(real X, real E_X, real Y, real E_Y)"
  $$
  text "= E_X + E_Y + ulp_dp(\\abs(X - Y) + E_X + E_Y)/2;\n"
  $$
  text "logic real errMul_dp(real X, real E_X, real Y, real E_Y)"
  $$
  text "= \\abs(X)*E_Y+\\abs(Y)*E_X+E_X*E_Y + ulp_dp(\\abs(X)*\\abs(Y) + \\abs(X)*E_Y + E_X*\\abs(Y) + E_X*E_Y)/2;\n"
  $$
  text "logic real errDiv_dp(real X, real E_X, real Y, real E_Y)"
  $$
  text "= ( ((Y*Y - E_Y*\\abs(Y)) != 0 && (\\abs(Y) - E_Y) !=0)? (\\abs(Y)*E_X + \\abs(X)*E_Y) / (Y*Y - E_Y*\\abs(Y)) + ulp_dp((\\abs(X) + E_X) / (\\abs(Y) - E_Y)) / 2 : 0 );\n"
  $$
  text "logic real errNeg_dp(real X, real E_X) = E_X;\n"
  $$
  text "logic real errAdd_i(integer X, real E_X, integer Y, real E_Y) = E_X + E_Y;\n"
  $$
  text "logic real errSub_i(integer X, real E_X, integer Y, real E_Y) = E_X + E_Y;"
  $$
  text "}"
  $$
  text "*/\n"
  $$
  text "struct maybeInt {"
  $$
  nest 2 (text "bool isValid;")
  $$
  nest 2 (text "int value;")
  $$
  text "};\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures ! \\result.isValid;"
  $$
  text "*/"
  $$
  text "struct maybeInt none () {"
  $$
  nest 2 (text "struct maybeInt result = { false, 0 };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures \\result.isValid;"
  $$
  text "ensures \\result.value == val;"
  $$
  text "*/"
  $$
  text "struct maybeInt some (int val) {"
  $$
  nest 2 (text "struct maybeInt result = { true, val };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "struct maybeFloat {"
  $$
  nest 2 (text "bool isValid;")
  $$
  nest 2 (text "float value;")
  $$
  text "};\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures ! \\result.isValid;"
  $$
  text "*/"
  $$
  text "struct maybeFloat noneFloat () {"
  $$
  nest 2 (text "struct maybeFloat result = { false, 0 };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures \\result.isValid;"
  $$
  text "ensures \\result.value == val;"
  $$
  text "*/"
  $$
  text "struct maybeFloat someFloat (float val) {"
  $$
  nest 2 (text "struct maybeFloat result = { true, val };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "struct maybeDouble {"
  $$
  nest 2 (text "bool isValid;")
  $$
  nest 2 (text "double value;")
  $$
  text "};\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures ! \\result.isValid;"
  $$
  text "*/"
  $$
  text "struct maybeDouble noneDouble () {"
  $$
  nest 2 (text "struct maybeDouble result = { false, 0 };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures \\result.isValid;"
  $$
  text "ensures \\result.value == val;"
  $$
  text "*/"
  $$
  text "struct maybeDouble someDouble (double val) {"
  $$
  nest 2 (text "struct maybeDouble result = { true, val };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "struct maybeBool {"
  $$
  nest 2 (text "bool isValid;")
  $$
  nest 2 (text "bool value;")
  $$
  text "};\n"
  $$
  text "/*@"
  $$
  text "assigns \\nothing;"
  $$
  text "ensures ! \\result.isValid;"
  $$
  text "*/"
  $$
  text "struct maybeBool noneBool () {"
  $$
  nest 2 (text "struct maybeBool result = { false, false };")
  $$
  nest 2 (text "return result;")
  $$
  text "}\n"
  $$
  text "/*@ assigns \\nothing;"
  $$
  text "ensures \\result.isValid;"
  $$
  text "ensures \\result.value == val;"
  $$
  text "*/"
  $$
  text "struct maybeBool someBool (bool val) {"
  $$
  nest 2 (text "struct maybeBool result = { true, val };")
  $$
  nest 2 (text "return result;")
  $$
  text "}"
