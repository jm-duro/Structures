include std/sequence.e
include std/text.e
include std/io.e
include std/console.e
include common.e
include extract_vars.e

sequence myStructure = 
"struct _mydata {" &
"  int a;" &
"  float b;" &
"  char c;" &
"} bar;"
/*
sequence myStructure = 
"struct _mydata {" &
"    int which_one;" &
"    union _data {" &
"            int a;" &
"            float b;" &
"            char c;" &
"    } foo;" &
"} bar;"
*/
sequence s3 = "switch (bar.which_one)" &
"{" &
"   case INTEGER  :  /* access bar.foo.a;*/ break;" &
"   case FLOATING :  /* access bar.foo.b;*/ break;" &
"   case CHARACTER:  /* access bar.foo.c;*/ break;" &
"}"

constant SU_TYPE=1, SU_TYPE_NAME=2, SU_VAR_NAME=3, SU_BLOCK=4
constant STRUCT=1, UNION=2

--------------------------------------------------------------------------------

function split_first_level(sequence str)
  sequence s
  integer level = 0
  sequence result = {}
  integer slen = length(str)
  if slen = 0 then return result end if

  sequence item = {}
  for i = 1 to slen do
    if str[i] = '{' then
      item = item & str[i]
      level += 1
      continue
    elsif str[i] = '}' then
      item = item & str[i]
      level -= 1
      continue
    end if
    item = item & str[i]
    if str[i] = ';' then
      if level = 0 then
        s = trim(item)
        -- if length(s) then result = append(result, s) end if
        result = append(result, s)
        item = {}
      end if
    end if
  end for
  s = trim(item)
  if length(s) then result = append(result, s) end if
  dump_array(result, "result", EXTENSIVE)
  return result
end function

--------------------------------------------------------------------------------

function identifyInstruction(sequence path, integer level, integer n, object x={})
  log_printf("  path: %s, level: %d, n: %d, x: %s\n", {object_dump(path), level, n, object_dump(x)}, EXTENSIVE)
  sequence prefix = "", value = ""
  if integer(x) then
    log_printf("n = %d, x = %d\n", {n, x}, EXTENSIVE)
    if n = 1 then
      if x = 1 then value = "STRUCT"
      elsif x = 2 then value = "UNION"
      else
        value = sprintf("%d", x)
      end if
    else
      value = sprintf("%d", x)
    end if
  end if
  if length(path) then
    if path[$] = SU_BLOCK then
      prefix = sprintf("%d",n)
    else
      switch n do
        case 1 then prefix = "SU_TYPE"
        case 2 then prefix = "SU_TYPE_NAME"
        case 3 then prefix = "SU_VAR_NAME"
        case 4 then prefix = "SU_BLOCK"
        case else prefix = sprintf("%d",n)
      end switch
    end if
  else
    switch n do
      case 1 then prefix = "SU_TYPE"
      case 2 then prefix = "SU_TYPE_NAME"
      case 3 then prefix = "SU_VAR_NAME"
      case 4 then prefix = "SU_BLOCK"
      case else prefix = sprintf("%d",n)
    end switch
  end if
  log_printf("prefix = %s, value = %s\n", {prefix, value}, EXTENSIVE)
  return {prefix, value}
end function
integer rtn_id = routine_id("identifyInstruction")

--------------------------------------------------------------------------------

function decompose(sequence cstr)
  sequence vars = {}
  sequence s = ""
  integer ns = match("struct", cstr)
  integer nu = match("union", cstr)
  integer stType = 0
  if (ns = 0) and (nu = 0) then return vars end if
  if ns = 0 then  -- union
    vars = extract_vars(cstr, "[typedef] union [<type>] { <block> } [<var>];")
    stType = UNION
  elsif nu = 0 then  -- struct
    vars = extract_vars(cstr, "[typedef] struct [<type>] { <block> } [<var>];")
    stType = STRUCT
  else
    if ns < nu then  -- struct
      vars = extract_vars(cstr, "[typedef] struct [<type>] { <block> } [<var>];")
      stType = STRUCT
    else  -- union
      vars = extract_vars(cstr, "[typedef] union [<type>] { <block> } [<var>];")
      stType = UNION
    end if
  end if
  dump_array(vars, "vars", VERBOSE, 3, debug_show_prefix, debug_add_time_stamp)
  sequence result = {stType, "", "", {}}
  for i = 1 to length(vars) do
    if equal(vars[i][KEY], "<type>") then
      result[SU_TYPE_NAME] = vars[i][VALUE]
    elsif equal(vars[i][KEY], "<var>") then
      result[SU_VAR_NAME] = vars[i][VALUE]
    else
      if find(';', vars[i][VALUE]) then
        s =  split_first_level(vars[i][VALUE])
        for j = 1 to length(s) do
          s[j] = trim_spaces(s[j])
          if match("struct ", s[j]) or match("union", s[j]) then
            s[j] = decompose(s[j])
          end if
        end for
      else
        s = trim_spaces(vars[i][VALUE])
        if match("struct ", s) or match("union", s) then
          s = decompose(s)
        end if
      end if
      result[SU_BLOCK] = s
    end if
  end for
  analyze_object(result, "result", VERBOSE, rtn_id, debug_show_prefix, debug_add_time_stamp, 2)
  log_puts("\n", VERBOSE)
  return result
end function

--------------------------------------------------------------------------------

f_debug = open("debug.log", "w")
debug_level = DEBUG
-- error_level = WARNING
-- debug_show_prefix = 0
-- debug_add_time_stamp = 0

sequence vars = decompose(myStructure)
analyze_object(vars, "vars", DEBUG, rtn_id, debug_show_prefix, debug_add_time_stamp, 2)
close(f_debug)
maybe_any_key()
