include std/search.e      -- for begins()
include std/sequence.e    -- for columnize()
include std/text.e        -- for trim()
include std/math.e        -- for max()
include std/pretty.e      -- for pretty_sprint()
include common.e

public constant KEY=1, LEVEL=2, VALUE=3, PMIN=4, PMAX=5

--------------------------------------------------------------------------------

function add_fixed(sequence result, sequence fixed, integer option_level)
  sequence trimmed = trim(fixed)
  if length(trimmed) then
    result = append(result, {trimmed, option_level, "", 0, 0})
  else
    if length(fixed) then
      result = append(result, {" ", option_level, "", 0, 0})
    end if
  end if
  return result
end function

--------------------------------------------------------------------------------

function split_template(sequence s)
  integer option_level = 0  -- current option level
  sequence result = {}
  integer inside_variable = 0
  sequence fixed = "", trimmed = ""
  sequence variable = ""
  for i = 1 to length(s) do
    if s[i] = '<' then
      inside_variable = 1
      variable = "<"
      result = add_fixed(result, fixed, option_level)
      fixed = ""
    elsif s[i] = '>' then
      inside_variable = 0
      result = append(result, {variable & ">", option_level, "", 0, 0})
      fixed = ""
    elsif s[i] = '[' then
      result = add_fixed(result, fixed, option_level)
      fixed = ""
      option_level += 1
    elsif s[i] = ']' then
      result = add_fixed(result, fixed, option_level)
      fixed = ""
      option_level -= 1
    else
      if inside_variable then
        variable &= s[i]
      else
        fixed &= s[i]
      end if
    end if
  end for
  result = add_fixed(result, fixed, option_level)
  return result
end function

--------------------------------------------------------------------------------

function filter_template_by_level(sequence details, integer level)
  sequence tmpl = {}
  for i = 1 to length(details) do
    if details[i][LEVEL] <= level then
      tmpl = append(tmpl, details[i])
    else
      tmpl = append(tmpl, 0)
    end if
  end for
  return tmpl
end function

------------------------------------------------------------------------------

function get_lower_limit(sequence details, integer from, integer default)
  integer sc = default
  for i = from-1 to 1 by -1 do
    if atom(details[i]) then continue end if
    if details[i][PMAX] > 0 then
      sc = details[i][PMAX] + 1
      exit
    end if
  end for
  return sc
end function

------------------------------------------------------------------------------

function get_upper_limit(sequence details, integer from, integer default)
  integer ec = default
  for i = from+1 to length(details) do
    if atom(details[i]) then continue end if
    if details[i][PMIN] > 0 then
      ec = details[i][PMIN] - 1
      exit
    end if
  end for
  return ec
end function

------------------------------------------------------------------------------

function remove_unused_option(sequence details, integer from)
  if not begins("<", details[from][KEY]) then
    integer lvl = details[from][LEVEL]
    integer n = from
    while sequence(details[n]) and (details[n][LEVEL] = lvl) do
      details[n] = 0
      n += 1
      if n > length(details) then exit end if
    end while
  end if
  return details
end function

--------------------------------------------------------------------------------

public function extract_vars(sequence code, sequence template)
  integer n = 0
  log_puts("\n\n"& repeat('-', 80) & "\n", VERBOSE)
  code = trim_spaces(code)
  log_puts("0        1         2         3         4         5         6         7         8         9\n", EXTENSIVE)
  log_puts("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890\n", EXTENSIVE)
  log_puts(code & "\n", VERBOSE)
  sequence details = split_template(template)
  dump_array(details, "1) details", VERBOSE)
  -- make posts
  integer max_level = max(columnize(details, LEVEL))
  integer lgt = length(details), lgc = length(code)
  for level = 0 to max_level label "main" do
    sequence tmpl = filter_template_by_level(details, level)
    log_puts("\n", VERBOSE)
    dump_array(tmpl, sprintf("tmpl (level %d)", {level}), VERBOSE)
    -- make posts
    integer st = 1, et = lgt
    integer sc = 1, ec = lgc
    while (et >= st) do
      log_printf("st = %2d, et = %2d\n", {st, et}, EXTENSIVE)
      if atom(tmpl[st]) or begins("<", tmpl[st][KEY]) then
        st += 1
        continue
      end if
      sc = get_lower_limit(details, st, 1)
      ec = get_upper_limit(details, st, lgc)
      log_printf("a) st = %2d, et = %2d, sc = %2d, ec = %2d\n", {st, et, sc, ec}, EXTENSIVE)
      n = match(tmpl[st][KEY], code, sc)
      if (n = 0) or (n > ec) then
        st += 1
        continue
      end if
      log_printf("b) st = %d, key = \"%s\", n = %d\n", {st, tmpl[st][KEY], n}, EXTENSIVE)
      details[st][PMIN] = n
      details[st][PMAX] = n + length(tmpl[st][KEY]) - 1
      details[st][VALUE] = tmpl[st][KEY]
      dump_array(details, "1a) details", EXTENSIVE)
      st += 1
      if atom(tmpl[et]) or begins("<", tmpl[et][KEY]) then
        et -= 1
        continue
      end if
      sc = get_lower_limit(details, et, 1)
      ec = get_upper_limit(details, et, lgc)
      log_printf("c) st = %2d, et = %2d, sc = %2d, ec = %2d\n", {st, et, sc, ec}, EXTENSIVE)
      n = rmatch(tmpl[et][KEY], code, ec)
      if (n = 0) or (n < sc) then
        et -= 1
        continue
      end if
      log_printf("et = %d, key = %s, n = %d\n", {et, tmpl[et][KEY], n}, EXTENSIVE)
      details[et][PMIN] = n
      details[et][PMAX] = n + length(tmpl[et][KEY]) - 1
      details[et][VALUE] = tmpl[et][KEY]
      dump_array(details, "1b) details", EXTENSIVE)
      et -= 1
    end while
  end for
  dump_array(details, "2) details", VERBOSE)
  -- check and fill intervals
  for i = 1 to length(details) label "main" do
    if atom(details[i]) then  -- unused option
      continue
    end if
    if not begins("<", details[i][KEY]) then continue end if
    -- first code item should match
    integer sc = get_lower_limit(details, i, 1)
    integer ec = get_upper_limit(details, i, lgc)
    if ec >= sc then
      sequence s = trim(code[sc..ec])
      if length(s) then
        details[i][VALUE] = s
        details[i][PMIN] = sc
        details[i][PMAX] = ec
        code[sc..ec] = ' '
      else
        details[i] = 0
      end if
    else
      details[i] = 0
    end if
    if length(trim(code)) = 0 then exit "main" end if
  end for
  dump_array(details, "3) details", VERBOSE)
  -- extract variables
  sequence vars = {}
  for l = 1 to length(details) do
    if atom(details[l]) then continue end if
    if begins("<", details[l][KEY]) then
      if details[l][PMIN] = 0 then
        continue
      end if
      vars = append(vars, details[l])
    end if
  end for
  return vars
end function
