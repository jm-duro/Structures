include std/dll.e
include std/machine.e
include std/convert.e
include std/search.e
include std/console.e
include std/os.e
include std/sequence.e
include std/text.e
include std/io.e
include euphoria/info.e
include common.e
include extract_vars.e

constant
  LNX_4_1_64 = 1, LNX_4_1_32 = 2, LNX_4_0_32 = 3,
  WIN_4_1_64 = 4, WIN_4_1_32 = 5, WIN_4_0_32 = 6

public integer eu_version = 0
ifdef WINDOWS then
  if version_minor() = 0 then
    eu_version = WIN_4_0_32
  elsif version_minor() = 1 then
    if sizeof( C_POINTER ) = 4 then
      eu_version = WIN_4_1_32
    elsif sizeof( C_POINTER ) = 8 then
      eu_version = WIN_4_1_64
    end if
  end if
elsifdef LINUX then
  s = getLnxVersion(cmd[1])
  if version_minor() = 0 then
    eu_version = LNX_4_0_32
  elsif version_minor() = 1 then
    if sizeof( C_POINTER ) = 4 then
      eu_version = LNX_4_1_32
    elsif sizeof( C_POINTER ) = 8 then
      eu_version = LNX_4_1_64
    end if
  end if
end ifdef

public constant
  T_CHAR = 1, T_UCHAR = 2, T_SHORT = 3, T_USHORT = 4, T_INT = 5, T_UINT = 6, T_LONG = 7, T_ULONG = 8, T_LONGLONG = 9, T_DWORDLONG = 10,
  T_FLOAT = 11, T_DOUBLE = 12, T_POINTER = 13, T_HANDLE = 14, T_HRESULT = 15, T_LONG_PTR = 16, T_PARAM = 17

constant ALIASES = {
  {"char", T_CHAR},
  {"signed_char", T_CHAR},
  {"uchar", T_UCHAR},
  {"unsigned_char", T_UCHAR},
  {"byte", T_UCHAR},
  {"short", T_SHORT},
  {"short_int", T_SHORT},
  {"signed_short", T_SHORT},
  {"signed_short_int", T_SHORT},
  {"ushort", T_USHORT},
  {"unsigned_short", T_USHORT},
  {"unsigned_short_int", T_USHORT},
  {"word", T_USHORT},
  {"int", T_INT},
  {"signed", T_INT},
  {"signed_int", T_INT},
  {"bool", T_INT},
  {"uint", T_UINT},
  {"unsigned", T_UINT},
  {"unsigned_int", T_UINT},
  {"dword", T_UINT},
  {"BOOL", T_UINT},
  {"long", T_LONG},
  {"long_int", T_LONG},
  {"signed_long", T_LONG},
  {"signed_long_int", T_LONG},
  {"ulong", T_ULONG},
  {"unsigned_long", T_ULONG},
  {"unsigned_long_int", T_ULONG},
  {"size_t", T_ULONG},
  {"long_long", T_LONGLONG},
  {"long_long_int", T_LONGLONG},
  {"signed_long_long", T_LONGLONG},
  {"signed_long_long_int", T_LONGLONG},
  {"unsigned_long_long", T_LONGLONG},
  {"unsigned_long_long_int", T_LONGLONG},
  {"float", T_FLOAT},
  {"double", T_DOUBLE},
  {"long_double", T_DOUBLE},
  {"double_double", T_DOUBLE},
  {"pointer", T_POINTER},
  {"LPARAM", T_PARAM},
  {"WPARAM", T_PARAM},
  {"HANDLE", T_HANDLE},
  {"HWND", T_HANDLE},
  {"HDC", T_HANDLE},
  {"HRESULT", T_HRESULT}
}

public constant TYPE_DEFINITION = {
-- name,          LNX_4_1_64,    LNX_4_1_32,    LNX_4_0_32, WIN_4_1_64,    WIN_4_1_32,    WIN_4_0_32
  {T_CHAR       , #01000001,     #01000001,     #01000001,  #01000001,     #01000001,     #01000001},
  {T_UCHAR      , #02000001,     #02000001,     #02000001,  #02000001,     #02000001,     #02000001},
  {T_SHORT      , #01000002,     #01000002,     #01000002,  #01000002,     #01000002,     #01000002},
  {T_USHORT     , #02000002,     #02000002,     #02000002,  #02000002,     #02000002,     #02000002},
  {T_INT        , #01000004,     #01000004,     #01000004,  #01000004,     #01000004,     #01000004},
  {T_UINT       , #02000004,     #02000004,     #02000004,  #02000004,     #02000004,     #02000004},
  {T_LONG       , #01000008,     #01000008,     #01000004,  #01000008,     #01000008,     #01000004},
  {T_ULONG      , #02000008,     #02000008,     #02000004,  #02000008,     #02000008,     #02000004},
  {T_LONGLONG   , #03000002,     #03000002,     0,          #03000002,     #03000002,     0        },
  {T_DWORDLONG  , #03000002,     #03000002,     #03000008,  #03000002,     #03000002,     #03000008},
  {T_FLOAT      , #03000004,     #03000004,     #03000004,  #03000004,     #03000004,     #03000004},
  {T_DOUBLE     , #03000008,     #03000008,     #03000008,  #03000008,     #03000008,     #03000008},
  {T_POINTER    , #03000001,     #03000001,     #02000004,  #03000001,     #03000001,     #02000004},
  {T_HANDLE     , #03000002,     #01000008,     #02000004,  #03000002,     #01000008,     #02000004},
  {T_HRESULT    , #01000008,     #01000008,     #01000004,  #01000008,     #01000008,     #01000004},
  {T_LONG_PTR   , #03000002,     #01000008,     0,          #03000002,     #01000008,     0        },
  {T_PARAM      , #03000001,     #03000001,     #01000004,  #03000001,     #03000001,     #01000004}
}

public constant DEF_TYPE=1, DEF_TYPE_NAME=2, DEF_VAR_NAME=3, DEF_BLOCK=4
public constant STRUCT=1, UNION=2
public constant SU_NAME=1, SU_SIZE=2, SU_DEF=3, SU_ADDRESS=4
public constant VAR_OFFSET=1, VAR_NAME=2, VAR_BITS=3, VAR_TYPE=4, VAR_SIZE=5

sequence ListOfStructures = {}

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
    if path[$] = DEF_BLOCK then
      prefix = sprintf("%d",n)
    else
      switch n do
        case 1 then prefix = "DEF_TYPE"
        case 2 then prefix = "DEF_TYPE_NAME"
        case 3 then prefix = "DEF_VAR_NAME"
        case 4 then prefix = "DEF_BLOCK"
        case else prefix = sprintf("%d",n)
      end switch
    end if
  else
    switch n do
      case 1 then prefix = "DEF_TYPE"
      case 2 then prefix = "DEF_TYPE_NAME"
      case 3 then prefix = "DEF_VAR_NAME"
      case 4 then prefix = "DEF_BLOCK"
      case else prefix = sprintf("%d",n)
    end switch
  end if
  log_printf("prefix = %s, value = %s\n", {prefix, value}, EXTENSIVE)
  return {prefix, value}
end function
public integer rtn_id = routine_id("identifyInstruction")

--------------------------------------------------------------------------------

public function decompose(sequence cstr)
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
      result[DEF_TYPE_NAME] = vars[i][VALUE]
    elsif equal(vars[i][KEY], "<var>") then
      result[DEF_VAR_NAME] = vars[i][VALUE]
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
      result[DEF_BLOCK] = s
    end if
  end for
  analyze_object(result, "result", VERBOSE, rtn_id, debug_show_prefix, debug_add_time_stamp, 2)
  log_puts("\n", VERBOSE)
  return result
end function

-----------------------------------------------------------------------------

public function getStructureTypeName(integer struct, integer n)
  log_printf("getStructureTypeName(%d, %d)\n", {struct, n}, VERBOSE)
  if struct = 0 then
    error_message("getStructureTypeName: struct cannot be null!", CRITICAL)
  end if
  analyze_object(ListOfStructures, sprintf("ListOfStructures[%d]", {struct}), VERBOSE)
  return ListOfStructures[struct][SU_DEF][n][DEF_TYPE_NAME]
end function

-----------------------------------------------------------------------------

public function getStructureVariableName(integer struct, integer n)
  log_printf("getStructureVariableName(%d, %d)\n", {struct, n}, VERBOSE)
  if struct = 0 then
    error_message("getStructureVariableName: struct cannot be null!", CRITICAL)
  end if
  analyze_object(ListOfStructures, sprintf("ListOfStructures[%d]", {struct}), VERBOSE)
  return ListOfStructures[struct][SU_DEF][n][DEF_VAR_NAME]
end function

-----------------------------------------------------------------------------

public function getStructureLength(integer struct)
  log_printf("getStructureLength(%d)\n", {struct}, VERBOSE)
  if struct = 0 then
    error_message("getStructureLength: struct cannot be null!", CRITICAL)
  end if
  analyze_object(ListOfStructures, sprintf("ListOfStructures[%d]", {struct}), VERBOSE)
  return ListOfStructures[struct][SU_SIZE]
end function

-----------------------------------------------------------------------------

public function getStructureAddress(integer struct)
  log_printf("getStructureAddress(%d)\n", {struct}, VERBOSE)
  if struct = 0 then
    error_message("getStructureAddress: struct cannot be null!", CRITICAL)
  end if
  analyze_object(ListOfStructures, sprintf("ListOfStructures[%d]", {struct}), VERBOSE)
  return ListOfStructures[struct][SU_ADDRESS]
end function
--------------------------------------------------------------------------------

-- creates a structure definition
-- automatically sizes structure to OEU version and OS Architecture
-- ex: atom p = createStructure(
--   "struct _mydata {" &
--   "  int a;" &
--   "  float b;" &
--   "  char c;" &
--   "} bar;")
public function createStructure(sequence cStruct, atom p)
  log_printf("createStructure(%s)\n", {cStruct}, EXTENSIVE)
  sequence def = decompose(cStruct)
  if def[DEF_TYPE] != STRUCT then return 0 end if
  integer lg = 0
  sequence definition = {}
  for i = 1 to length(def[DEF_BLOCK]) do
    sequence vars = extract_vars(def[DEF_BLOCK][i], "<type>[\\[<size>\\]] <name>[:<bits>];")
    dump_array(vars, "vars", VERBOSE)
    object cType = vlookup("<type>", vars, KEY, VALUE)
    if atom(cType) then
      error_message("createStructure: Incorrect variable type!", CRITICAL)
    end if
    object cSize = vlookup("<size>", vars, KEY, VALUE, 1)
    if sequence(cSize) then
      cSize = to_number(cSize)
    end if
    object cName = vlookup("<name>", vars, KEY, VALUE)
    if atom(cName) then
      error_message("createStructure: Incorrect variable name!", CRITICAL)
    end if
    object cBits = vlookup("<bits>", vars, KEY, VALUE)
    log_printf("createStructure: cType = %s, cSize = %d, cName = %s, cBits = %d\n", {cType, cSize, cName, cBits}, DEBUG)
    atom eutype = 0
    object varType = vlookup(cType, ALIASES, 1, 2)
    if varType = 0 then
      -- check if varType refers to another structure
      integer n = find_key(cType, ListOfStructures, SU_NAME)
      if n = 0 then
        error_message(sprintf("createStructure: No corresponding EU type found for C type %s!", {cType}), CRITICAL)
      end if
      eutype = -n
      log_printf("createStructure: lg = %d, euType = %d\n", {lg, eutype}, DEBUG)
      definition = append(definition, {lg, cName, cBits, eutype, cSize})
      lg += ListOfStructures[n][SU_SIZE] * cSize
    else
      eutype = vlookup(varType, TYPE_DEFINITION, 1, eu_version+1)
      log_printf("createStructure: lg = %d, euType = #%08x\n", {lg, eutype}, DEBUG)
      definition = append(definition, {lg, cName, cBits, eutype, cSize})
      lg += sizeof(eutype) * cSize
    end if
  end for
  log_printf("createStructure: {%s, %d, %s, %d}\n", {def[DEF_TYPE_NAME], lg, object_dump(definition), p}, DEBUG)
  ListOfStructures = append(ListOfStructures, {def[DEF_TYPE_NAME], lg, definition, p})
  analyze_object(ListOfStructures, "ListOfStructures", DEBUG)
  return length(ListOfStructures)
end function

--------------------------------------------------------------------------------

-- allocates memory according to a structure definition
-- automatically sizes structure to OEU version and OS Architecture
-- ex: atom p = allocateStructure(
--   "cStruct _mydata {" &
--   "  int a;" &
--   "  float b;" &
--   "  char c;" &
--   "} bar;")
public function allocateStructure(sequence cStruct)
  log_printf("allocateStructure(%s)\n", {cStruct}, DEBUG)
  atom p = 0
  integer struct = createStructure(cStruct, 0)
  if ListOfStructures[struct][SU_SIZE] = 0 then
    error_message("allocateStructure: Struct size cannot be null!", CRITICAL)
  end if
  ListOfStructures[struct][SU_ADDRESS] = allocate(ListOfStructures[struct][SU_SIZE])
  log_printf("allocateStructure: {%s, %d, %s, %d}\n", {ListOfStructures[struct][SU_NAME], ListOfStructures[struct][SU_SIZE],
             object_dump(ListOfStructures[struct][SU_DEF]), ListOfStructures[struct][SU_ADDRESS]}, DEBUG)
  return struct
end function

-----------------------------------------------------------------------------

procedure pokeVariable(atom p, atom ctype, object value)
  log_printf("pokeVariable(#%08x, #%08x, %s)\n", {p, ctype, object_dump(value)}, DEBUG)
  if p = 0 then
    error_message("pokeVariable: p cannot be null!", CRITICAL)
  end if
  if (ctype = #01000001) or (ctype = #02000001) then  -- 8-bit signed, unsigned
    poke(p, value)
  elsif (ctype = #01000002) or (ctype = #02000002) then  -- 16-bit signed, unsigned
    poke2(p, value)
  elsif (ctype = #01000004) or (ctype = #02000004) then  -- 32-bit signed, unsigned
    poke4(p, value)
  elsif (ctype = #01000008) or (ctype = #02000008) then  -- 64-bit signed, unsigned on Linux x64, 32-bit elsewhere
    if sizeof(C_LONG)=4 then     -- 32-bit
      poke4(p, value)
    elsif sizeof(C_LONG)=8 then  -- 64-bit
      poke8(p, value)
    end if
  elsif ctype = #03000001 then       -- pointer
    if sizeof(C_POINTER)=4 then     -- 32-bit
      poke4(p, value)
    elsif sizeof(C_POINTER)=8 then  -- 64-bit
      poke8(p, value)
    end if
  elsif ctype = #03000002 then       -- 64-bit integer
    poke8(p, value)
  elsif ctype = #03000004 then       -- 32-bit float
    poke(p, atom_to_float32(value))
  elsif ctype = #03000008 then       -- 64-bit float
    poke(p, atom_to_float64(value))
  end if
end procedure

-----------------------------------------------------------------------------

-- writes data in memory according to a structure definition
-- automatically sizes structure to OEU version and OS Architecture
-- ex: writeStructure(struct, {hwnd, 12, 25} )
public procedure writeStructure(integer struct, sequence values)
  log_printf("writeStructure(%d, %s)\n", {struct, object_dump(values)}, DEBUG)
  for i = 1 to length(ListOfStructures[struct][SU_DEF]) do
    integer offset = ListOfStructures[struct][SU_DEF][i][VAR_OFFSET]
    atom ctype = ListOfStructures[struct][SU_DEF][i][VAR_TYPE]
    atom p = ListOfStructures[struct][SU_ADDRESS] + offset
    log_printf("writeStructure: offset = %d, ctype = #%08x, p = #%08x\n", {offset, ctype, p}, DEBUG)
    if ctype > 0 then
      pokeVariable(p, ctype, values[i])
    else               -- structure
      log_printf("writeStructure: ListOfStructures[%d][SU_ADDRESS] = %d\n", {-ctype, ListOfStructures[-ctype][SU_ADDRESS]}, DEBUG)
      poke_pointer(p, ListOfStructures[-ctype][SU_ADDRESS])
    end if
  end for
end procedure

-----------------------------------------------------------------------------

public procedure writeVariable(integer struct, sequence variable, object value)
  log_printf("writeVariable(%d, %s, %s)\n", {struct, object_dump(variable), object_dump(value)}, DEBUG)
  /*
  -- bits is not supported yet so no need to extract vars
  sequence vars = extract_vars(element, "<name>[:<bits>]")
  object cName = vlookup("<name>", vars, KEY, VALUE)
  if atom(cName) then
    error_message("writeVariable: Incorrect variable name!", CRITICAL)
  end if
  object cBits = vlookup("<bits>", vars, KEY, VALUE)
  */
  integer n = find_key(variable, ListOfStructures[struct][SU_DEF], VAR_NAME)
  if n = 0 then
    error_message("writeVariable: Incorrect variable name!", CRITICAL)
  end if
  integer offset = ListOfStructures[struct][SU_DEF][n][VAR_OFFSET]
  atom ctype = ListOfStructures[struct][SU_DEF][n][VAR_TYPE]
  atom p = ListOfStructures[struct][SU_ADDRESS] + offset
  log_printf("writeVariable: offset = %d, ctype = #%08x, p = #%08x\n", {offset, ctype, p}, DEBUG)
  if ctype > 0 then
    pokeVariable(p, ctype, value)
  else
    poke_pointer(p, ListOfStructures[-ctype][SU_ADDRESS])
  end if
end procedure

-----------------------------------------------------------------------------

function peekVariable(atom p, atom ctype)
  log_printf("peekVariable(#%08x, #%08x)\n", {p, ctype}, DEBUG)
  if p = 0 then
    error_message("peekVariable: p cannot be null!", CRITICAL)
  end if
  object result
    if ctype = #01000001 then       -- 8-bit signed
      return peeks(p)
    elsif ctype = #01000002 then       -- 16-bit signed
      return peek2s(p)
    elsif ctype = #01000004 then       -- 32-bit signed
      return peek4s(p)
    elsif ctype = #01000008 then       -- 64-bit signed
      if sizeof(C_LONG)=4 then     -- 32-bit
        return peek4s(p)
      elsif sizeof(C_LONG)=8 then  -- 64-bit
        return peek8s(p)
      end if
    elsif ctype = #02000001 then       -- 8-bit unsigned
      return peek(p)
    elsif ctype = #02000002 then       -- 16-bit unsigned
      return peek2u(p)
    elsif ctype = #02000004 then       -- 32-bit unsigned
      return peek4u(p)
    elsif ctype = #02000008 then       -- 64-bit unsigned
      if sizeof(C_LONG)=4 then     -- 32-bit
        return peek4u(p)
      elsif sizeof(C_LONG)=8 then  -- 64-bit
        return peek8u(p)
      end if
    elsif ctype = #03000001 then       -- pointer
      if sizeof( C_POINTER )=4 then     -- 32-bit
        return peek4s(p)
      elsif sizeof( C_POINTER )=8 then  -- 64-bit
        return peek8s(p)
      end if
    elsif ctype = #03000002 then       -- 64-bit integer
      return peek8u(p)
    elsif ctype = #03000004 then       -- 32-bit float
      return float32_to_atom(peek({p, 4}))
    elsif ctype = #03000008 then       -- 64-bit float
      return float64_to_atom(peek({p, 8}))
    else
      error_message("peekVariable: Invalid ctype", CRITICAL)
      return 0  -- useless but needed for OEU parser
    end if
end function

-----------------------------------------------------------------------------

-- reads data in memory according to a structure definition
-- automatically sizes structure to OEU version and OS Architecture
-- ex: sequence s = readStructure(struct)
public function readStructure(integer struct)
  log_printf("readStructure(%d)\n", {struct}, DEBUG)
  hexDump(peek({ListOfStructures[struct][SU_ADDRESS], ListOfStructures[struct][SU_SIZE]}), ListOfStructures[struct][SU_ADDRESS], DEBUG)
  sequence  result = {}
  for i = 1 to length(ListOfStructures[struct][SU_DEF]) do
    integer offset = ListOfStructures[struct][SU_DEF][i][VAR_OFFSET]
    atom ctype = ListOfStructures[struct][SU_DEF][i][VAR_TYPE]
    atom p = ListOfStructures[struct][SU_ADDRESS] + offset
    log_printf("readStructure: offset = %d, ctype = #%08x, p = #%08x\n", {offset, ctype, p}, DEBUG)
    if ctype > 0 then
      result = append( result, peekVariable(p, ctype))
    else               -- structure
      result = append( result, peek_pointer(p))  -- ListOfStructures[-ctype][SU_ADDRESS]
    end if
  end for
  analyze_object(result, "result", DEBUG)
  return result
end function

-----------------------------------------------------------------------------

public function readVariable(integer struct, sequence variable)
  log_printf("readVariable(%d, %s)\n", {struct, object_dump(variable)}, DEBUG)
  integer n = find_key(variable, ListOfStructures[struct][SU_DEF], VAR_NAME)
  if n = 0 then
    error_message("readVariable: Incorrect variable name!", CRITICAL)
  end if
  integer offset = ListOfStructures[struct][SU_DEF][n][VAR_OFFSET]
  atom ctype = ListOfStructures[struct][SU_DEF][n][VAR_TYPE]
  atom p = ListOfStructures[struct][SU_ADDRESS] + offset
  log_printf("readVariable: offset = %d, ctype = #%08x, p = #%08x\n", {offset, ctype, p}, DEBUG)
  if ctype > 0 then
    return peekVariable(p, ctype)
  else
    return peek_pointer(p)
  end if
end function

-----------------------------------------------------------------------------

-- frees memory according to a structure definition
-- automatically frees pointers included in the structure
-- according to OEU version and OS Architecture
-- ex: freeStructure(struct)
public procedure freeStructure(integer struct)
  log_printf("freeStructure(%d)\n", {struct}, DEBUG)
  for i = 1 to length(ListOfStructures[struct][SU_DEF]) do
    integer offset = ListOfStructures[struct][SU_DEF][i][VAR_OFFSET]
    atom ctype = ListOfStructures[struct][SU_DEF][i][VAR_TYPE]
    atom p = ListOfStructures[struct][SU_ADDRESS] + offset
    if find(ListOfStructures[struct][SU_DEF][i], {T_POINTER, T_HANDLE, T_LONG_PTR, T_PARAM}) then
      free(p)
    end if
  end for
  free(ListOfStructures[struct][SU_ADDRESS])
end procedure
