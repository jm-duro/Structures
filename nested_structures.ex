include std/dll.e
include std/machine.e
include std/console.e
include include/common.e
include include/structs.e

sequence s

sequence RECT = "struct RECT {" &
    "long left;" &
    "long top;" &
    "long right;" &
    "long bottom;}"

sequence PAINTSTRUCT = 
"struct PAINTSTRUCT {" &
    "long hdc;" &
    "BOOL fErase;" &
    "RECT rcPaint;" &
    "BOOL fRestore;" &
    "BOOL fIncUpdate;" &
    "byte[32] rgbReserved;}"

f_debug = open("debug.log", "w")
debug_level = DEBUG

integer rect = allocateStructure(RECT)
writeStructure(rect, {10,20,50,100} )
s = readStructure(rect)
printf(f_debug, "%s = %d, %s = %d, %s = %d, %s = %d\n", {
  getStructureTypeName(rect, 1), s[1],
  getStructureTypeName(rect, 2), s[2],
  getStructureTypeName(rect, 3), s[3],
  getStructureTypeName(rect, 4), s[4]
})
integer paintstruct = allocateStructure(PAINTSTRUCT)
writeStructure(paintstruct, {0, 1, rect, 0, 0, repeat(0, 32)} )
s = readStructure(paintstruct)
s = getStructureTypeName(paintstruct, 1)
printf(f_debug, "getStructureTypeName(paintstruct, 1) = \"%s\"\n", {s})
s = getStructureTypeName(paintstruct, 2)
printf(f_debug, "getStructureTypeName(paintstruct, 2) = \"%s\"\n", {s})
s = getStructureTypeName(paintstruct, 3)
printf(f_debug, "getStructureTypeName(paintstruct, 3) = \"%s\"\n", {s})
printf(f_debug, "%s = \"%s\", %s = %4.1f, %s = '%s'\n", {
  getStructureTypeName(paintstruct, 1), peek_string(s[1]),
  getStructureTypeName(paintstruct, 2), s[2],
  getStructureTypeName(paintstruct, 3), s[3]
})
/*
writeVariable(paintstruct, "b", 40.0)
s = readStructure(paintstruct)
atom b = readVariable(paintstruct, "b")
printf(f_debug, "b = %4.1f\n", {b})
printf(f_debug, "%s = \"%s\", %s = %4.1f, %s = '%s'\n", {
  getStructureTypeName(paintstruct, 1), peek_string(s[1]),
  getStructureTypeName(paintstruct, 2), s[2],
  getStructureTypeName(paintstruct, 3), s[3]
})
*/
freeStructure(paintstruct)

close(f_debug)
maybe_any_key()
