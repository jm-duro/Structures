include std/dll.e 
include std/machine.e 
include ../include/structs.e

-- Some Basic Colors
public constant LIGHTGRAY  = { 200, 200, 200, 255 } -- Light Gray
public constant GRAY       = { 130, 130, 130, 255 } -- Gray
public constant DARKGRAY   = { 80, 80, 80, 255 }    -- Dark Gray
public constant YELLOW     = { 253, 249, 0, 255 }   -- Yellow
public constant GOLD       = { 255, 203, 0, 255 }   -- Gold
public constant ORANGE     = { 255, 161, 0, 255 }   -- Orange
public constant PINK       = { 255, 109, 194, 255 } -- Pink
public constant RED        = { 230, 41, 55, 255 }   -- Red
public constant MAROON     = { 190, 33, 55, 255 }   -- Maroon
public constant GREEN      = { 0, 228, 48, 255 }    -- Green
public constant LIME       = { 0, 158, 47, 255 }    -- Lime
public constant DARKGREEN  = { 0, 117, 44, 255 }    -- Dark Green
public constant SKYBLUE    = { 102, 191, 255, 255 } -- Sky Blue
public constant BLUE       = { 0, 121, 241, 255 }   -- Blue
public constant DARKBLUE   = { 0, 82, 172, 255 }    -- Dark Blue
public constant PURPLE     = { 200, 122, 255, 255 } -- Purple
public constant VIOLET     = { 135, 60, 190, 255 }  -- Violet
public constant DARKPURPLE = { 112, 31, 126, 255 }  -- Dark Purple
public constant BEIGE      = { 211, 176, 131, 255 } -- Beige
public constant BROWN      = { 127, 106, 79, 255 }  -- Brown
public constant DARKBROWN  = { 76, 63, 47, 255 }    -- Dark Brown
public constant WHITE      = { 255, 255, 255, 255 } -- White
public constant BLACK      = { 0, 0, 0, 255 }       -- Black
public constant BLANK      = { 0, 0, 0, 0 }         -- Blank (Transparent)
public constant MAGENTA    = { 255, 0, 255, 255 }   -- Magenta
public constant RAYWHITE   = { 245, 245, 245, 255 } -- My own White (raylib logo)

------------------------------------------------------------------------------------
-- Structures Definition
------------------------------------------------------------------------------------

-- Vector2, 2 components 
public sequence RL_VECTOR2 =
"typedef struct Vector2 {" &
"  float x;" &                -- Vector x component 
"  float y;" &                -- Vector y component 
"} Vector2;"
 
-- Vector3, 3 components
public sequence RL_VECTOR3 =
"typedef struct Vector3 {" &
"    float x;" &                -- Vector x component
"    float y;" &                -- Vector y component
"    float z;" &                -- Vector z component
"} Vector3;"

-- Color, 4 components, R8G8B8A8 (32bit)
public sequence RL_COLOR =
"typedef struct Color {" &
"    unsigned_char r;" &        -- Color red value 
"    unsigned_char g;" &        -- Color green value 
"    unsigned_char b;" &        -- Color blue value 
"    unsigned_char a;" &        -- Color alpha value 
"} Color;" 
 
constant raylib = open_dll( "raylib.dll" ),
  xInitWindow        = define_c_proc( raylib, "+InitWindow", {C_INT,C_INT,C_POINTER} ),
  xWindowShouldClose = define_c_func( raylib, "+WindowShouldClose", {}, C_BYTE ),
  xCloseWindow       = define_c_proc( raylib, "+CloseWindow", {} ),
  xGetWindowPosition = define_c_func( raylib, "+GetWindowPosition", {}, C_POINTER ),
  xSetTargetFPS      = define_c_proc( raylib, "+SetTargetFPS", {C_INT} ),
  xClearBackground   = define_c_proc( raylib, "+ClearBackground", {C_POINTER} ),
  xBeginDrawing      = define_c_proc( raylib, "+BeginDrawing", {} ),
  xEndDrawing        = define_c_proc( raylib, "+EndDrawing", {} ),
  -- xBeginMode3D       = define_c_proc( raylib, "+BeginMode3D", {C_POINTER} ),
  -- xEndMode3D         = define_c_proc( raylib, "+EndMode3D", {} ),
  xDrawFPS           = define_c_proc( raylib, "+DrawFPS", {C_INT,C_INT} ),
  xDrawText          = define_c_proc( raylib, "+DrawText", {C_POINTER,C_INT,C_INT,C_INT,C_POINTER} ),
  xDrawCube          = define_c_proc( raylib, "+DrawCube", {C_POINTER,C_FLOAT,C_FLOAT,C_FLOAT,C_POINTER} ),
  xDrawCubeWires     = define_c_proc( raylib, "+DrawCubeWires", {C_POINTER,C_FLOAT,C_FLOAT,C_FLOAT,C_POINTER} ),
  xDrawGrid          = define_c_proc( raylib, "+DrawGrid", {C_INT,C_FLOAT} ),
$

public procedure InitWindow( integer width, integer height, sequence title )
  c_proc( xInitWindow, {width,height,allocate_string(title)} ) -- title string is allocated/freed automatically
end procedure

public function WindowShouldClose()
  return c_func( xWindowShouldClose, {} )
end function

public procedure CloseWindow()
  c_proc( xCloseWindow, {} )
end procedure

public function GetWindowPosition()
  atom p = c_func( xGetWindowPosition, {} ) -- returns {x,y}
  sequence Vector2 = createStructure(RL_VECTOR2, p)
  return readStructure(Vector2)
end function

public procedure SetTargetFPS( integer fps )
  c_proc( xSetTargetFPS, {fps} )
end procedure

public procedure ClearBackground( sequence color )
  integer Color = allocateStructure(RL_COLOR)
  writeStructure(Color, color )
  c_proc( xClearBackground, {peek4u(getStructureAddress(Color))}) -- color is {r,g,b,a}
end procedure

public procedure BeginDrawing()
  c_proc( xBeginDrawing, {} )
end procedure

public procedure EndDrawing()
  c_proc( xEndDrawing, {} )
end procedure
/*
public procedure BeginMode3D( sequence camera )
  c_proc( xBeginMode3D, {camera} )
end procedure

public procedure EndMode3D()
  c_proc( xEndMode3D, {} )
end procedure
*/
public procedure DrawFPS( integer posX, integer posY )
  c_proc( xDrawFPS, {posX,posY} )
end procedure

public procedure DrawText( sequence text, integer posX, integer posY, integer fontSize, sequence color )
  integer Color = allocateStructure(RL_COLOR)
  writeStructure(Color, color )
  c_proc( xDrawText, {allocate_string(text),posX,posY,fontSize,getStructureAddress(Color)} )
  freeStructure(Color)
end procedure

public procedure DrawCube( sequence position, atom width, atom height, atom length, sequence color )
  integer Color = allocateStructure(RL_COLOR)
  writeStructure(Color, color )
  c_proc( xDrawCube, {position,width,height,length,getStructureAddress(Color)} )
  freeStructure(Color)
end procedure

public procedure DrawCubeWires( sequence position, atom width, atom height, atom length, sequence color )
  integer Color = allocateStructure(RL_COLOR)
  writeStructure(Color, color )
  c_proc( xDrawCubeWires, {position,width,height,length,getStructureAddress(Color)} )
  freeStructure(Color)
end procedure

public procedure DrawGrid( integer slices, atom spacing )
  c_proc( xDrawGrid, {slices,spacing} )
end procedure

