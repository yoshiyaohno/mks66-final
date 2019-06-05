{
module Lexer where
}

%wrapper "basic"

tokens :-

    [\ \t\n]+           ;

    \-?[0-9]+ | \-?[0-9]+\.[0-9]+
                        { \s -> TkDouble (read s) }
    \-?[0-9]+\.         { \s -> TkDouble (read . init $ s) }
    \.[0-9]+            { \s -> TkDouble (read ('0':s)) }
    \-\.[0-9]+          { \s -> TkDouble (-1 * (read . ('0':) . tail $ s)) }
    
    "//".*              ;

    "light"             { const TkLight }
    "constants"         { const TkConstants }
    "save_coord_system" { const TkSaveCoords }
    "camera"            { const TkCamera }
    "ambient"           { const TkAmbient }
    "sphere"            { const TkSphere }
    "torus"             { const TkTorus }
    "box"               { const TkBox }
    "line"              { const TkLine }
    "mesh"              { const TkMesh }
    "texture"           { const TkTexture }

    "set"               { const TkSet }
    "move"              { const TkMove }
    "scale"             { const TkScale }
    "rotate"            { const TkRotate }
    "basename"          { const TkBasename }
    "save_knobs"        { const TkSaveKnobs }
    "tween"             { const TkTween }
    "frames"            { const TkFrames }
    "vary"              { const TkVary }

    "push"              { const TkPush }
    "pop"               { const TkPop }
    "save"              { const TkSave }
    "generate_rayfiles" { const TkGenerateRayfiles }

    "shading"           { const TkShading }

    phong|flat|gouraud|raytrace|wireframe
                        { \s -> TkShadingType s }
                        
    "setknobs"          { const TkSetknobs }
    "focal"             { const TkFocal }
    "display"           { const TkDisplay }
    "web"               { const TkWeb }
    
    ":"                 { const TkColon }

    [a-zA-z][\.a-zA-Z0-9_]*
                        { \s -> TkString s }

{

lexString :: String -> [Token]
lexString = alexScanTokens

-- token datatype
data Token  = TkDouble Double
            | TkString String
            | TkLight       | TkConstants
            | TkSaveCoords  | TkCamera
            | TkAmbient     | TkTorus
            | TkBox         | TkLine
            | TkMesh        | TkTexture
            | TkSet         | TkMove
            | TkScale       | TkRotate
            | TkBasename    | TkSaveKnobs
            | TkTween       | TkFrames
            | TkVary        | TkPush
            | TkPop         | TkSave
            | TkGenerateRayfiles
            | TkShading     | TkShadingType String
            | TkSetknobs    | TkFocal
            | TkDisplay     | TkWeb
            | TkColon       | TkSphere
            deriving (Eq, Show)
}
