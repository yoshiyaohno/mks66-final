{
module Parser where

import Lexer

}

%name parseReversed
%tokentype { Token }
%error { parseError }

%token
    str                 { TkString $$ }
    dbl                 { TkDouble $$ }
    light               { TkLight }
    constants           { TkConstants }
    save_coords         { TkSaveCoords }
    camera              { TkCamera }
    ambient             { TkAmbient }
    sphere              { TkSphere }
    torus               { TkTorus }
    box                 { TkBox }
    line                { TkLine }
    mesh                { TkMesh }
    texture             { TkTexture }
    set                 { TkSet }
    move                { TkMove }
    scale               { TkScale }
    rotate              { TkRotate }
    basename            { TkBasename }
    save_knobs          { TkSaveKnobs }
    tween               { TkTween }
    frames              { TkFrames }
    vary                { TkVary }
    push                { TkPush }
    pop                 { TkPop }
    save                { TkSave }
    generate_rayfiles   { TkGenerateRayfiles }
    shading             { TkShading }
    shading_type        { TkShadingType $$ }
    setknobs            { TkSetknobs }
    focal               { TkFocal }
    display             { TkDisplay }
    web                 { TkWeb }
    ':'                 { TkColon }

%%

Input   : Input Command     { $2:$1 }
        | {- empty -}       { [] }
        ;

Command : sphere dbl dbl dbl dbl
            { CmdSphere Nothing ($2,$3,$4) $5 Nothing }
        | sphere str dbl dbl dbl dbl
            { CmdSphere (Just $2) ($3,$4,$5) $6 Nothing }
        | sphere dbl dbl dbl dbl str
            { CmdSphere Nothing ($2,$3,$4) $5 (Just $6) }
        | sphere str dbl dbl dbl dbl str
            { CmdSphere (Just $2) ($3,$4,$5) $6 (Just $7) }

        | box dbl dbl dbl dbl dbl dbl
            { CmdBox Nothing ($2,$3,$4) ($5,$6,$7) Nothing }
        | box str dbl dbl dbl dbl dbl dbl
            { CmdBox (Just $2) ($3,$4,$5) ($6,$7,$8) Nothing }
        | box dbl dbl dbl dbl dbl dbl str
            { CmdBox Nothing ($2,$3,$4) ($5,$6,$7) (Just $8) }
        | box str dbl dbl dbl dbl dbl dbl str
            { CmdBox (Just $2) ($3,$4,$5) ($6,$7,$8) (Just $9) }

        | torus dbl dbl dbl dbl dbl
            { CmdTorus Nothing ($2,$3,$4) $5 $6 Nothing }
        | torus str dbl dbl dbl dbl dbl
            { CmdTorus (Just $2) ($3,$4,$5) $6 $7 Nothing }
        | torus dbl dbl dbl dbl dbl str
            { CmdTorus Nothing ($2,$3,$4) $5 $6 (Just $7) }
        | torus str dbl dbl dbl dbl dbl str
            { CmdTorus (Just $2) ($3,$4,$5) $6 $7 (Just $8) }

        | line dbl dbl dbl dbl dbl dbl
            { CmdLine Nothing ($2,$3,$4) Nothing ($5,$6,$7) Nothing }
        | line dbl dbl dbl str dbl dbl dbl
            { CmdLine Nothing ($2,$3,$4) (Just $5) ($6,$7,$8) Nothing }
        | line dbl dbl dbl dbl dbl dbl str 
            { CmdLine Nothing ($2,$3,$4) Nothing ($5,$6,$7) (Just $8) }
        | line dbl dbl dbl str dbl dbl dbl str
            { CmdLine Nothing ($2,$3,$4) (Just $5) ($6,$7,$8) (Just $9) }
        | line str dbl dbl dbl dbl dbl dbl
            { CmdLine (Just $2) ($3,$4,$5) Nothing ($6,$7,$8) Nothing }
        | line str dbl dbl dbl str dbl dbl dbl
            { CmdLine (Just $2) ($3,$4,$5) (Just $6) ($7,$8,$9) Nothing }
        | line str dbl dbl dbl dbl dbl dbl str
            { CmdLine (Just $2) ($3,$4,$5) Nothing ($6,$7,$8) (Just $9) }
        | line str dbl dbl dbl str dbl dbl dbl str
            { CmdLine (Just $2) ($3,$4,$5) (Just $6) ($7,$8,$9) (Just $10) }

        | mesh ':' str
            { CmdMesh Nothing $3 Nothing }
        | mesh str ':' str
            { CmdMesh (Just $2) $4 Nothing }
        | mesh ':' str str
            { CmdMesh Nothing $3 (Just $4) }
        | mesh str ':' str str
            { CmdMesh (Just $2) $4 (Just $5) }

        | move dbl dbl dbl
            { CmdMove ($2,$3,$4) Nothing }
        | move dbl dbl dbl str
            { CmdMove ($2,$3,$4) (Just $5) }
        | scale dbl dbl dbl
            { CmdScale ($2,$3,$4) Nothing }
        | scale dbl dbl dbl str
            { CmdScale ($2,$3,$4) (Just $5) }

        | rotate str dbl        { CmdRotate (axisHelper $2) $3 Nothing }
        | rotate str dbl str    { CmdRotate (axisHelper $2) $3 (Just $4) }
        | push                  { CmdPush }
        | pop                   { CmdPop }
        | display               { CmdDisplay }
        | save str              { CmdSave $2 }

        | light str dbl dbl dbl dbl dbl dbl
            { CmdLight $2 ($3,$4,$5) ($6,$7,$8) }
        | constants str dbl dbl dbl dbl dbl dbl dbl dbl dbl
            { CmdConstants $2 ($3,$4,$5) ($6,$7,$8) ($9,$10,$11) (0,0,0) 10 }
        | constants str dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl
            { CmdConstants $2 ($3,$4,$5) ($6,$7,$8) ($9,$10,$11) (0,0,0) $12 }
        | constants str dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl
            { CmdConstants $2 ($3,$4,$5) ($6,$7,$8) ($9,$10,$11) ($12,$13,$14)
                10 }

        | camera dbl dbl dbl dbl dbl dbl
            { CmdCamera ($2,$3,$4) ($5,$6,$7) }
        | texture str dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl dbl
            { CmdTexture $2 ($3,$4,$5) ($6,$7,$8) ($9,$10,$11) ($12,$13,$14) }

        | set str dbl               { CmdSet $2 $3 }
        | basename str              { CmdBasename $2 }
        | save_knobs str            { CmdSaveKnobs $2 }
        | tween dbl dbl str str     { CmdTween $2 $3 $4 $5 }
        | frames dbl                { CmdFrames $2 }
        | vary str dbl dbl dbl dbl  { CmdVary $2 $3 $4 $5 $6 }
        | shading shading_type      { CmdShading $2 }
        | setknobs dbl              { CmdSetknobs $2 }
        | focal dbl                 { CmdFocal $2 }
        | web                       { CmdWeb }
        | ambient dbl dbl dbl       { CmdAmbient ($2,$3,$4) }
        | generate_rayfiles         { CmdGenerateRayfiles }
        | save_coords str           { CmdSaveCoords $2 }
        
{
type Vec3 = (Double, Double, Double)

data Command
    = CmdSphere MS Vec3 Db MS       --done
    | CmdBox MS Vec3 Vec3 MS        --done
    | CmdTorus MS Vec3 Db Db MS     --done
    | CmdLine MS Vec3 MS Vec3 MS    --done
    | CmdMesh MS String MS
    | CmdMove Vec3 MS       --done
    | CmdScale Vec3 MS      --done
    | CmdRotate Axis Db MS  --done
    | CmdPush   --done
    | CmdPop    --done
    | CmdDisplay        --done
    | CmdSave String    --done
    | CmdLight String Vec3 Vec3
    | CmdConstants String Vec3 Vec3 Vec3 Vec3 Db

    | CmdSaveCoords String
    | CmdCamera Vec3 Vec3
    | CmdTexture String Vec3 Vec3 Vec3 Vec3
    | CmdSet String Db
    | CmdBasename String
    | CmdSaveKnobs String
    | CmdTween Db Db String String
    | CmdFrames Db
    | CmdVary String Db Db Db Db
    | CmdShading String
    | CmdSetknobs Db
    | CmdFocal Db
    | CmdWeb
    | CmdAmbient Vec3
    | CmdGenerateRayfiles
    deriving (Eq, Show)

data Axis = AxisX | AxisY | AxisZ deriving (Show, Eq)

axisHelper :: String -> Axis
axisHelper [] = AxisX
axisHelper (x:xs)
    | x == 'x' || x == 'X'  = AxisX
    | x == 'y' || x == 'Y'  = AxisY
    | x == 'z' || x == 'Z'  = AxisZ
    | otherwise = error $ "incorrect axis \"" ++ (x:xs) ++ "\"...somewhere"

parseError :: [Token] -> a
parseError _ = error "parse error haha"

type Db = Double
type MS = Maybe String

parse :: [Token] -> [Command]
parse = reverse . parseReversed

}
