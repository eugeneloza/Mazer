unit Tile_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  castlescene, castleimages, castlescenecore,
  generic_var;

const MaxTilesTypes = 51;

// constants for tiles and faces

const face_na = 0;
const face_wall = 1;
      face_free = 2; // and >2 are face types


 const tile_na = 0;
 const tile_free = 2;
       //tile_wall = 1;   //?unneeded?
       Tile_Stairs_Down = 254;
       Tile_Stairs_Up = 253;

       tile_inacceptible = 255;

const floor_na = 0;
const floor_wall = 1;
      floor_free = 2;

// 1x1x1 tile type (subtype) declaration
const maxangles = 3; {0..3 for rectagonal grid //0..5 for hexagonal}
      angle_top = 1;
      angle_bottom = 3;
      angle_left = 2;
      angle_right = 0;
      angle_stairs_up = 1;   //for floor
      angle_stairs_down = 2;

type Basic_Tile_type = record
  base:byte; //this tile basic state
  faces:array[0..maxangles] of byte; //each tile face state //redundant 2x
  floor:array[1..2] of byte; // at this moment each tile has top and bottom only
end;

const maxtilesize = 3;   {max tile size (square tiles)}
      maxtilesizez = 2;   {max tile height}
type Map_Tile_type = record
//  Tile3D: TX3DRootNode;
  Tile_scene: TCastleScene;
  Tile_PNG: array[1..maxtilesizez] of TCastleImage;
  TileFreeFaces:byte;
  tilesizex,tilesizey,tilesizez:byte;
  Has_Stairs_down:boolean;    // Tile has stairs down (to meet the generation algorithm, requiring sometimes ladders to go down)
  blocker:boolean;            // Tile is a generic face blocker.
  TileMap:array[1..maxtilesize,1..maxtilesize,1..maxtilesizez] of Basic_Tile_Type;
end;

var Tiles: array[1..MaxTilesTypes] of Map_Tile_Type;
  RoseS:TCastleScene;


procedure LoadTiles;

implementation

procedure GenerateTileMaps;
var i,j:integer;
    ix,iy,iz:integer;
    face_type:byte;
begin
 //reset 1-15 tiles data
  for i:=1 to 15 do with Tiles[i] do begin
    tilesizex:=1;
    tilesizey:=1;
    tilesizez:=1;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;

  {********************** CASTLE *****************************}

  face_type:=0;

  //deadends
  Tiles[1].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  Tiles[2].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  Tiles[3].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  Tiles[4].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  //linear passages
  tiles[5].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[5].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;

  tiles[6].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  tiles[6].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  //crossroad
  tiles[7].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[7].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[7].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  tiles[7].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  //T-shapes
  tiles[8].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[8].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  tiles[8].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  tiles[9].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[9].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  tiles[9].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  tiles[10].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[10].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[10].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  tiles[11].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[11].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[11].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

  //rotations
  tiles[12].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[12].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

  tiles[13].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  tiles[13].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  tiles[14].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[14].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

  tiles[15].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  tiles[15].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

  //STAIRCASEs 1x3x2
  with Tiles[16] do begin
    tilesizex:=1;
    tilesizey:=3;
    tilesizez:=2;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[1,2,1] do begin
      base:=tile_Stairs_Down;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[1,3,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[1,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[1,2,2] do begin
      base:=tile_Stairs_Up;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[1,3,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;

  with Tiles[17] do begin
    tilesizex:=1;
    tilesizey:=3;
    tilesizez:=2;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[1,2,1] do begin
      base:=tile_Stairs_Down;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[1,3,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;

    with TileMap[1,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[1,2,2] do begin
      base:=tile_Stairs_Up;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      faces[angle_bottom]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[1,3,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_top]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;

  with Tiles[18] do begin
    tilesizex:=3;
    tilesizey:=1;
    tilesizez:=2;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[2,1,1] do begin
      base:=tile_Stairs_Down;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[3,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;

    with TileMap[1,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[2,1,2] do begin
      base:=tile_Stairs_Up;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[3,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;

  with Tiles[19] do begin
    tilesizex:=3;
    tilesizey:=1;
    tilesizez:=2;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[2,1,1] do begin
      base:=tile_Stairs_Down;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;
    with TileMap[3,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_free;
    end;

    with TileMap[1,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[2,1,2] do begin
      base:=tile_Stairs_Up;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      faces[angle_right]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
    with TileMap[3,1,2] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      faces[angle_left]:=face_free+face_type;
      floor[angle_stairs_up]:=floor_free;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;

  with Tiles[20] do begin
    tilesizex:=3;
    tilesizey:=3;
    tilesizez:=2;
    for ix:=1 to 3 do
     for iy:=1 to 3 do
      for iz:=1 to 2 do with tilemap[ix,iy,iz] do begin
        base:=tile_free;
        for j:=0 to maxangles do faces[j]:=face_free+face_type;
        if ix=1 then faces[angle_left]:=face_wall;
        if ix=3 then faces[angle_right]:=face_wall;
        if iy=1 then faces[angle_top]:=face_wall;
        if iy=3 then faces[angle_bottom]:=face_wall;
        if iz=1 then floor[angle_stairs_up]:=floor_wall else floor[angle_stairs_up]:=floor_free;
        if iz=2 then floor[angle_stairs_down]:=floor_wall else floor[angle_stairs_down]:=floor_free;
      end;

      TileMap[2,1,2].faces[angle_top]:=face_free+face_type;
      TileMap[1,2,2].faces[angle_left]:=face_free+face_type;
      TileMap[3,2,2].faces[angle_right]:=face_free+face_type;
      TileMap[1,3,2].faces[angle_bottom]:=face_free+face_type;
      TileMap[3,3,2].faces[angle_bottom]:=face_free+face_type;
  end;

{************** MAUSOLEUM ************************************}

face_type:=1;
//clear all tiles data
 for i:=21 to 35 do with Tiles[i] do begin
   tilesizex:=1;
   tilesizey:=1;
   tilesizez:=1;
   with TileMap[1,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_wall;
   end;
 end;

 //deadends
 Tiles[21].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 Tiles[22].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
 Tiles[23].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 Tiles[24].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 //linear passages
 tiles[25].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[25].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;

 tiles[26].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
 tiles[26].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 //crossroad
 tiles[27].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[27].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[27].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
 tiles[27].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 //T-shapes
 tiles[28].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[28].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
 tiles[28].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 tiles[29].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[29].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
 tiles[29].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 tiles[30].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[30].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[30].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 tiles[31].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[31].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[31].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

 //rotations
 tiles[32].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[32].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

 tiles[33].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
 tiles[33].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 tiles[34].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[34].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;

 tiles[35].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
 tiles[35].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

 //STAIRCASEs 1x3
 with Tiles[36] do begin
   tilesizex:=1;
   tilesizey:=3;
   tilesizez:=2;
   with TileMap[1,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[1,2,1] do begin
     base:=tile_Stairs_Down;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[1,3,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[1,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[1,2,2] do begin
     base:=tile_Stairs_Up;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[1,3,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
 end;

 with Tiles[37] do begin
   tilesizex:=1;
   tilesizey:=3;
   tilesizez:=2;
   with TileMap[1,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[1,2,1] do begin
     base:=tile_Stairs_Down;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[1,3,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;

   with TileMap[1,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[1,2,2] do begin
     base:=tile_Stairs_Up;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     faces[angle_bottom]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[1,3,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_top]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
 end;

 with Tiles[38] do begin
   tilesizex:=3;
   tilesizey:=1;
   tilesizez:=2;
   with TileMap[1,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[2,1,1] do begin
     base:=tile_Stairs_Down;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[3,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;

   with TileMap[1,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[2,1,2] do begin
     base:=tile_Stairs_Up;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[3,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
 end;

 with Tiles[39] do begin
   tilesizex:=3;
   tilesizey:=1;
   tilesizez:=2;
   with TileMap[1,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[2,1,1] do begin
     base:=tile_Stairs_Down;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;
   with TileMap[3,1,1] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_wall;
     floor[angle_stairs_down]:=floor_free;
   end;

   with TileMap[1,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[2,1,2] do begin
     base:=tile_Stairs_Up;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     faces[angle_right]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
   with TileMap[3,1,2] do begin
     base:=tile_free;
     for j:=0 to maxangles do faces[j]:=face_wall;
     faces[angle_left]:=face_free+face_type;
     floor[angle_stairs_up]:=floor_free;
     floor[angle_stairs_down]:=floor_wall;
   end;
 end;

 {********* MAUSOLEUM-CASTLE adapter ******************}
 //clear all tiles data
  for i:=40 to 43 do with Tiles[i] do begin
    tilesizex:=1;
    tilesizey:=1;
    tilesizez:=1;
    with TileMap[1,1,1] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_wall;
      floor[angle_stairs_up]:=floor_wall;
      floor[angle_stairs_down]:=floor_wall;
    end;
  end;
 tiles[40].TileMap[1,1,1].faces[angle_top]:=face_free+1;
 tiles[40].TileMap[1,1,1].faces[angle_bottom]:=face_free+0;

 tiles[42].TileMap[1,1,1].faces[angle_top]:=face_free+0;
 tiles[42].TileMap[1,1,1].faces[angle_bottom]:=face_free+1;

 tiles[41].TileMap[1,1,1].faces[angle_left]:=face_free+0;
 tiles[41].TileMap[1,1,1].faces[angle_right]:=face_free+1;

 tiles[43].TileMap[1,1,1].faces[angle_right]:=face_free+0;
 tiles[43].TileMap[1,1,1].faces[angle_left]:=face_free+1;

{****************MAUSOLEUM-CASTLE blockers ****************************}

 //clear all tiles data
  for i:=44 to 51 do with Tiles[i] do begin
    tilesizex:=1;
    tilesizey:=1;
    tilesizez:=1;
    with TileMap[1,1,1] do begin
      base:=tile_na;
      for j:=0 to maxangles do faces[j]:=face_na;
      floor[angle_stairs_up]:=floor_na;
      floor[angle_stairs_down]:=floor_na;
    end;
  end;

  //deadends
  face_type:=0;
  Tiles[44].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  Tiles[47].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  Tiles[46].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  Tiles[45].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;
  face_type:=1;
  Tiles[48].TileMap[1,1,1].faces[angle_top]:=face_free+face_type;
  Tiles[51].TileMap[1,1,1].faces[angle_left]:=face_free+face_type;
  Tiles[50].TileMap[1,1,1].faces[angle_bottom]:=face_free+face_type;
  Tiles[49].TileMap[1,1,1].faces[angle_right]:=face_free+face_type;

{*****************************************************}

end;

Procedure CalculateTileFaces;
var i,j,ix,iy,iz:integer;
    FreeFaces:byte;
begin
   //calculate tile's free faces for later use
  for i:=1 to MaxTilesTypes do with Tiles[i] do begin
    FreeFaces:=0;
    for ix:=1 to tilesizex do
     for iy:=1 to tilesizey do
      for iz:=1 to tilesizez do
       for j:=0 to maxangles do if TileMap[ix,iy,iz].faces[j]>=face_free then inc(FreeFaces);
    TileFreeFaces:=FreeFaces;
    //check if it's a blocker tile
    if (FreeFaces=1) and (tilesizex+tilesizey+tilesizez=3) and (TileMap[1,1,1].base=tile_na) then      blocker:=true    else      blocker:=false;
    //check if it has stairs down for later generation
    has_stairs_down:=false;
    for ix:=1 to tilesizex do
     for iy:=1 to tilesizey do
      for iz:=1 to tilesizez do if TileMap[ix,iy,iz].base=tile_stairs_down then has_stairs_down:=true;
  end;
end;

Procedure LoadTiles;
var i,j:integer;
begin

 GenerateTileMaps;
 CalculateTileFaces;
  // now prepare 3D part + tile map view
  for i:=1 to maxTilesTypes do begin
    Tiles[i].Tile_scene:= TCastleScene.create(Window.sceneManager);
    Tiles[i].Tile_scene.spatial := [ssRendering, ssDynamicCollisions];
    Tiles[i].Tile_scene.processevents:=true;
    Tiles[i].Tile_scene.load(models_folder+inttostr(i)+'.x3d');
//    Tiles[i].Tile3D:=Load3D(models_folder+inttostr(i)+'.x3d');

    //load png tile for the mini-map
    if Tiles[i].tilesizez=1 then
      try
        Tiles[i].Tile_PNG[1]:=LoadImage(models_folder+inttostr(i)+'.png')
      except
        Tiles[i].Tile_PNG[1]:=nil
      end
    else
    for j:=1 to Tiles[i].tilesizez do
          try
            Tiles[i].Tile_PNG[j]:=LoadImage(models_folder+inttostr(i)+'_'+inttostr(j)+'.png');
{            tiles[i].Tile_PNG[j]:=(tiles[i].Tile_PNG[j] as TRGBImage).ToRGBAlphaImage;
            (Tiles[i].Tile_PNG[j] as TRGBAlphaImage).clearAlpha(255); }
//            tiles[i].Tile_PNG[j].setsize(16,16,1);
          except
            Tiles[i].Tile_PNG[j]:=nil;
          end;
  end;

  RoseS:=TCastleScene.create(Window.sceneManager);
  RoseS.spatial := [ssRendering];
  RoseS.processevents:=true;
  RoseS.load(models_folder+'Rose.x3d');
end;


end.
