{$R+}{$Q+}
program project1;

uses
{  Classes, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CastleControl,
  CastleCreatures,
  CastleKeysMouse,}
  SysUtils,
  castle_base, castle_window, CastleWindow,
  castlescene, castlescenecore, castlescenemanager,
  castle3d, castlevectors,  X3DNodes, x3dload,
  castleplayer, castlecameras;

const MaxTilesTypes = 51;

      myscale = 1;

const maxmaxx=15;
      maxmaxy=15;
      maxmaxz=9;
      maxMapTiles=maxmaxx*maxmaxy*maxmaxz;

      show_map_boolean=false;

      Target_Map_Area=maxmaxx*maxmaxy*maxmaxz div 2;

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

// this is a single 1x1 tile
const maxangles = 3; {for rectagonal grid //0..5 for hexagonal}
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

const maxtilesize = 3; {1x1 tiles, todo}
      maxtilesizez = 2; {1 is one floor only as all ladders are 2}
type Map_Tile_type = record
  Tile3D:TX3DRootNode;
  Tile_scene: TCastleScene;
  TileFreeFaces:byte;
  tilesizex,tilesizey,tilesizez:byte;
  Has_Stairs_down:boolean;    // Tile has stairs down (to meet the algorithm)
  blocker:boolean;            // Tile is a generig face blocker.
  TileMap:array[1..maxtilesize,1..maxtilesize,1..maxtilesizez] of Basic_Tile_Type;
end;

var
  Window: TCastleWindow;

  Tiles: array[1..MaxTilesTypes] of Map_Tile_Type;
  Map: array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of Basic_Tile_Type;
  MapTiles: array[1..maxMapTiles] of T3DTransform;

  DistanceMap,olddistancemap:array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of integer;
  vis,chngd: array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of boolean;
  drawall:boolean;
  px,py,pz,oldx,oldy,oldz: integer;
  lasttimer:TDateTime;
  MapArea,deepest_level,max_distance:integer;

  RoseR:TX3DRootNode;
  RoseS:TCastleScene;
  RoseT:T3DTransform;
  RoseTransform: TTransformNode;

  Rosex,Rosey,Rosez:integer;
  RoseFound:boolean;

{  lightR:TX3DRootNode;
  LightS:TCastleScene;
  LightT:T3DTransform;}
  n_tiles:integer;
  maxx,maxy,maxz:byte;

  Player:TPlayer;

  WalkCamera:TWalkCamera;

{  RenderParams: TBasicRenderParams;
  LightNode: TPointLightNode;
  LightInstance: TLightInstance;}

{==========================================================================}
{============================ PROCEDURES ==================================}
{==========================================================================}

procedure LoadTiles;
var i,j:integer;
    ix,iy,iz:integer;
    FreeFaces:byte;
    face_type:byte;
begin
 //clear all tiles data
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

  //STAIRCASEs 1x3
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


  // now prepare 3D part;
  for i:=1 to maxTilesTypes do begin
    Tiles[i].Tile3D:=load3d('DAT'+pathdelim+'models'+pathdelim+inttostr(i)+'.x3d'); //load tile x3d
    Tiles[i].Tile_scene:= TCastleScene.create(Window.sceneManager);
    Tiles[i].Tile_scene.spatial := [ssRendering, ssDynamicCollisions];
    Tiles[i].Tile_scene.processevents:=true;
    Tiles[i].Tile_scene.load(Tiles[i].Tile3D,true);
  end;

  RoseR:=load3d('DAT'+pathdelim+'models'+pathdelim+'Rose.x3d');
  RoseS:=TCastleScene.create(Window.sceneManager);
  RoseS.spatial := [ssRendering];
  RoseS.processevents:=true;
  RoseS.load(RoseR,true);
{  LightR:=load3d('light.x3d');
  LightS:=TCastleScene.create(Window.sceneManager);
  LightS.spatial := [ssRendering];
  LightS.processevents:=true;
  LightS.load(LightR,true);}
end;
{---------------------------------------------------------------------------}
function getMap(gx,gy,gz:integer):Basic_Tile_Type;
begin
  if (gx>0)     and (gy>0)     and (gz>0) and
     (gx<=maxx) and (gy<=maxx) and (gz<=maxz) then
    getMap:=Map[gx,gy,gz]
  else
    GetMap.base:=tile_inacceptible;
end;
{---------------------------------------------------------------------------}
function InverseAngle(inangle:byte):byte;
begin
 case inangle of
   angle_top:inverseAngle:=angle_bottom;
   angle_bottom:inverseAngle:=angle_top;
   angle_left:inverseAngle:=angle_right;
   angle_right:inverseAngle:=angle_left;
 end;
 //todo inverseAngle:=inangle+maxangle/2
end;
function a_dx(angle:byte):shortint;
begin
  a_dx:=0;
  if angle=angle_left then a_dx:=-1 else
  if angle=angle_right then a_dx:=1;
end;
function a_dy(angle:byte):shortint;
begin
  a_dy:=0;
  if angle=angle_top then a_dy:=-1 else
  if angle=angle_bottom then a_dy:=1;
end;
{---------------------------------------------------------------------------}
function CheckTileCompatible(InTile:Basic_Tile_Type;cx,cy,cz:integer):boolean;
var TmpTile:Basic_Tile_Type;
    i:integer;
begin
  CheckTileCompatible:=true;
  if InTile.base<>tile_na then begin
    TmpTile:=getMap(cx,cy,cz);
    if TmpTile.base<>tile_na then checkTileCompatible:=false else begin
      //check current tile if it has some pre-determination
      for i:=0 to maxangles do if (TmpTile.faces[i]<>InTile.faces[i]) and (TmpTile.faces[i]<>face_na) then CheckTileCompatible:=false;
      for i:=1 to 2 do         if (TmpTile.floor[i]<>InTile.floor[i]) and (TmpTile.floor[i]<>floor_na) then CheckTileCompatible:=false;
      if checkTileCompatible then begin
        //if current tile ok, then check adjacent tiles
        for i:=0 to maxangles do begin
          TmpTile:=getMap(cx+a_dx(i),cy+a_dy(i),cz);
          if TmpTile.base<>tile_inacceptible then  //might be optimized at getMap level / todo
            if (TmpTile.faces[inverseAngle(i)]<>InTile.faces[i]) and (TmpTile.faces[inverseAngle(i)]<>face_na) then CheckTileCompatible:=false;
        end;
      end;
    end;
  end;
end;

{---------------------------------------------------------------------------}
function putTile(InTileType,x,y,z:integer):boolean;
var jx,jy,jz,jj:integer;
    TileCanBePlaced:boolean;
begin
 with tiles[InTileType] do begin
  TileCanBePlaced:=true;
  for jx:=1 to tilesizex do
   for jy:=1 to tilesizey do
    for jz:=1 to tilesizez do TileCanBePlaced:=TileCanBePlaced and CheckTileCompatible(TileMap[jx,jy,jz],x+jx-1,y+jy-1,z+jz-1);
  if TileCanBePlaced then begin
//    if InTileType=21 then memo1.lines.add('21!');

    for jx:=1 to tilesizex do
     for jy:=1 to tilesizey do
      for jz:=1 to tilesizez do begin
        if TileMap[jx,jy,jz].base<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].base:=TileMap[jx,jy,jz].base;
        for jj:=0 to maxangles do
          if TileMap[jx,jy,jz].faces[jj]<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].faces[jj]:=TileMap[jx,jy,jz].faces[jj];
        for jj:=1 to 2 do
          if TileMap[jx,jy,jz].floor[jj]<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].floor[jj]:=TileMap[jx,jy,jz].floor[jj];
      end;
    //place 3D tile
    inc(n_tiles);
    MapTiles[n_tiles]:=T3DTransform.Create(Window.SceneManager);
    MapTiles[n_tiles].add(Tiles[inTileType].Tile_scene);
{    MapTiles[n_tiles].add(Lights);}
    MapTiles[n_tiles].translation:=Vector3Single(-2*myscale*(x),-2*myscale*(z),-2*myscale*(y));
    MapTiles[n_tiles].scale:=Vector3Single(myscale,myscale,myscale);
    Window.Scenemanager.items.add(MapTiles[n_tiles]);

{    LightT:=T3DTransform.Create(Window.SceneManager);
    LightT.add(LigthS);
    LightT.translation:=Vector3Single(-2*myscale*(x),-2*myscale*(z),-2*myscale*(y));
    LightT.scale:=Vector3Single(myscale,myscale,myscale);
    Window.Scenemanager.items.add(LightT);}

    PutTile:=true;
  end else PutTile:=false;
 end;
end;

{---------------------------------------------------------------------------}
procedure GenerateMap;
var i,ix,iy,iz:integer;
    tx,ty,tz,ta,tt:integer;
    startx,starty,startz:integer;
    FreeFaces:integer;
    flg:boolean;
    shiftx,shifty,shiftz:integer;
begin
 //memo1.clear;

 n_tiles:=0;
 randomize;
 //erase map;
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do with Map[ix,iy,iz] do begin
     base:=tile_na;
     for i:=0 to maxangles do faces[i]:=face_na;
     if ix=1 then faces[angle_left]:=face_wall;
     if iy=1 then faces[angle_top]:=face_wall;
     if ix=maxx then faces[angle_right]:=face_wall;
     if iy=maxy then faces[angle_bottom]:=face_wall;
     if iz=maxz then floor[angle_stairs_down]:=floor_wall else floor[angle_stairs_down]:=floor_na;
     if iz=1 then floor[angle_stairs_up]:=floor_wall else floor[angle_stairs_up]:=floor_na;
   end;

 //make entrance;
 startx:=maxx div 2;
 starty:=maxy div 2;
 startz:=1;

 deepest_level:=0;

 PutTile(3,startx,starty,startz);

 //now for the generation
 repeat
   // Calculate remaining Free Faces
   FreeFaces:=0;
   MapArea:=0;
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     for iz:=1 to maxz do begin
      for i:=0 to maxangles do begin
        if (Map[ix,iy,iz].faces[i]>=face_free) and (getMap(ix+a_dx(i),iy+a_dy(i),iz).faces[inverseAngle(i)]=face_na) then begin
          inc(FreeFaces);
          tx:=ix;   //store the latest face
          ty:=iy;
          tz:=iz;
          ta:=i;
          if tz>deepest_level then deepest_level:=tz;
        end;
      end;
      if Map[ix,iy,iz].base=tile_free then inc(MapArea);
     end;
   If FreeFaces>0 then begin
     if FreeFaces>3 then begin
       //seek random free face, else - use last one at tx,ty,tz,ta
       flg:=false;
       repeat
         tx:=round(random*(maxx-1))+1;
         ty:=round(random*(maxy-1))+1;
         tz:=round(random*(maxz-1))+1;
         ta:=round(random*maxangles);
         if (GetMap(tx,ty,tz).faces[ta]>=face_free) and (GetMap(tx+a_dx(ta),ty+a_dy(ta),tz).faces[inverseAngle(ta)]=face_na) then flg:=true;
       until flg;
     end;
     //try to place a random tile at the face exit
     flg:=false;
     Repeat

       //select tile to place (try to) * demand more or less free faces at the map
       tt:=round(random*(MaxTilesTypes-1))+1;
       if (deepest_Level<maxz) and (tz=deepest_level) and (deepest_level<3*MapArea/Target_Map_area*maxz) then
         repeat
           tt:=round(random*(MaxTilesTypes-1))+1;
         until (Tiles[tt].has_stairs_down) or (random<0.1);

       if (FreeFaces>9) or (MapArea>target_Map_area) then
         repeat
           tt:=round(random*(MaxTilesTypes-1))+1;
         until (Tiles[tt].TileFreeFaces<3) or (random<0.1)
       else begin
         if (FreeFaces<4) then
           repeat
             tt:=round(random*(MaxTilesTypes-1))+1;
           until (Tiles[tt].TileFreeFaces>2) or (random<0.1);

         if (Tiles[tt].TileFreeFaces<3) and (MapArea<target_map_area) then
           repeat
             tt:=round(random*(MaxTilesTypes-1))+1;
           until (Tiles[tt].TileFreeFaces>=2) or (random<0.001)
       end;

       // now get compatible face coordinates shift of tt in case >1x1 !!!
       if Tiles[tt].TileSizex+Tiles[tt].TileSizey+Tiles[tt].TileSizez>3 then begin
         //todo: optimize!!!
         // place large tile
         repeat
           shiftx:=-round((random)*(Tiles[tt].TileSizex-1));
           shifty:=-round((random)*(Tiles[tt].TileSizey-1));
           shiftz:=-round((random)*(Tiles[tt].TileSizez-1));
           flg:=PutTile(tt,tx+a_dx(ta)+shiftx,ty+a_dy(ta)+shifty,tz+shiftz);
         until (flg) or (random<1/(Tiles[tt].TileSizex+Tiles[tt].TileSizey+Tiles[tt].TileSizez));
       end else begin
         //simple place small tile 1x1x1
         if not tiles[tt].blocker then
           flg:=PutTile(tt,tx+a_dx(ta),ty+a_dy(ta),tz);
       end;
     Until flg or (random<0.001);
     // if impossible to place any tile... block the face out
     if not flg then begin
       repeat
         tt:=round(random*(MaxTilesTypes-1))+1;
         if Tiles[tt].blocker then begin
           if tiles[tt].TileMap[1,1,1].faces[inverseAngle(ta)]=Map[tx,ty,tz].faces[ta] then begin
             flg:=PutTile(tt,tx+a_dx(ta),ty+a_dy(ta),tz);
             if flg then begin
               Map[tx,ty,tz].faces[ta]:=face_wall;
               Map[tx+a_dx(ta),ty+a_dy(ta),tz].faces[inverseAngle(ta)]:=face_wall;
             end;
           end;
         end;
       until flg or (random<0.01);
     end;
   end;
 until (freeFaces=0){ or (random<0.001)};

 //memo1.lines.add('Map Area = '+inttostr(MapArea)+'/'+inttostr(Target_Map_Area));
 //if MapArea<Target_Map_Area then memo1.lines.add('Target failed by '+inttostr(round((Target_Map_Area-MapArea)/Target_Map_Area*100))+'%') else
 //                                memo1.lines.add('Target met with excess of '+inttostr(round((MapArea-Target_Map_Area)/Target_Map_Area*100))+'%');
 //memo1.lines.add('Deepest level = '+inttostr(Deepest_level)+'/'+inttostr(Maxz));

 //create distance map;
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do distanceMap[ix,iy,iz]:=-1;
 distanceMap[startx,starty,startz]:=0;
 repeat
   flg:=true;
   olddistancemap:=distancemap;
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     for iz:=1 to maxz do if {((Map[ix,iy,iz].base=tile_free) or (Map[ix,iy,iz].base=tile_stairs_up) or (Map[ix,iy,iz].base=tile_stairs_down)) and} (olddistanceMap[ix,iy,iz]>-1) then begin
       for i:=0 to maxAngles do if (Map[ix,iy,iz].faces[i]<>face_wall) then
         if (ix+a_dx(i)>0) and (ix+a_dx(i)<=maxx) and (iy+a_dy(i)>0) and (iy+a_dy(i)<=maxy) then
           if (distancemap[ix+a_dx(i),iy+a_dy(i),iz]=-1) then begin
             distancemap[ix+a_dx(i),iy+a_dy(i),iz]:=olddistanceMap[ix,iy,iz]+1;
             flg:=false;
           end;
       if (Map[ix,iy,iz].base=tile_stairs_up) then
         if distancemap[ix,iy,iz-1]=-1 then begin
           distancemap[ix,iy,iz-1]:=olddistanceMap[ix,iy,iz]+1;
           flg:=false;
         end;
       if (Map[ix,iy,iz].base=tile_stairs_down) then
         if distancemap[ix,iy,iz+1]=-1 then begin
           distancemap[ix,iy,iz+1]:=olddistanceMap[ix,iy,iz]+1;
           flg:=false;
         end;
     end;
 until flg;
 max_distance:=0;
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do if (distanceMap[ix,iy,iz]>max_distance) then begin
     tx:=ix;
     ty:=iy;
     tz:=iz;
     max_Distance:=distanceMap[ix,iy,iz];
   end;
 //memo1.lines.add('Max distance = '+inttostr(Max_distance));
// memo1.lines.add('at '+inttostr(tx)+';'+inttostr(ty)+';'+inttostr(tz));
     RoseT:=T3DTransform.Create(Window.SceneManager);
     RoseT.add(RoseS);
     RoseT.translation:=Vector3Single(-2*myscale*(tx),-2*myscale*(tz),-2*myscale*(ty));
     RoseT.scale:=Vector3Single(myscale,myscale,myscale);
     Window.Scenemanager.items.add(RoseT);
     Rosex:=tx;
     Rosey:=ty;
     Rosez:=tz;
     RoseFound:=false;
//     RoseTransform := RoseS.RootNode.FindNodeByName(TTransformNode,'Cylinder_TRANSFORM', true) as TTransformNode;

 //clear visible
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do begin
     vis[ix,iy,iz]:=show_map_boolean;
     chngd[ix,iy,iz]:=true;
   end;
 oldx:=-1;
end;



{==========================================================================}
{================================= MAIN ===================================}
{==========================================================================}
begin
  Window := TCastleWindow.Create(Application);

  maxx:=maxmaxx;
  maxy:=maxmaxy;
  maxz:=maxmaxz;
  LoadTiles;
  GenerateMap;

  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;
  player.Camera.MouseLook:=true;
{   player.camera.Input_Jump.assign(K_Space);
  player.camera.JumpMaxHeight:=1;
  player.camera.JumpTime:=1;}
  player.DefaultPreferredHeight:=1;
  player.DefaultMoveHorizontalSpeed:=3;
  player.Camera.MouseLookHorizontalSensitivity:=0.5;
  player.Camera.MouseLookVerticalSensitivity:=0.5;
  player.position:=Vector3Single(-2*myscale*(maxx div 2),-2*myscale+(player.DefaultPreferredHeight),-2*myscale*(maxy div 2));
  player.camera.FallingEffect:=false;
  Window.scenemanager.camera:=player.camera;

{ WalkCamera:=TWalkCamera.create(Window.SceneManager);
WalkCamera.Init(Vector3Single(-2*myscale*(maxx div 2),-2*myscale+1,-2*myscale*(maxy div 2)),vector3single(0,0,-1),vector3single(0,1,0),vector3single(0,1,0),2,0.1);
WalkCamera.mouselook:=true;
//WalkCamera.MoveHorizontalSpeed:=5;
Window.scenemanager.camera:=WalkCamera;  }

{  LightNode := TPointLightNode.Create('', '');
LightNode.FdLocation.Value := player.position;

LightInstance.Node := LightNode;
LightInstance.Transform := IdentityMatrix4Single;
LightInstance.TransformScale := 1;
LightInstance.Location := player.position;
LightInstance.Radius := 1000;

RenderParams := TBasicRenderParams.Create;
RenderParams.FBaseLights.Add(LightInstance);}

//DrawMap;


  Window.Open;
  Application.Run;
end.

