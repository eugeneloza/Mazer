{Copyright (C) 2015-2016 Yevhen Loza

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.}

unit Generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  castle_base, CastleWindow,
  castlescene, castlescenecore, castlescenemanager,
  castle3d, castlevectors,  X3DNodes, CastleImages,
  {castlerenderer,}
  Tile_var, generic_var, MazerMapParser;

const allow_error=0.10; {target fail allow}

const digit_accuracy=1e-6; //accuracy for single (7/8 digits accuracy) rounding-ups, yeah, its ugly, I know.
      //also have to keep track that random<digit_accuracy or 1-random<digit_accuracy will lead to a bug.

const MaxLODs=2; // maximal number of LODs

type Generator_type = record
  tile_type:integer;
  tx,ty,tz:integer;
end;

type MapIntArray = array [1..maxmaxx,1..maxmaxy,1..maxmaxz] of integer;
type GeneratorStepsArray = array[0..maxMapTiles] of Generator_type;
type MapArray =array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of Basic_Tile_Type;
type NeighboursArray = array [1..maxmaxx,1..maxmaxy,1..maxmaxz] of array [0..maxMapTiles] of integer;

var n_tiles:integer;

    MapTiles: array of TTransformNode;
    {$ifdef UseSwitches}
    MapSwitches: array of TSwitchNode;
    MapTilesLOD: array of TTransformNode;
    MaxNeighbours, currentMaxNeighbours: integer;
    {$else}
    {$endif}
    //TODO: dynamic array, however, I'm not yet sure, how to make it without resizing it at every placeholder add
    MapPlaceholders: array[1..maxMapPlaceholders] of TtransformNode;
    mapPlaceholderElements:integer;
    //these are larger groups for tiles (to optimize FPS and other stuff)
    {$ifdef TryCastleScenes}
    MapScenes: array of TCastleScene;
    MapRoots:array of TX3DRootNode;
    {$else}
    MapGroups: array[1..maxGroups] of TStaticGroupNode;
    GroupsSwitches: array[1..maxGroups] of TSwitchNode;

    MainRoot: TX3DRootNode;
    MainScene: TCastleScene;
    {$endif}
    Nav:TKambiNavigationInfoNode; /// !!!
    NavLight:TPointLightNode;

    MapTileIndex:^MapIntArray; //here we store all tiles numbers at specific xyz; used to quicken search for neighbours + debug TileLabel in-game

    //TODO: optimize memory usage. However these are TIME CRITICAL ARRAYS
    Neighbours: ^NeighboursArray; //this stores all neigbours with all 'distance' relation for LODs
    groups: array [0..maxMapTiles] of integer;//stores group numbers for quick access
    N_groups: integer;

    Map: ^MapArray;
    GeneratorSteps: ^GeneratorStepsArray;

    MapArea,explored_area,deepest_level,max_distance:integer;
    startx,starty,startz:integer;

    RoseT:T3DTRansform;
    Rosex,Rosey,Rosez:integer;
    RoseFound:boolean;

    Minimap:array[1..maxmaxz] of TCastleImage;


procedure MakeMap;
function InverseAngle(inangle:byte):byte;
function a_dx(angle:byte):shortint;
function a_dy(angle:byte):shortint;

implementation

{---------------------------------------------------------------------------}
//basic get map procedure. Simply does all routine checking that the gx,gy,gz is not out of bounds.
function getMap(gx,gy,gz:integer):Basic_Tile_Type;
begin
  if (gx>0)     and (gy>0)     and (gz>0) and
     (gx<=maxx) and (gy<=maxx) and (gz<=maxz) then
    getMap:=Map^[gx,gy,gz]
  else
    GetMap.base:=tile_inacceptible;
end;
{---------------------------------------------------------------------------}
// get inverse angle for the incoming angle to match the faces
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

// x and y shift determined by incoming angle
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
// The most important sub-procedure of the algorithm
// It compares incoming 1x1x1 base tiles to the Map.
// Returns false if tile mismatches Map
// Returns True if tile matches Map and can be placed
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
// This is a procedure to put the tile at x,y,z
// returns true if tile is put successfully and false if the tile cannot be placed at these coordinates
function putTile(InTileType,x,y,z:integer):boolean;
var jx,jy,jz,jj:integer;
    TileCanBePlaced:boolean;
begin
 with tiles[InTileType] do begin
  TileCanBePlaced:=true;
  //check all tiles against map area they are placed to
  for jx:=1 to tilesizex do
   for jy:=1 to tilesizey do
    for jz:=1 to tilesizez do TileCanBePlaced:=TileCanBePlaced and CheckTileCompatible(TileMap[jx,jy,jz],x+jx-1,y+jy-1,z+jz-1);
  if TileCanBePlaced then begin
//    if InTileType=21 then memo1.lines.add('21!');
    // if tile can be placed - then place it
    for jx:=1 to tilesizex do
     for jy:=1 to tilesizey do
      for jz:=1 to tilesizez do begin
        if TileMap[jx,jy,jz].base<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].base:=TileMap[jx,jy,jz].base;
        for jj:=0 to maxangles do
          if TileMap[jx,jy,jz].faces[jj]<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].faces[jj]:=TileMap[jx,jy,jz].faces[jj];
        for jj:=1 to 2 do
          if TileMap[jx,jy,jz].floor[jj]<>tile_na then Map^[x+jx-1,y+jy-1,z+jz-1].floor[jj]:=TileMap[jx,jy,jz].floor[jj];
      end;
    // Prepare a Generator step for placing a tile
    // to create a 3D world later
    inc(n_tiles);
    GeneratorSteps^[n_tiles].Tile_Type:=inTileType;
    GeneratorSteps^[n_tiles].tx:=x;
    GeneratorSteps^[n_tiles].ty:=y;
    GeneratorSteps^[n_tiles].tz:=z;
    PutTile:=true;
  end else PutTile:=false;
 end;
end;

{----------------------------------------------------------------------------------}

//empty the map to begin working with
Procedure EraseMap;
var i,ix,iy,iz:integer;
begin
 n_tiles:=-1;
 //erase map;
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do with Map^[ix,iy,iz] do begin
     base:=tile_na;
     for i:=0 to maxangles do faces[i]:=face_na;
     if ix=1 then faces[angle_left]:=face_wall;
     if iy=1 then faces[angle_top]:=face_wall;
     if ix=maxx then faces[angle_right]:=face_wall;
     if iy=maxy then faces[angle_bottom]:=face_wall;
     if iz=maxz then floor[angle_stairs_down]:=floor_wall else floor[angle_stairs_down]:=floor_na;
     if iz=1 then floor[angle_stairs_up]:=floor_wall else floor[angle_stairs_up]:=floor_na;
   end;
end;

{------------------------------------------------------------------------}

//This is the core procedure of all the algorithm.
//The algorithm is following:
//- pick a place on the map, which has adjacent free face
//- scan randomly through tiles to see if any fits here
//- if correct tile found, place it (add to GeneratorSteps) and repeat until finished
//also some tests for the map to meet certain requirements are made
Procedure CreateMap;
var i,ix,iy,iz:integer;
    tx,ty,tz,ta,tt:integer;
    FreeFaces:integer;
    flg:boolean;
    shiftx,shifty,shiftz:integer;
begin
 deepest_level:=0;
 i:=0;
 repeat
   inc(i);
 until Tiles[i].TileName='library4_03_D.x3d';
 PutTile(i,startx,starty,startz);

 //now for the generation
 repeat
   // Calculate remaining Free Faces
   FreeFaces:=0;
   MapArea:=0;
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     for iz:=1 to maxz do begin
      for i:=0 to maxangles do begin
        if (Map^[ix,iy,iz].faces[i]>=face_free) and (getMap(ix+a_dx(i),iy+a_dy(i),iz).faces[inverseAngle(i)]=face_na) then begin
          inc(FreeFaces);
          tx:=ix;   //store the latest face
          ty:=iy;
          tz:=iz;
          ta:=i;
          if tz>deepest_level then deepest_level:=tz;
        end;
      end;
      if Map^[ix,iy,iz].base>=tile_free then inc(MapArea);
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

     //try to place a random tile at a free face exit
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
         until (flg) or (random<1/(sqr(Tiles[tt].TileSizex)+sqr(Tiles[tt].TileSizey)+sqr(Tiles[tt].TileSizez)));
       end else begin
         //simple place small tile 1x1x1
         if not tiles[tt].blocker then
           flg:=PutTile(tt,tx+a_dx(ta),ty+a_dy(ta),tz);
       end;
     Until flg or (random<0.001/maxTilesTypes);
     // if impossible to place any tile... block the face out
     if not flg then begin
       repeat
         tt:=round(random*(MaxTilesTypes-1))+1;
         if Tiles[tt].blocker then begin
           if tiles[tt].TileMap[1,1,1].faces[inverseAngle(ta)]=Map^[tx,ty,tz].faces[ta] then begin
             flg:=PutTile(tt,tx+a_dx(ta),ty+a_dy(ta),tz);
             if flg then begin
               Map^[tx,ty,tz].faces[ta]:=face_wall;
               Map^[tx+a_dx(ta),ty+a_dy(ta),tz].faces[inverseAngle(ta)]:=face_wall;
             end;
           end;
         end;
       until flg or (random<0.01);
     end;
   end;
 until (freeFaces=0){ or (random<0.001)};
end;

{------------------------------------------------------------------------}

//very basic pathfinding test. For now it just scans for the greatest distance from the entrance to put a rose there.
Procedure CreateDistanceMap;
var i,ix,iy,iz:integer;
    tx,ty,tz:integer;
    flg:boolean;
    DistanceMap,olddistancemap:^MapIntArray; //memoptimize
begin
New(DistanceMap);
New(OlddistanceMap);
 //create distance map; basic for pathfinding
 // now only used to place a rose at the most distant place of the map
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do distanceMap^[ix,iy,iz]:=-1;
 distanceMap^[startx,starty,startz]:=0;
 repeat
   flg:=true;
   olddistancemap^:=distancemap^;
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     for iz:=1 to maxz do if {((Map[ix,iy,iz].base=tile_free) or (Map[ix,iy,iz].base=tile_stairs_up) or (Map[ix,iy,iz].base=tile_stairs_down)) and} (olddistanceMap^[ix,iy,iz]>-1) then begin
       for i:=0 to maxAngles do if (Map^[ix,iy,iz].faces[i]<>face_wall) then
         if (ix+a_dx(i)>0) and (ix+a_dx(i)<=maxx) and (iy+a_dy(i)>0) and (iy+a_dy(i)<=maxy) then
           if (distancemap^[ix+a_dx(i),iy+a_dy(i),iz]=-1) then begin
             distancemap^[ix+a_dx(i),iy+a_dy(i),iz]:=olddistanceMap^[ix,iy,iz]+1;
             flg:=false;
           end;
       if (Map^[ix,iy,iz].base=tile_stairs_up) then
         if distancemap^[ix,iy,iz-1]=-1 then begin
           distancemap^[ix,iy,iz-1]:=olddistanceMap^[ix,iy,iz]+1;
           flg:=false;
         end;
       if (Map^[ix,iy,iz].base=tile_stairs_down) then
         if distancemap^[ix,iy,iz+1]=-1 then begin
           distancemap^[ix,iy,iz+1]:=olddistanceMap^[ix,iy,iz]+1;
           flg:=false;
         end;
     end;
 until flg;
 dispose(OlddistanceMap);

 //now find the max distance
max_distance:=0;
for ix:=1 to maxx do
for iy:=1 to maxy do
 for iz:=1 to maxz do if (distanceMap^[ix,iy,iz]>max_distance) then begin
   tx:=ix;
   ty:=iy;
   tz:=iz;
   max_Distance:=distanceMap^[ix,iy,iz];
 end;
dispose(DistanceMap);

{if tx=0 then begin}
  Rosex:=tx;
  Rosey:=ty;
  Rosez:=tz;
{end else begin
  {if error}
  Rosex:=1;
  Rosey:=1;
  Rosez:=1;
end;}
writeln(tx,' ',ty,' ',tz);

end;

{------------------------------------------------------------------------}

//this procedure creates a set of TCastleImages for each z -> minimap of the level;
//later we just pick up pieces of it and copy it into minimap image
Procedure MakeMinimap;
var i,j,iz:integer;
begin
//Draw Minimap
  for iz:=1 to maxz do begin
    minimap[iz]:=LoadImage(etc_Models_Folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;       //BUG: it's the only way I could initialize TCastleImage...????????
    minimap[iz].setsize((maxx)*16,(maxy)*16,1);
    minimap[iz].Clear(Vector4Byte(0,0,0,0));
//    writeln('drawing '+inttostr(iz));
    for i:=0 to n_tiles do with Tiles[GeneratorSteps^[i].Tile_Type] do
     for j:=1 to tilesizez do
       if GeneratorSteps^[i].tz+j-1=iz then begin
         if Tile_PNG[j]<>nil then
           try
             //writeln('draw: '+inttostr(GeneratorSteps^[i].Tile_Type));
             //!!!
             if Tiles[GeneratorSteps^[i].Tile_Type].TileName='library4_20_R.x3d' then writeln('drawing library4_20_R.x3d');
             //if (GeneratorSteps^[i].Tile_Type<16) or (GeneratorSteps^[i].Tile_Type=20) then
             if not blocker then
               Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps^[i].tx-1)*16,(maxy-GeneratorSteps^[i].ty-tilesizey+1)*16,dmBlendSmart)
             else begin
               if Tiles[GeneratorSteps^[i].Tile_Type].TileMap[1,1,1].faces[angle_top]>=face_free then
                 Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps^[i].tx-1)*16,(maxy-GeneratorSteps^[i].ty-tilesizey+1+1)*16,dmBlendSmart)
               else
               if Tiles[GeneratorSteps^[i].Tile_Type].TileMap[1,1,1].faces[angle_bottom]>=face_free then
                 Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps^[i].tx-1)*16,(maxy-GeneratorSteps^[i].ty-tilesizey+1-1)*16,dmBlendSmart)
               else
               if Tiles[GeneratorSteps^[i].Tile_Type].TileMap[1,1,1].faces[angle_left]>=face_free then
                 Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps^[i].tx-1-1)*16,(maxy-GeneratorSteps^[i].ty-tilesizey+1)*16,dmBlendSmart)
               else
               if Tiles[GeneratorSteps^[i].Tile_Type].TileMap[1,1,1].faces[angle_right]>=face_free then
                 Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps^[i].tx-1+1)*16,(maxy-GeneratorSteps^[i].ty-tilesizey+1)*16,dmBlendSmart)
             end;
           except
             writeln(Tile_PNG[j].width,'x',Tile_PNG[j].height,' - ',Tile_PNG[j].classname,' vs ',Minimap[iz].classname);
           end
         else writeln('no tile');
       end;
   end;

end;

{------------------------------------------------------------------------}

//this procedure creates 'chunks' of the tiles to improve FPS and etc.
//Neighbours is all tiles that can be visible from this specific xyz.
procedure GetNeighbours;
const raycast_corner_accuracy=0.001;
var i,j,ix,iy,iz:integer;
    fi,theta,d_alpha,d_fi:single;
    x0,y0,z0,vx,vy,vz:single;
    x1,y1,z1:single;
    xx0,yy0,zz0,xx,yy,zz:integer;
    dx,dy,dz:integer;
    flg:boolean;
    startRaycastTime:TDateTime;

    Tiles_count:array[0..maxMapTiles]of integer;
    min_tiles_count,min_tile_pos:integer;
    debug_group_size:integer;
begin
 StartRaycastTime:=now;
 writeln('raycasting...');
 new(MapTileIndex);
 new(Neighbours);

 //clear neighbours and MapTileIndex
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do begin
     MapTileIndex^[ix,iy,iz]:=-1;
     for i:=0 to n_tiles do Neighbours^[ix,iy,iz][i]:=0;
   end;

 //Fill MapTileIndex for quick access here and in-game
 for i:=0 to n_tiles do if not Tiles[GeneratorSteps^[i].tile_type].blocker then with Tiles[GeneratorSteps^[i].Tile_type] do
   for ix:=1 to tilesizex do
    for iy:=1 to tilesizey do
     for iz:=1 to tilesizez do if TileMap[ix,iy,iz].base<>tile_na then
       MapTileIndex^[GeneratorSteps^[i].tx+ix-1,GeneratorSteps^[i].ty+iy-1,GeneratorSteps^[i].tz+iz-1]:=i;

 {the following raycasting algorithm is extremely inefficient...
 However, I found no easy way to improve it significantly without missing any tiles and its better to spend more time now but provide for better visual and higher FPS
 The main idea is following:
 We select 'this tile' and raycast from 8 of its corner points in all sphere around with angular step d_alpha which is ~half minimal possible view angle for a 1x1x1 tile
 Then we sum how many times each tile has been hit by our raycast. If it is 0, then this tile is invisible and may be switched-off.
 If this is above 0, then this number is the tile priority we may use to dynamically choose different LODs if FPS is low}

 d_alpha:=1/sqrt(sqr(maxx)+sqr(maxy)+sqr(maxz))*2;
 for iz:=1 to maxz do begin
  for ix:=1 to maxx do
   for iy:=1 to maxy do if MapTileIndex^[ix,iy,iz]>=0 then

     for dx:=0 to 1 do
      for dy:=0 to 1 do
       for dz:=0 to 1 do begin{these enumerate 8 corners, i.e. we have to raycast 8 spheres}
         {x0,y0,z0 is initial point randomly near the corner}
         if dx=0 then x0:=ix+raycast_corner_accuracy*(random+0.1) else x0:=ix+1-raycast_corner_accuracy*(random+0.1);
         if dy=0 then y0:=iy+raycast_corner_accuracy*(random+0.1) else y0:=iy+1-raycast_corner_accuracy*(random+0.1);
         if dz=0 then z0:=iz+raycast_corner_accuracy*(random+0.1) else z0:=iz+1-raycast_corner_accuracy*(random+0.1);
         {initialize fi&theta}
         theta:=0;
         repeat
           if (theta>0) and (theta<Pi) then d_fi:=d_alpha/sin(theta) else d_fi:=2*Pi; //fi angular step is different depending on theta to maintain the same spatial step
           fi:=0;
           repeat
             {---- begin 1 raycast at (fi,theta) ----}
             {first determining raycast vector components}
             vx:=sin(theta)*cos(fi);
             vy:=sin(theta)*sin(fi);
             vz:=cos(theta);
             x1:=x0;
             y1:=y0;
             z1:=z0;
             xx:=ix;
             yy:=iy;
             zz:=iz;
             flg:=false;
             {now move along the vector sequentially checking all x<>x, y<>y and z<>z faces}
             repeat
               xx0:=xx;
               yy0:=yy;
               zz0:=zz;
               x1:=x1+vx/10;
               y1:=y1+vy/10;
               z1:=z1+vz/10;
               xx:=trunc(x1);
               yy:=trunc(y1);
               zz:=trunc(z1);
               if (xx<>xx0) or (yy<>yy0) or (zz<>zz0) then begin
                if MapTileIndex^[xx0,yy0,zz0]=-1 then flg:=true else begin
                 inc(Neighbours^[ix,iy,iz][MapTileIndex^[xx0,yy0,zz0]]);
                 if (xx<>xx0) then begin
                   if (xx>xx0) and (Map^[xx0,yy0,zz0].faces[angle_right]=face_wall) then flg:=true;
                   if (xx<xx0) and (Map^[xx0,yy0,zz0].faces[angle_left]=face_wall) then flg:=true;
                 end;
                 if (yy<>yy0) then begin
                   if (yy>yy0) and (Map^[xx0,yy0,zz0].faces[angle_bottom]=face_wall) then flg:=true;
                   if (yy<yy0) and (Map^[xx0,yy0,zz0].faces[angle_top]=face_wall) then flg:=true;
                 end;
                 if (zz<>zz0) then begin
                   if (zz>zz0) and (Map^[xx0,yy0,zz0].floor[angle_stairs_down]=floor_wall) then flg:=true;
                   if (zz<zz0) and (Map^[xx0,yy0,zz0].floor[angle_stairs_up]=floor_wall) then flg:=true;
                 end;
                end;
               end;
             until flg;
             {---- end 1 raycast at (fi,theta) ----}
             fi+=d_fi;
           until fi>=2*Pi;
           if (theta<Pi/2) and (theta+d_alpha>=Pi/2) then theta:=Pi/2 else theta+=d_alpha; //It is mandatory to catch exactly Pi/2 for theta
         until theta>Pi;

       end;
   writeln('raycasting ',100*iz div maxz,'%');
   end;
  {one more thing left to do - check for blockers because they are not included in MapTileIndex!}
  for i:=0 to n_tiles do if Tiles[GeneratorSteps^[i].tile_type].blocker then begin
    xx:=GeneratorSteps^[i].tx;
    yy:=GeneratorSteps^[i].ty;
    zz:=GeneratorSteps^[i].tz;
    j:=-1;
    repeat
      inc(j);
    until (Tiles[GeneratorSteps^[i].tile_type].TileMap[1,1,1].faces[j]>=face_free) or (j>=MaxAngles);
    for ix:=1 to maxx do
     for iy:=1 to maxy do
      for iz:=1 to maxz do
       Neighbours^[ix,iy,iz][i]:=Neighbours^[ix,iy,iz][MapTileIndex^[xx+a_dx(j),yy+a_dy(j),zz]];
  end;
  writeln('Raycasting done in ',trunc((now-StartRayCastTime)*24*60),' min ',round((now-StartRayCastTime)*24*60*60-trunc((now-StartRayCastTime)*24*60)*60),' s');

  {and one more thing... calculate basic neighbours
  These coefficients allow replace distant tiles for LODs (currently just tiles without placeholders) to improve FPS.
  Neighbours_limit causes almost no artefacts at current tileset. Only occasional glitches at stairs.
  Neighbours_limit_max is heavy on artefacts and practically is the largest rational value possible
  For some stupid reason I can't estimate it approximately correctly :(}
  MaxNeighbours:=maxint;   //well... practically we seek the lowest value, representing 1x1 tiles as larger tiles are non-linearly multiplied by that
  for i:=1 to n_tiles do if not Tiles[generatorsteps^[i].tile_type].blocker then
    if (Neighbours^[generatorsteps^[i].tx,generatorsteps^[i].ty,generatorsteps^[i].tz][i]>0) and (MaxNeighbours>Neighbours^[generatorsteps^[i].tx,generatorsteps^[i].ty,generatorsteps^[i].tz][i]) then MaxNeighbours:=Neighbours^[generatorsteps^[i].tx,generatorsteps^[i].ty,generatorsteps^[i].tz][i];
  writeln('Basic_Neighbours_Value ',MaxNeighbours);
  MaxNeighbours:=round(100*sqrt(MaxNeighbours/4000));
  CurrentMaxNeighbours:=MaxNeighbours div 10;

  {this part of the code will combine tiles in larger groups to boost FPS
  also this will provide for LOD of the whole group generation and far land support for overworld later
  I'm not yet sure how textures will behave... but let's leave this question for later}
  writeln('Preparing TGroupNodes...');
  {first we just count how much time a tile is 'hit' by neighbours
  Therefore we find "more popular" tiles which will be 'seeds' for our groups}
  for i:=0 to n_tiles do tiles_count[i]:=0;
  for ix:=1 to maxx do
   for iy:=1 to maxy do
    for iz:=1 to maxz do
     for i:=0 to n_tiles do if Neighbours^[ix,iy,iz][i]>0 then inc(Tiles_count[i]);
  {now let's start the main algorithm}
  N_groups:=0;
  repeat
    {here we find the tile with minimal amount (>0) of currently visible neighbours,
    it produces smoother amount of members in a group
    versus less groups in case maximum is used
    Roughly, amount of groups will be ~sqrt(tiles) However, it's not as simple as it might seem}
    Min_Tiles_Count:=MaxInt;
    for i:=0 to n_tiles do if (min_Tiles_Count>tiles_count[i]) and (tiles_count[i]>0) then begin
      min_Tiles_Count:=Tiles_count[i];
      min_Tile_pos:=i;
    end;
    //if there are still nodes remaining we create a new group to hold them
    if min_Tiles_count<maxint then begin
      inc(n_groups);
      debug_group_size:=0;
      //scan max_tile_pos tile and mark all its neighbours as a group
      for dx:=0 to Tiles[GeneratorSteps^[min_Tile_pos].Tile_Type].tilesizex-1 do
       for dy:=0 to Tiles[GeneratorSteps^[min_Tile_pos].Tile_Type].tilesizey-1 do
        for dz:=0 to Tiles[GeneratorSteps^[min_Tile_pos].Tile_Type].tilesizez-1 do
         for i:=0 to n_tiles do if (Neighbours^[GeneratorSteps^[min_Tile_pos].tx+dx,GeneratorSteps^[min_Tile_pos].ty+dy,GeneratorSteps^[min_Tile_pos].tz+dz][i]>0) and (Tiles_count[i]>0) then begin
           Tiles_count[i]:=-1;     //don't add this tile to another group
           groups[i]:=n_groups; //add this tile to this group
           inc(debug_group_size);
         end;
      writeln(n_groups,' group size=',debug_group_size);
    end;
  //  writeln('Max_Tiles_Count=',Max_Tiles_Count);
  until Min_Tiles_Count=maxint;
  writeln('TGroupNodes list is ready... n_groups=',n_groups);
end;

{------------------------------------------------------------------------------------}

//powerful procedure to add placeholders recoursively to their places.
procedure AddPlaceHolderRecoursive(ContainerObject:TTransformNode;ParentObject:TTransformNode);
var k,m,q:integer;
    thisPlaceholder:integer;
    ThisPlaceholderStyle:PlaceholderStyle;
    MPE:integer; //MapPlaceholderElements for this recoursion
    flg:boolean;
begin
 //todo pick a random placeholder and check against random
 //todo placeholders symmetry groups.
   ThisPlaceholder:=-1;
   ThisPlaceholderStyle:=ParsePlaceholder(ParentObject.NodeName);

   if random*100>ThisPlaceholderStyle.PlaceholderRND then exit; //drop empty if empty demanded

//   writeln('adding placeholder '+ThisPlaceholderStyle.PlaceholderName);

   //First of all, we search for this placeholder name in PlaceholderCompatibility array
   m:=0;
   flg:=false;
   repeat
     inc(m);
     if PlaceholderCompatibility^[m].PlaceholderName=ThisPlaceHolderStyle.PlaceholderName then flg:=true;
   until (flg) or (m=MaxPlaceholderAtlas);
   if not flg then begin
     //I think something better should be here
     writeln('ERROR: invalid placeholder name ',ThisPlaceHolderStyle.PlaceholderName);
     exit;
   end;

   repeat
     //Now we start randomly 'hitting' all loaded placeholder replacers to check if they are compatible with 'this placeholder'
     //those records are located in PlaceHolderAtlas array
     k:=trunc(random*MaxPlaceholderTypes)+1; if k>MaxPlaceHolderTypes then k:=MaxPlaceHolderTypes;
     //first we check the selected k-th placeholder against it's 'wieght'.
     if random<PlaceHolderAtlas^[k].PlaceholderRND[1] then begin //todo:different weights at different z
       //now cycle through all m-th placeholder compatibility array to check if k-th placeholder replacer is compatible with it
       q:=0;
       flg:=false;
       repeat
         inc(q);
         if PlaceholderCompatibility^[m].compatibility_string[q]=PlaceHolderAtlas^[k].PlaceholderName then flg:=true;
       until (flg) or (q>=PlaceholderCompatibility^[m].Compatibility_records);
       //if such record is found then ThisPlaceHolder is set to k. We've found a proper replacement for the placeholder.
       if flg then
         if random<PlaceholderCompatibility^[m].compatibility_RND[q] then begin
           ThisPlaceHolder:=k;
//           writeln('select ',PlaceholderCompatibility[m].placeholderName, ' to ',thisplaceholder,' ',PlaceHolderAtlas[k].PlaceholderName);
         end;
     end;
   until (ThisPlaceholder>-1) or (random<0.0001);
   //there is a 0.01% chance that placeholder search fails (just to be freeze-save in case no compatible placeholder exists with 'this placeholder' e.g. due to a typo). It'll spoil only 0.1% of placeholders so negligible.
   if ThisPlaceHolder=-1 then begin
     writeln('ERROR: cannot pick a placeholder for '+ThisPlaceholderStyle.PlaceholderName+'. Dropping...');
     exit;
   end;
   //finally if everything is fine, then proceed.

 //this placeholder 'survived' random-checks and compatibility checks and is ready to be placed;
 //let's create a TTransformNode wrapper
 //practically we simply replace 'parent' (which is not added to container) with an empty newly created TTransform
 //writeln('Replacing '+ParentObject.nodename+' with'+placeholders[thisPlaceholder].NodeName);

 inc(mapPlaceholderElements);
 if MapPlaceholderElements>maxMapPlaceholders then begin
   writeln('Error: MapPlaceholderElements>maxMapPlaceholders!');
   exit;
 end;
 MPE:=mapPlaceholderElements;
 MapPlaceHolders[MPE]:=TTransformNode.Create('','');
 MapPlaceHolders[MPE].Translation:=ParentObject.Translation;
 MapPlaceHolders[MPE].Rotation:=ParentObject.Rotation;
 MapPlaceHolders[MPE].scale:=ParentObject.Scale;
// writeln('translation: ',MapPlaceHolders[mapPlaceholderElements].Translation[0],MapPlaceHolders[mapPlaceholderElements].Translation[1],MapPlaceHolders[mapPlaceholderElements].Translation[2]);

 for k:=0 to placeholders[thisPlaceholder].fdchildren.count-1 do if placeholders[thisPlaceholder].fdchildren[k] is TTransformNode then begin
   if copy(placeholders[thisPlaceholder].fdchildren[k].NodeName,1,1)='(' then begin
     //if recoursive placeholder then add a recoursive child
     AddPlaceHolderRecoursive(MapPlaceHolders[MPE]{Container Object},placeholders[thisPlaceholder].fdchildren[k] as TTransformNode{Parent object});
   end else begin
     //if a simple TTransform element, then add it
     //TOOD: Lights
     MapPlaceHolders[MPE].FdChildren.add(placeholders[thisPlaceholder].fdchildren[k]);
   end;
  end;
  //finally, we add this placeholder as a child to its Container to keep all previous transformations...
  ContainerObject.FdChildren.add(MapPlaceHolders[MPE]);
end;


{------------------------------------------------------------------------}

//this procedure makes a X3D scene from the generator steps and corresponding X3D tiles.
//I.e. everything before was abstract. Now it's a 3D world.
//I prefer to keep it separate for many reasons.
procedure MakeScene;
var i,j:integer;
begin
// NOW: Create the scene {todo: and slice it into chunks}
 GetNeighbours;
// First, we construct MapTiles[i] array of map tiles together with all static items & placeholders
// note: now mapelements=n_tiles, but this might change in future, so, I'd like to keep those separated
 mapPlaceholderElements:=0;
 SetLength(MapTiles,n_tiles+1);
 {$Ifdef UseSwitches}SetLength(MapTilesLOD,n_tiles+1);{$endif}
 for i:=0 to n_tiles do begin
   MapTiles[i]:=TTransformNode.Create('','');
   MapTiles[i].translation:=Vector3Single(2*myscale*(GeneratorSteps^[i].tx),-2*myscale*(GeneratorSteps^[i].ty),-2*myscale*(GeneratorSteps^[i].tz));
   MapTiles[i].scale:=Vector3Single(myscale,myscale,myscale);
   {$Ifdef UseSwitches}MapTilesLOD[i]:=MapTiles[i].DeepCopy as TTRansformNode;{$endif}
   for j:=0 to Tiles[GeneratorSteps^[i].Tile_type].Tile3d.FdChildren.Count-1 do if Tiles[GeneratorSteps^[i].Tile_type].Tile3d.fdChildren[j] is TTransformNode then begin
     //if this is a placeholder, then some work is needed t
     //else no work, just add the tile element to the 'MapTiles[i]'
     if copy(Tiles[GeneratorSteps^[i].Tile_type].Tile3d.FdChildren[j].NodeName,1,1)='(' then begin
       AddPlaceHolderRecoursive(MapTiles[i]{ContainerObject},Tiles[GeneratorSteps^[i].Tile_type].Tile3d.FdChildren[j] as TTransformNode{ParentObject});
     end else begin
       MapTiles[i].FdChildren.add(Tiles[GeneratorSteps^[i].Tile_type].Tile3d.FdChildren[j]);
      {$Ifdef UseSwitches}MapTilesLOD[i].FdChildren.add(Tiles[GeneratorSteps^[i].Tile_type].Tile3d.FdChildren[j]);{$endif}
     end;
   end;
 end;

// Second, we add all the ready tiles to the MainRoot. For some 'future' reason and/or readability we might keep that separate
//!!! WE NEED TO ADD EVERYTHING ONCE AND RENDER IT TO PREPARE EVERYTHING, LIKE LIGHT, ETC.
//THEN GAME WILL REMOVE THE UNNEEDED CHUNKS AT FIRST RENDER
//Doesn't work now...

{$IfDef UseSwitches}
  SetLength(MapSwitches,n_tiles+1);
  for i:=0 to N_tiles do begin
    MapSwitches[i]:=TSwitchNode.create('','');
    MapSwitches[i].FdChildren.add(MapTiles[i]);
    MapSwitches[i].FdChildren.add(MapTilesLOD[i]);
    MapSwitches[i].WhichChoice:=0;
  end;
{$EndIf}

SetLength(MapRoots,N_Groups+1);
{$ifdef TryCastleScenes}
for i:=1 to N_groups do MapRoots[i] := TX3DRootNode.Create('', '');
for i:=0 to N_tiles do
  {$ifdef UseSwitches}
  MapRoots[groups[i]].FdChildren.Add(MapSwitches[i]);
  {$else}
  MapRoots[groups[i]].FdChildren.Add(MapTiles[i]);
  {$endif}
{$else}
MapGroups: array[1..maxGroups] of TStaticGroupNode;
for i:=1 to N_groups do MapGroups[i]:=TStaticGroupNode.Create('','');
 for i:=0 to N_tiles do MapGroups[groups[i]].FdChildren.Add(MapTiles[i]);

 MainRoot := TX3DRootNode.Create('', '');
 for i:=1 to N_groups do begin
   GroupsSwitches[i]:=TSwitchNode.Create('','');
   GroupsSwitches[i].WhichChoice:=0;
   GroupsSwitches[i].FdChildren.Add(MapGroups[i]);
   MainRoot.FdChildren.Add(GroupsSwitches[i]);
 end;
{$endif}
//create light that follows the player
NavLight:= TPointLightNode.Create('', '');
NavLight.FdColor.Value := vector3single(1,0.5,0.1);
NavLight.FdAttenuation.value := Vector3Single(0,0,6);
NavLight.FdRadius.value:=1;
NavLight.FdIntensity.value:=30;
NavLight.FdOn.value:=true;
NavLight.FdShadows.value:=false;
//and create a respective navigation node and add it to the MainRoot
nav:=TKambiNavigationInfoNode.Create('', '');
nav.FdHeadLightNode.Value := NavLight;
nav.FdHeadlight.Value:=true;

{$ifdef TryCastleScenes}
MapRoots[0]:=TX3DRootNode.Create('','');
MapRoots[0].FdChildren.add(nav);
{$else}
MainRoot.FdChildren.add(nav);
//Save3D(MainRoot,'Map.x3d','','',xeXML);
{$endif}

//finally add everything to the scene.
{$ifdef TryCastleScenes}
SetLength(MapScenes,N_Groups+1);
for i:=0 to N_groups do begin
  MapScenes[i]:=TCastleScene.Create(Window.Scenemanager);
  {MapScenes[i].Attributes.Shaders := srAlways;}
  MapScenes[i].ShadowMaps:=Shadow_maps_enabled;  {?????}
  MapScenes[i].Spatial := [ssRendering, ssDynamicCollisions];
  MapScenes[i].ProcessEvents := true;
  MapScenes[i].Load(MapRoots[i],true);
  Window.Scenemanager.items.add(MapScenes[i]);
end;
Window.SceneManager.mainScene:=MapScenes[0];
{$else}
MainScene:=TCastleScene.create(window);
MainScene.ShadowMaps:=Shadow_maps_enabled;  {?????}
MainScene.Spatial := [ssRendering, ssDynamicCollisions];
MainScene.ProcessEvents := true;
MainScene.load(MainRoot,true);
Window.Scenemanager.items.add(MainScene);
Window.SceneManager.mainScene:=MainScene;
{$endif}
Window.Scenemanager.ShadowVolumes:=true; {?????}
Window.SceneManager.ShadowVolumesRender:=true; {?????}

dispose(PlaceholderAtlas);
dispose(PlaceholderCompatibility);
end;

{=====================================================================}

//this is a meta-procedure that gathers all map generation steps to a single sequence.
procedure MakeMap;
var timestamp:TDateTime;
    LastTimestamp:TDateTime;
begin
  TimeStamp:=now;

  new(Map);
  new(GeneratorSteps);
  Repeat
    LastTimeStamp:=now;
    randomize;
    eraseMap;

    //make entrance;
    startx:=maxx div 2;
    starty:=maxy div 2;
    startz:=1;

    CreateMap;

    writeln('Map created in ',round((now-LastTimeStamp)*24*60*60*1000),'ms');
    if not ((Target_Map_Area-MapArea)/Target_Map_Area<allow_error) then writeln('Target failed by more than ',round(allow_error*100),'%. Re-generating map... (',MapArea,'/',Target_Map_Area,')');
    if not (deepest_Level=maxz) then writeln('Not all levels of the dungeons used '+inttostr(deepest_level)+'/'+inttostr(maxz)+'. Re-generating map...');
  until (((Target_Map_Area-MapArea)/Target_Map_Area<allow_error) and (deepest_Level=maxz)) or (random<0.01);

  dispose(TileAtlas); //this one is no longer needed as soon as we've generated the basic map.

  writeln('Job finished in ',round((now-timestamp)*24*60*60),'s');

  LastTimeStamp:=now;
  writeln('making minimap');
  MakeMinimap;
  writeln('making scene');
  MakeScene;
  writeln('create distance map');


  CreateDistanceMap;

  writeln('Done in ',round((now-LastTimeStamp)*24*60*60),'s');

   //Place Rose
   RoseT:=T3DTransform.Create(Window.SceneManager);
   RoseT.add(RoseS);
   RoseT.translation:=Vector3Single(2*myscale*(Rosex),-2*myscale*(Rosey),-2*myscale*(Rosez));
   //todo: this is an ugly fix for brick pass tileset being 0.5 tile 'higher' than others
   //I'm not yet sure how to work on 'floor height' for dynamic placeholders/items but it's not urgent.
   if copy(Tiles[GeneratorSteps^[MapTileIndex^[Rosex,Rosey,Rosez]].Tile_Type].TileName,1,5)='Brick' then RoseT.translation:=Vector3Single(2*myscale*(Rosex),-2*myscale*(Rosey),-2*myscale*(Rosez-0.25));

   dispose(GeneratorSteps); //!!! If not needed later for debugging
   dispose(MapTileIndex);//PS we might need that for debugging...

   RoseT.scale:=Vector3Single(myscale,myscale,myscale);
   Window.Scenemanager.items.add(RoseT);
   RoseFound:=false;
  //     RoseTransform := RoseS.RootNode.FindNodeByName(TTransformNode,'Cylinder_TRANSFORM', true) as TTransformNode;

  //show some debug information

   writeln('Map finished in ',round((now-timestamp)*24*60*60*1000),'ms');

   writeln('Map Area = '+inttostr(MapArea)+'/'+inttostr(Target_Map_Area));
   if MapArea<Target_Map_Area then writeln('Target failed by '+inttostr(round((Target_Map_Area-MapArea)/Target_Map_Area*100))+'%') else
                                   writeln('Target met with excess of '+inttostr(round((MapArea-Target_Map_Area)/Target_Map_Area*100))+'%');
   writeln('Tiles = '+inttostr(n_tiles+1));
   writeln('Deepest level = '+inttostr(Deepest_level)+'/'+inttostr(Maxz));
   writeln('Max distance = '+inttostr(Max_distance));
end;

end.

