unit Generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  castle_base, CastleWindow,
  castlescene, castlescenemanager,
  castle3d, castlevectors,  X3DNodes, CastleImages,
  Tile_var, generic_var;

type Generator_type = record
  tile_type:integer;
  tx,ty,tz:integer;
end;

var n_tiles:integer;
    maxx,maxy,maxz:byte;

    MapTiles: array[1..maxMapTiles] of T3DTransform;
    Map: array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of Basic_Tile_Type;
    GeneratorSteps: array[1..maxMapTiles] of Generator_type;

    MapArea,explored_area,deepest_level,max_distance:integer;
    startx,starty,startz:integer;


    DistanceMap,olddistancemap:array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of integer;

    RoseT:T3DTRansform;

    Rosex,Rosey,Rosez:integer;
    RoseFound:boolean;

    Minimap:array[1..maxmaxz] of TCastleImage;


procedure MakeMap;

implementation

{---------------------------------------------------------------------------}
//basic get map procedure. Simply does all routine checking that the gx,gy,gz is not out of bounds.
function getMap(gx,gy,gz:integer):Basic_Tile_Type;
begin
  if (gx>0)     and (gy>0)     and (gz>0) and
     (gx<=maxx) and (gy<=maxx) and (gz<=maxz) then
    getMap:=Map[gx,gy,gz]
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
//The most important procedure - compares incoming 1x1x1 base tiles to the Map.
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
        if TileMap[jx,jy,jz].base<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].base:=TileMap[jx,jy,jz].base;
        for jj:=0 to maxangles do
          if TileMap[jx,jy,jz].faces[jj]<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].faces[jj]:=TileMap[jx,jy,jz].faces[jj];
        for jj:=1 to 2 do
          if TileMap[jx,jy,jz].floor[jj]<>tile_na then Map[x+jx-1,y+jy-1,z+jz-1].floor[jj]:=TileMap[jx,jy,jz].floor[jj];
      end;
    // Prepare a Generator step for placing a tile
    // to create a 3D world later
    inc(n_tiles);
    GeneratorSteps[n_tiles].Tile_Type:=inTileType;
    GeneratorSteps[n_tiles].tx:=x;
    GeneratorSteps[n_tiles].ty:=y;
    GeneratorSteps[n_tiles].tz:=z;
    PutTile:=true;
  end else PutTile:=false;
 end;
end;

{----------------------------------------------------------------------------------}

Procedure EraseMap;
var i,ix,iy,iz:integer;
begin
 n_tiles:=0;
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
end;

{------------------------------------------------------------------------}

Procedure CreateMap;
var i,ix,iy,iz:integer;
    tx,ty,tz,ta,tt:integer;
    FreeFaces:integer;
    flg:boolean;
    shiftx,shifty,shiftz:integer;
begin
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
      if Map[ix,iy,iz].base>=tile_free then inc(MapArea);
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
         until (flg) or (random<1/(Tiles[tt].TileSizex+Tiles[tt].TileSizey+Tiles[tt].TileSizez));
       end else begin
         //simple place small tile 1x1x1
         if not tiles[tt].blocker then
           flg:=PutTile(tt,tx+a_dx(ta),ty+a_dy(ta),tz);
       end;
     Until flg or (random<0.01/maxTilesTypes);
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
end;

{------------------------------------------------------------------------}

Procedure CreateDistanceMap;
var i,ix,iy,iz:integer;
    tx,ty,tz:integer;
    flg:boolean;
begin
 //create distance map; basic for pathfinding
 // now only used to place a rose at the most distant place of the map
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

 //now find the max distance
max_distance:=0;
for ix:=1 to maxx do
for iy:=1 to maxy do
 for iz:=1 to maxz do if (distanceMap[ix,iy,iz]>max_distance) then begin
   tx:=ix;
   ty:=iy;
   tz:=iz;
   max_Distance:=distanceMap[ix,iy,iz];
 end;

Rosex:=tx;
Rosey:=ty;
Rosez:=tz;

end;

{------------------------------------------------------------------------}

Procedure MakeMinimap;
var i,j,iz:integer;
begin
//DrawMinimap;
  for iz:=1 to maxz do begin
    minimap[iz]:=LoadImage(Models_Folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;       //BUG: it's the only way I could initialize TCastleImage...????????
    //minimap[iz]:=(minimap[iz] as TRGBImage).ToRGBAlphaImage;
    minimap[iz].setsize((maxx)*16-1,(maxy)*16-1,1);
    for i:=1 to n_tiles do with Tiles[GeneratorSteps[i].Tile_Type] do
     for j:=1 to tilesizez do
       if GeneratorSteps[i].tz+j-1=iz then begin
         if Tile_PNG[j]<>nil then
           try
             Tile_PNG[j].DrawTo(Minimap[iz],(GeneratorSteps[i].tx-1)*16,(maxy-GeneratorSteps[i].ty-tilesizey+1)*16);
           except
             writeln(Tile_PNG[j].width,'x',Tile_PNG[j].height,' - ',Tile_PNG[j].classname,' vs ',Minimap[iz].classname);
           end;
       end;
   end;

end;

{------------------------------------------------------------------------}

procedure MakeScene;
var i:integer;
begin
// NOW: Create the scene {todo: and slice it into chunks}
 for i:=1 to n_tiles do begin
   MapTiles[i]:=T3DTransform.Create(Window.SceneManager);
   MapTiles[i].add(Tiles[GeneratorSteps[i].Tile_type].Tile_scene);
   MapTiles[i].translation:=Vector3Single(-2*myscale*(GeneratorSteps[i].tx),-2*myscale*(GeneratorSteps[i].tz),-2*myscale*(GeneratorSteps[i].ty));
   MapTiles[i].scale:=Vector3Single(myscale,myscale,myscale);
   Window.Scenemanager.items.add(MapTiles[i]);
//    window.scenemanager.items.remove(MapTiles[n_tiles]);
 end;
end;

{=====================================================================}

procedure MakeMap;
var timestamp:TDateTime;
    LastTimestamp:TDateTime;
begin
  TimeStamp:=now;

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
    if not ((Target_Map_Area-MapArea)/Target_Map_Area<0.05) then writeln('Target failed by more than 5%. Re-generating map...');
  until ((Target_Map_Area-MapArea)/Target_Map_Area<0.05);

  writeln('Job finished in ',round((now-timestamp)*24*60*60*1000),'ms');

  MakeMinimap;
  MakeScene;

  CreateDistanceMap; //todo:and place the rose

   //PlaceRose;
   RoseT:=T3DTransform.Create(Window.SceneManager);
   RoseT.add(RoseS);
   RoseT.translation:=Vector3Single(-2*myscale*(Rosex),-2*myscale*(Rosez),-2*myscale*(Rosey));
   RoseT.scale:=Vector3Single(myscale,myscale,myscale);
   Window.Scenemanager.items.add(RoseT);
   RoseFound:=false;
  //     RoseTransform := RoseS.RootNode.FindNodeByName(TTransformNode,'Cylinder_TRANSFORM', true) as TTransformNode;

  //show some debug information

   writeln('Map finished in ',round((now-timestamp)*24*60*60*1000),'ms');

   writeln('Map Area = '+inttostr(MapArea)+'/'+inttostr(Target_Map_Area));
   if MapArea<Target_Map_Area then writeln('Target failed by '+inttostr(round((Target_Map_Area-MapArea)/Target_Map_Area*100))+'%') else
                                   writeln('Target met with excess of '+inttostr(round((MapArea-Target_Map_Area)/Target_Map_Area*100))+'%');
   writeln('Tiles = '+inttostr(n_tiles));
   writeln('Deepest level = '+inttostr(Deepest_level)+'/'+inttostr(Maxz));
   writeln('Max distance = '+inttostr(Max_distance));
end;

end.

