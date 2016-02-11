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

//This unit defines what is a tile and loads it.

unit Tile_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  castlescene, castleimages, castlescenecore, X3DNodes,
  x3dload,
  generic_var, MazerMapParser;


// constants for tiles and faces

const face_na = 0;
const face_wall = 1;
      face_free = 2; // and >2 are face types


 const tile_na = 0;
 const tile_free = 2;
       //tile_wall = 1;   //?unneeded?
       Tile_Stairs_Down = 3;
       Tile_Stairs_Up = 4;

       tile_inacceptible = 255;

const floor_na = 0;
const floor_wall = 1;
      floor_free = 2;

// 1x1x1 tile type (subtype) declaration
const maxangles = 3; {0..3 for rectagonal grid //0..5 for hexagonal - yet not implemented, and maybe, it'll never be}
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
      maxtilesizez = 3;   {max tile height}
type Map_Tile_type = record
  Tile3D: TX3DRootNode;
  TileName: string;
//  Tile_scene: TCastleScene;
  Tile_PNG: array[1..maxtilesizez] of TCastleImage;
  TileFreeFaces:byte;
  tilesizex,tilesizey,tilesizez:byte;
  Has_Stairs_down:boolean;    // Tile has stairs down (to meet the generation algorithm, requiring sometimes ladders to go down)
  blocker:boolean;            // Tile is a generic face blocker.
  TileMap:array[1..maxtilesize,1..maxtilesize,1..maxtilesizez] of Basic_Tile_Type;
end;

//this record defines 'how often the tile can appear at certain levels'
//formula is random<tileRND. i.e. tileRND=1 will 100% appear if chosen and fits the location
// and tileRND=0.1 will be descarded in 90% cases if chosen
type TileZAtlasRecord=record
  TileRND:array[1..maxmaxz] of single;
end;

//Same atlas stuff for placeholders
//if placeholder is chosen, it must resist PlaceholderRND to be placed
//parallel to other checks
type PlaceholderZAtlasRecord=record
  PlaceholderName:string;
  PlaceholderRND:array[1..maxmaxz] of single;
end;

//this is the placeholder atlas. It defines compatibility between 'Placeholder_name' and specific placeholders
//if compatibility_RND>0 then the specific placeholder may be replaced by the item named compatibility string
//todo sort'n search features (maybe, no need in that as it doesn't need to be efficient)
Type PlaceholderAtlasRecord=record
  placeholderName:shortstr;
  Compatibility_records:integer;
  compatibility_string:array[1..maxMaxPlaceholdersTypes] of shortstr;
  compatibility_RND:array[1..maxMaxPlaceholdersTypes] of single;
end;

var Tiles: array[1..MaxMaxTilesTypes] of Map_Tile_Type;
    TileAtlas: array[1..MaxMaxTilesTypes] of TileZAtlasRecord;
    MaxTilesTypes:integer=MaxMaxTilesTypes;

    Placeholders: array [1..maxMaxPlaceholdersTypes] of TX3DRootNode;
    MaxPlaceholderTypes:integer=maxMaxPlaceholdersTypes;
    PlaceholderAtlas: array[1..maxMaxPlaceholdersTypes] of PlaceholderZAtlasRecord;
    MaxPlaceholderAtlas:integer=maxMaxPlaceholdersTypes;
    PlaceholderCompatibility: array[1..MaxMaxPlaceholderAtlasRecords] of PlaceholderAtlasRecord;
    MaxPlaceholderAtlasRecords:integer=MaxMaxPlaceholderAtlasRecords;

    RoseS:TCastleScene;


    TileMapLoaded:boolean; //if false then no tile map could have been loaded

    TextureProperties:TTexturePropertiesNode;

Function LoadTile(fileName:string):Map_Tile_type;

procedure LoadTiles;

implementation

Procedure CalculateTileFaces;
var i,j,ix,iy,iz:integer;
    FreeFaces:byte;
begin
   //calculate tile's free faces for later use
  for i:=1 to MaxTilesTypes do with Tiles[i] do begin
    FreeFaces:=0;
    for iz:=1 to tilesizez do begin
      //todo: this is not correct! Exits may be not only on border tiles
      //correct way is to search for borders and face_na
      ix:=1;
      for iy:=1 to tilesizey do if TileMap[ix,iy,iz].faces[angle_left]>=face_free then inc(FreeFaces);
      ix:=tilesizex;
      for iy:=1 to tilesizey do if TileMap[ix,iy,iz].faces[angle_right]>=face_free then inc(FreeFaces);
      iy:=1;
      for ix:=1 to tilesizex do if TileMap[ix,iy,iz].faces[angle_top]>=face_free then inc(FreeFaces);
      iy:=tilesizey;
      for ix:=1 to tilesizex do if TileMap[ix,iy,iz].faces[angle_bottom]>=face_free then inc(FreeFaces);
    end;
{    for ix:=1 to tilesizex do
     for iy:=1 to tilesizey do
      for iz:=1 to tilesizez do
       for j:=0 to maxangles do if TileMap[ix,iy,iz].faces[j]>=face_free then inc(FreeFaces);}
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
{-----------------------------------------------------------------------------------}

Function LoadTile(fileName:string):Map_Tile_type;
var val1,j:integer;
    s:string;
    tmp:XYZ_record;
    TmpTile:Map_Tile_type;
    thisface,thisvalue,err:integer;

    i1,i2,i3,i4:integer;
    level1,level2:TTransformNode;
    level3:TGroupNode;
    level4:TShapeNode;
    Texture_diffuse,texture_normal:TImageTextureNode;
    flg:boolean;
begin
 TmpTile.Tile3D:=Load3D(tiles_models_folder+fileName);

 //create texture properties for anisotropic smoothing
 if anisotropic_smoothing>0 then begin
   textureProperties:=TTexturePropertiesNode.Create('','');
   TextureProperties.AnisotropicDegree:=anisotropic_smoothing;
   TextureProperties.FdMagnificationFilter.Value:='DEFAULT';
   TextureProperties.FdMinificationFilter.Value:='DEFAULT';
 end else TextureProperties:=nil;
 //scan Tile3D for TImageTexture derivatives
 for i1:=0 to TmpTile.Tile3D.FdChildren.count-1 do
   if TmpTile.Tile3D.FdChildren[i1] is TTransformNode then begin
     level1:=TmpTile.Tile3D.FdChildren[i1] as TTransformNode;
     {memo1.lines.add('TransformNode '+level1.NiceName);}
     for i2:=0 to level1.FdChildren.count-1 do
       if TmpTile.Tile3D.FdChildren[i2] is TTransformNode then begin
         level2:=level1.FdChildren[i2] as TTransformNode;
         {memo1.lines.add('TransformNode2 '+level2.NiceName);}
         for i3:=0 to level2.FdChildren.count-1 do
         if level2.FdChildren[i3] is TGroupNode then begin
           level3:=level2.FdChildren[i3] as TGroupNode;
           {memo1.lines.add('Groupnode '+level3.NiceName);}
           for i4:=0 to level3.fdChildren.count-1 do
           if level3.fdChildren[i4] is TShapeNode then begin
             level4:=level3.fdChildren[i4] as TShapeNode;
             //level4.Appearance.FdShaders;
             {memo1.lines.add('Shape '+ level4.NiceName);}
             texture_diffuse:=level4.fdappearance.Value.FindNode(TImageTextureNode,false) as TImageTextureNode;
             texture_diffuse.FdTextureProperties.Value:=TextureProperties;
             {memo1.lines.add('ImageTexture '+ texture_diffuse.NiceName);}

             //TODO: load normal map?
             //BUG: It doesn't work as expected?
             if Normal_map_enabled then begin
               texture_normal:=texture_diffuse.DeepCopy as TImageTextureNode;
               texture_normal.FdUrl.Items.Clear;
               texture_normal.fdURL.Items.add(stringreplace(Texture_Diffuse.FdUrl.Items[0],'diffuse','normal',[rfIgnoreCase]));
               //writeln(texture_normal.FdUrl.Items[0]);
               level4.Appearance.FdNormalMap.Value := texture_normal;
             end;
           end;
         end;
       end;
   end;

 //now, parse the tile map (tile layout)
 //I make some excessive checks... just in case e.g. file format change, or file damage
 assignFile(TileMapFile,tiles_models_folder+fileName+'.map');
 {$I-}
 reset(TileMapFile);
 {$I+}
 if IOresult=0 then begin
   repeat
     readln(TileMapFile,s);
     if copy(s,1,1)='<' then begin
       tmp:=ParseTileCoord(s)
     end else if copy(s,1,1)='[' then begin
       j:=1;
       repeat inc(j) until copy(s,j,1)=']';
       val(copy(s,2,j-2),thisface,err);
       if err=0 then begin
         val(copy(s,j+1,length(s)-j),thisvalue,err);
         if err=0 then begin
           TmpTile.TileMap[tmp.x,tmp.y,tmp.z].faces[thisface]:=thisvalue;
         end else ErrorString+='Parse error in thisvalue! '+ copy(s,j+1,length(s)-j);
       end else ErrorString+='Parse error in thisface! '+ copy(s,2,j-2);
     end else begin
       val1:=ParseBasicData(s,TileSizeX_Record);
       if val1<>no_data_record then TmpTile.tilesizex:=val1;
       val1:=ParseBasicData(s,TileSizeY_Record);
       if val1<>no_data_record then TmpTile.tilesizey:=val1;
       val1:=ParseBasicData(s,TileSizeZ_Record);
       if val1<>no_data_record then TmpTile.tilesizez:=val1;
       val1:=ParseBasicData(s,TileSizeY_Record);
       if val1<>no_data_record then TmpTile.tilesizey:=val1;

       val1:=ParseBasicData(s,TileBase_record);
       if val1<>no_data_record then TmpTile.TileMap[tmp.x,tmp.y,tmp.z].base:=val1;
       val1:=ParseBasicData(s,TileFloorUp_record);
       if val1<>no_data_record then TmpTile.TileMap[tmp.x,tmp.y,tmp.z].floor[angle_stairs_up]:=val1;
       val1:=ParseBasicData(s,TileFloorDown_record);
       if val1<>no_data_record then TmpTile.TileMap[tmp.x,tmp.y,tmp.z].floor[angle_stairs_down]:=val1;
     end;
   until eof(TileMapFile);
   closeFile(TileMapFile);
   {if errorstring='' then }TileMapLoaded:=true;
 end else TileMapLoaded:=false;

 LoadTile:=TmpTile;
end;

{------------------------------------------------------------------------------------------}

Procedure LoadTiles;
var i,j:integer;
    Rec : TSearchRec;
begin
  // now prepare 3D part + tile map view
  i:=0;
  if FindFirst (tiles_models_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       inc(i);
       //load TileAtlasRecrod for this map
       //...
       for j:=1 to maxz do TileAtlas[i].TileRND[j]:=1.0;
       //if tileRND>0 then ...
       Tiles[i]:=LoadTile(Rec.Name);
       Tiles[i].TileName:=Rec.Name;
       //else dec(i);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec) ;
   end;
   if i=0 then halt else MaxTilesTypes:=i;

   for i:=1 to maxTilesTypes do begin
  {  Tiles[i]:=LoadTile(inttostr(i)+'.x3d');}

    //load png tile for the mini-map
    for j:=1 to Tiles[i].tilesizez do
     try
       Tiles[i].Tile_PNG[j]:=LoadImage(tiles_models_folder+Tiles[i].TileName+'_'+inttostr(j)+'.png', [TRGBAlphaImage]) as TRGBAlphaImage;
     except
       Tiles[i].Tile_PNG[j]:=nil;
     end;
  end;
  CalculateTileFaces;

  //grab the rose
  RoseS:=TCastleScene.create(Window.sceneManager);
  RoseS.spatial := [ssRendering];
  RoseS.processevents:=true;
  RoseS.load(items_models_folder+'47386_Rose_CC0_by_Hyuku_merged_joined_scaled.x3d');

  //load placeholders
  i:=0;
  if FindFirst (placeholders_models_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then
   try
     repeat
       inc(i);
       //load Placeholder atlas record for this map
       //...
       for j:=1 to maxz do PlaceholderAtlas[i].PlaceholderRND[j]:=1.0;
       //if placeholderRND>0 then ...   // however that's not so trivial here as it was in tiles
       Placeholders[i]:=Load3D(placeholders_models_folder+Rec.Name);
       PlaceholderAtlas[i].PlaceholderName:=Rec.Name;
       //else dec(i);
     until FindNext(Rec) <> 0;
   finally
     FindClose(Rec) ;
   end;
   if i>0 then MaxPlaceholderTypes:=i;

   //hardcode the atlas...
   //later it will be autoloaded
    with PlaceholderCompatibility[1] do begin
     placeholderName:='BookShelf';
     Compatibility_records:=3;
     for i:=1 to Compatibility_records do compatibility_RND[i]:=1;
     compatibility_string[1]:='Furniture1_6_bookShelf1.x3d';
     compatibility_string[2]:='Furniture1_7_bookShelf2.x3d';
     compatibility_string[3]:='Furniture1_8_bookShelf3.x3d';
    end;
    with PlaceholderCompatibility[2] do begin
     placeholderName:='RoundTableChairLayout';
     Compatibility_records:=1;
     for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
     compatibility_string[1]:='Furniture1_16_squaretablechairslayout.x3d';
    end;
    with PlaceholderCompatibility[3] do begin
      placeholderName:='RoundTableLayout';
      Compatibility_records:=2;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_14_roundtablelayout2.x3d';
      compatibility_string[2]:='Furniture1_15_roundtablelayout.x3d';
    end;
    with PlaceholderCompatibility[4] do begin
      placeholderName:='SquareTableChairLayout';
      Compatibility_records:=1;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_16_squaretablechairslayout.x3d';
    end;
    with PlaceholderCompatibility[5] do begin
      placeholderName:='SquareTableLayout';
      Compatibility_records:=3;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_17_squaretablelayout.x3d';
      compatibility_string[2]:='Furniture1_18_squaretablelayout2.x3d';
      compatibility_string[3]:='Furniture1_19_squaretablelayout3.x3d';
    end;
    with PlaceholderCompatibility[6] do begin
      placeholderName:='Chair';
      Compatibility_records:=3;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_01_chair1.x3d';
      compatibility_string[2]:='Furniture1_02_chair2.x3d';
      compatibility_string[3]:='Furniture1_03_chair3.x3d';
    end;
    with PlaceholderCompatibility[7] do begin
      placeholderName:='book';
      Compatibility_records:=15;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      for i:=1 to Compatibility_Records do compatibility_string[i]:='BookCovers_'+inttostr(i)+'.x3d';
    end;
    with PlaceholderCompatibility[8] do begin
      placeholderName:='OpenBook';
      Compatibility_records:=23;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      for i:=1 to Compatibility_Records do compatibility_string[i]:='BooksPages_'+inttostr(i)+'.x3d';
    end;
    with PlaceholderCompatibility[9] do begin
      placeholderName:='BookCase';
      Compatibility_records:=1;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_04_bookCase.x3d';
    end;
    with PlaceholderCompatibility[10] do begin
      placeholderName:='table';
      Compatibility_records:=2;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      compatibility_string[1]:='Furniture1_11_squaretable.x3d';
      compatibility_string[2]:='Furniture1_12_roundtable.x3d';
    end;
    with PlaceholderCompatibility[11] do begin
      placeholderName:='Painting';
      Compatibility_records:=27;
      for i:=1 to Compatibility_Records do compatibility_RND[i]:=1;
      for i:=1 to 11 do compatibility_string[i]:='Paintings1_'+inttostr(i)+'.x3d';
      for i:=1 to 8 do compatibility_string[i+11]:='Paintings2_'+inttostr(i)+'.x3d';
      for i:=1 to 8 do compatibility_string[i+11+8]:='Paintings3_'+inttostr(i)+'.x3d';
    end;

   MaxPlaceholderAtlas:=11;
end;


end.
