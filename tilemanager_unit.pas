{Copyright (C) 2015 Yevhen Loza

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

unit TileManager_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, castle_base, castle_window, CastleWindow, castlescene,
  castlescenecore, castlescenemanager, castle3d, castlevectors, X3DNodes,
  {x3dload, }CastleControl, CastleCameras, CastleControls,

  //CastleGLUtils,
  CastleFilesUtils, CastleImages,

  Tile_var, generic_var, Generator, MazerMapParser;

const delta=0.2; //this is 'wall thickness' at wall editor
      tile_scale=32;

const face_types=4;  {0..face_types}
      floor_types=2;
      base_types=4;


type Map_Tile_type2 = record
  Tile_scene: TCastleScene;
//  Tile_PNG: array[1..maxtilesizez] of TCastleImage;
  Tile: Map_Tile_Type;
end;

Type DAtlas_record = record
  friendlyName:string;
  color:integer;    //for display
  //style:TBrushStyle;
end;

type

  { TForm1 }

  TForm1 = class(TForm)
    MakeMapImage: TButton;
    SymmetricEdit: TCheckBox;
    FaceAtlasBox: TComboBox;
    BaseAtlasBox: TComboBox;
    Label1: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ResetMapButton: TButton;
    Load_button: TButton;
    ResetCamera_button: TButton;
    ScreenShot_button: TButton;
    FixX3D_button: TButton;
    FixAll_button: TButton;
    SaveMapButton: TButton;
    CastleControl1: TCastleControl;
    ComboBox1: TComboBox;
    Image1: TImage;
    Memo1: TMemo;
    ScrollBar1: TScrollBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;

    procedure BaseAtlasBoxSelect(Sender: TObject);
    procedure FaceAtlasBoxSelect(Sender: TObject);
    procedure Load_buttonClick(Sender: TObject);
    procedure MakeMapImageClick(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton3Change(Sender: TObject);
    procedure ResetCamera_buttonClick(Sender: TObject);
    procedure ResetMapButtonClick(Sender: TObject);
    procedure ScreenShot_buttonClick(Sender: TObject);
    procedure FixX3D_buttonClick(Sender: TObject);
    procedure FixAll_buttonClick(Sender: TObject);
    procedure SaveMapButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Draw_Tile_Map;

    procedure RefreshFilesList;
    procedure Empty_Tile_Map;
    procedure FillAtlas;
    procedure MakeFaceAtlasBox;
    procedure MakeBaseAtlasBox;

    procedure Fix_x3d(filename:string);

    procedure ResetCamera;
    procedure ScrollBar1Change(Sender: TObject);
    procedure SymmetricEditChange(Sender: TObject);
    procedure TakeScreenshot;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  MyTile: Map_Tile_type2;
  Tile_transfrom:T3DTransform;
  CurrentFileName:string;
  currentZ:integer=0;

  FaceAtlas:array [0..face_types] of DAtlas_record;
  FloorAtlas:array [0..floor_types] of DAtlas_record;
  BaseAtlas:array [0..base_types] of DAtlas_record;
  FaceAtlasList,BaseAtlasList:array of integer;
  LastFaceAtlasIndex,LastFloorAtlasIndex,LastBaseAtlasIndex:integer;

  file1,file2:text;

  unsavedchanges:boolean=false;

implementation
{$R+}{$Q+}
{$R *.lfm}

{-----------------------------------------------------------------------------------}

//scan the folder for X3D tiles.
procedure TForm1.RefreshFilesList;
var Rec : TSearchRec;
begin
  combobox1.clear;
  if FindFirst (tiles_models_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then begin
    Load_button.enabled:=true;
    try
    repeat
      Combobox1.Items.Add(Rec.Name) ;
    until FindNext(Rec) <> 0;
    finally
      FindClose(Rec) ;
    end;
    combobox1.itemindex:=0;
  end else Load_button.enabled:=false;
end;

//well, this one is not needed, but let it be for now.
procedure TForm1.FaceAtlasBoxSelect(Sender: TObject);
begin
  LastFaceAtlasIndex:=FaceAtlasBox.ItemIndex;
end;

//save last item index
procedure TForm1.BaseAtlasBoxSelect(Sender: TObject);
begin
  if RadioButton1.Checked then LastBaseAtlasIndex:=BaseAtlasBox.ItemIndex else LastFloorAtlasIndex:=BaseAtlasBox.ItemIndex
end;

//Need to call it every time after edit base element radio is changed
//May add some check&lastmode variable to see if there is a need to refill the combobox
procedure TForm1.MakeBaseAtlasBox;
var i:integer;
begin
 BaseAtlasBox.Clear;
 if RadioButton1.checked then begin
   for i:=low(BaseAtlas) to high(BaseAtlas) do begin
     BaseAtlasBox.Items.Add(BaseAtlas[i].friendlyName);
   end;
   BaseAtlasBox.ItemIndex:=LastBaseAtlasIndex;
 end else begin
   for i:=low(FloorAtlas) to high(FloorAtlas) do begin
     BaseAtlasBox.Items.Add(FloorAtlas[i].friendlyName);
   end;
   BaseAtlasBox.ItemIndex:=LastFloorAtlasIndex;
 end;
end;

//only once is enough (the combobox is not changed)
procedure TForm1.MakeFaceAtlasBox;
var i:integer;
begin
  FaceAtlasBox.Clear;
  for i:=low(FaceAtlas) to high(FaceAtlas) do begin
    FaceAtlasBox.Items.Add(FaceAtlas[i].friendlyName);
  end;
  FaceAtlasBox.ItemIndex:=LastFaceAtlasIndex;
end;

//I think some day it should start from an ini file. But now that's more than enough
procedure TForm1.FillAtlas;
var i:integer;
begin
 with FaceAtlas[face_na] do begin
  FriendlyName:='n/a';
  color:=0;
 end;
 with FaceAtlas[face_wall] do begin
  FriendlyName:='WALL';
  color:=$FFFFFF;
 end;
 for i:=2 to high(FaceAtlas) do with FaceAtlas[i] do begin
  FriendlyName:='free #'+inttostr(i-1);
  color:=$990000*(high(FaceAtlas)-i) div (high(FaceAtlas))+$000099*i div (high(FaceAtlas))+$009900;
 end;

 with FloorAtlas[floor_na] do begin
  FriendlyName:='n/a';
  color:=0;
 end;
 with FloorAtlas[floor_wall] do begin
  FriendlyName:='BLOCK';
  color:=$FFFFFF;
 end;
 for i:=2 to high(FloorAtlas) do with FloorAtlas[i] do begin
  FriendlyName:='free #'+inttostr(i-1);
  color:=$990000*(high(FloorAtlas)-i) div (high(FloorAtlas))+$000099*i div (high(FloorAtlas))+$009900;
 end;

 //base atlas is a special case. Let it die like this.
 with BaseAtlas[tile_na] do begin
  FriendlyName:='n/a';
  color:=0;
 end;
 with BaseAtlas[1] do begin  ///TileWall = unused???!!!!
  FriendlyName:='WALL (unused)';
  color:=$FFFFFF;
 end;
 with BaseAtlas[tile_free] do begin
  FriendlyName:='FREE';
  color:=$444444;
 end;
 with BaseAtlas[Tile_Stairs_Down] do begin
  FriendlyName:='STAIRS DOWN';
  color:=$0000FF;
 end;
 with BaseAtlas[Tile_Stairs_Up] do begin
  FriendlyName:='STAIRS UP';
  color:=$00FF00;
 end;

 LastFaceAtlasIndex:=2;
 LastBaseAtlasIndex:=2;
 LastFloorAtlasIndex:=2;

 MakeFaceAtlasBox;
 MakeBaseAtlasBox;
end;

{--------------------------------------------------------------------}

procedure TForm1.Fix_x3d(filename:string); deprecated;
var S,texture_diffuse,texture_normal:string;
    i1,i2:integer;
begin
  //this procedure makes blender-exported x3ds to have normal map and anisotropic filtering.
  //just a brutal fix to blender exporter
  assignFile(file1,tiles_models_folder+filename);
  {$I-}
  reset(file1);
  {$I+}
  if ioresult=0 then begin
    AssignFile(file2,'a'+filename);
    rewrite(file2);
    repeat
      readln(file1,s);
      //seek image texture record
      if copy(trim(s),1,13)='<ImageTexture' then begin
        //current line is discarded
        //read image texture filename (not optimal!!! but fine for blender);
        readln(file1,s);
        s:=trim(s);
        i1:=0;
        repeat
          inc(i1);
        until copy(s,i1,1)='"'; //find start
        i2:=i1;
        repeat
          inc(i2);
        until copy(s,i2,1)='"'; //find end
        texture_diffuse:=copy(s,i1+1,i2-i1-1); //finally this is diffuse texture image filename
        //now replace texture 'diffuse' with 'normal' (try for typos like 'diffus') also)
        //non-safe!!!!!!!!
        i1:=0;
        repeat
          inc(i1);
        until copy(texture_diffuse,i1,4)='diff';
        texture_normal:=copy(texture_diffuse,1,i1-1)+'normal.tga';
        texture_diffuse:=copy(texture_diffuse,1,i1-1)+'diffuse.tga';

        readln(file1,s); //and delete one more line - blender specific
        //now write a multitexture to the file
        writeln(file2,'<MultiTexture DEF="IM" mode='+#39+'"DOTPRODUCT3" "MODULATE"'+#39+' >');
        writeln(file2,'<ImageTexture url='+#39+'"'+texture_normal+'"'+#39+'>');
        writeln(file2,'<TextureProperties anisotropicDegree="8" magnificationFilter="DEFAULT" minificationFilter="DEFAULT"/>');
        writeln(file2,'</ImageTexture>');
        writeln(file2,'<ImageTexture url='+#39+'"'+texture_diffuse+'"'+#39+'>');
        writeln(file2,'<TextureProperties anisotropicDegree="8" magnificationFilter="DEFAULT" minificationFilter="DEFAULT"/>');
        writeln(file2,'</ImageTexture>');
        writeln(file2,'</MultiTexture>');
      end else
        writeln(file2,s);
    until eof(file1);
    closeFile(file2);
    closeFile(file1);
  end else showmessage('Error reading file!');
end;

procedure TForm1.FixX3D_buttonClick(Sender: TObject);
begin
  Fix_x3d(combobox1.Items[combobox1.itemindex]);
end;

procedure TForm1.FixAll_buttonClick(Sender: TObject);
var im:integer;
begin
  for im:=0 to combobox1.Items.Count-1 do Fix_x3d(combobox1.Items[im]);
end;


{---------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
 doublebuffered:=true;
 FillAtlas;
 RefreshFilesList;
end;

{---------------------------------------------------------------------------}

//this is image click at tile map. Enables tile editing - faces, base and floors
procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var scalex,scaley:double;
    dx,dy:double;
    x1,y1,z1,ax,ay:integer;
    angle1,face_value,base_value:integer;
begin
  //working with rectagonal grid only (for now, maybe forever :))
 currentZ:=Scrollbar1.Position;
 if MyTile.Tile.Tile3D<>nil then begin
   scalex:=image1.width/MyTile.Tile.tilesizex;
   scaley:=image1.height/MyTile.Tile.tilesizey;
   z1:=currentZ; //todo
   x1:=trunc(X / scalex)+1;
   y1:=trunc(Y / scaley)+1;
   //a tiny check, just in case...
   if x1<1 then x1:=1;
   if y1<1 then y1:=1;
   if z1<1 then z1:=1;
   if x1>MyTile.Tile.tilesizex then x1:=MyTile.Tile.tilesizex;
   if y1>MyTile.Tile.tilesizey then y1:=MyTile.Tile.tilesizey;
   if z1>MyTile.Tile.tilesizez then z1:=MyTile.Tile.tilesizez;
   //memo1.lines.add('click: '+inttostr(x1)+'x'+inttostr(y1));
   dx:=X/scalex-x1+1;
   dy:=Y/scaley-y1+1;
   //memo1.lines.add('d: '+floattostr(dx)+'x'+floattostr(dy));
   if dx<delta then ax:=-1 else begin
     if dx>1-delta then ax:=1 else ax:=0;
   end;
   if dy<delta then ay:=-1 else begin
     if dy>1-delta then ay:=1 else ay:=0;
   end;
   //memo1.lines.add('a: '+inttostr(ax)+'x'+inttostr(ay));
   if abs(ax)+abs(ay)>1 then exit; //yeah, ugly, but I don't want to take care of it later
   // and don't want to work with diagonals, maybe later

   //grab current palette values from the ComboBoxes
   base_value:=BaseAtlasBox.ItemIndex;
   face_value:=FaceAtlasBox.ItemIndex;

   if (ax=0) and (ay=0) then begin
     //edit central spot
     if Radiobutton2.checked then begin
       //ceiling
       if mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]=floor_wall then mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]:=base_value else mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]:=floor_wall;
       if (SymmetricEdit.checked) and (z1>1) then
         mytile.Tile.TileMap[x1,y1,z1-1].floor[angle_stairs_down]:=mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up];
     end else
     if Radiobutton3.checked then begin
       //floor
       if mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]=floor_wall then mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]:=base_value else mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]:=floor_wall;
       if (SymmetricEdit.checked) and (z1<myTile.Tile.tilesizez) then
         mytile.Tile.TileMap[x1,y1,z1+1].floor[angle_stairs_up]:=mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down];
     end else begin
       //base tile
       if mytile.Tile.TileMap[x1,y1,z1].base=base_value then begin
         //clear the tile to n/a
         mytile.Tile.TileMap[x1,y1,z1].base:=tile_na;
         if SymmetricEdit.checked then begin
           //and reset all the walls around it
           for angle1:=0 to maxangles do mytile.Tile.TileMap[x1,y1,z1].faces[angle1]:=face_na;
           mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]:=floor_na;
           mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]:=floor_na;
         end;
       end else begin
         //assign values and create walls around it if face=face_na;
         mytile.Tile.TileMap[x1,y1,z1].base:=base_value;
         if SymmetricEdit.checked then begin
           //I reset all faces to 'face-free' here because it's meant to be inside the tile map, so no external links will be necessary, just to point that there is a passage here
           if mytile.Tile.TileMap[x1,y1,z1].faces[angle_top]=face_na then begin
             if y1=1 then mytile.Tile.TileMap[x1,y1,z1].faces[angle_top]:=face_wall else mytile.Tile.TileMap[x1,y1,z1].faces[angle_top]:=face_free;
           end;
           if mytile.Tile.TileMap[x1,y1,z1].faces[angle_bottom]=face_na then begin
             if y1=myTile.Tile.TileSizeY then mytile.Tile.TileMap[x1,y1,z1].faces[angle_bottom]:=face_wall else mytile.Tile.TileMap[x1,y1,z1].faces[angle_bottom]:=face_free;
           end;
           if mytile.Tile.TileMap[x1,y1,z1].faces[angle_left]=face_na then begin
             if x1=1 then mytile.Tile.TileMap[x1,y1,z1].faces[angle_left]:=face_wall else mytile.Tile.TileMap[x1,y1,z1].faces[angle_left]:=face_free;
           end;
           if mytile.Tile.TileMap[x1,y1,z1].faces[angle_right]=face_na then begin
             if x1=myTile.Tile.TileSizeX then mytile.Tile.TileMap[x1,y1,z1].faces[angle_right]:=face_wall else mytile.Tile.TileMap[x1,y1,z1].faces[angle_right]:=face_free;
           end;
           if mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]=floor_na then begin
             if z1=1 then mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]:=floor_wall else mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_up]:=floor_free;
           end;
           if mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]=floor_na then begin
             if z1=myTile.Tile.TileSizeZ then mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]:=floor_wall else mytile.Tile.TileMap[x1,y1,z1].floor[angle_stairs_down]:=floor_free;
           end;
           //moreover, we have to check if this is a stairs-up-down and if the tile is correct
           if (mytile.Tile.TileMap[x1,y1,z1].base=tile_stairs_up) then begin
             if z1>1 then
               mytile.Tile.TileMap[x1,y1,z1-1].base:=tile_stairs_down
             else showmessage('The ladder goes UP beyond the tile border!');
           end;
           if (mytile.Tile.TileMap[x1,y1,z1].base=tile_stairs_down) then begin
             if z1<MyTile.Tile.TileSizeZ then
               mytile.Tile.TileMap[x1,y1,z1+1].base:=tile_stairs_up
             else showmessage('The ladder goes DOWN beyond the tile border!');
           end;
         end;
       end;
     end;
   end else begin
     //edit walls
     //get angle from ax,ay
     if ay<0 then angle1:=angle_top else
     if ay>0 then angle1:=angle_bottom else
     if ax<0 then angle1:=angle_left else
     if ax>0 then angle1:=angle_right;
     //todo diferent faces values here
     if mytile.Tile.TileMap[x1,y1,z1].faces[angle1]=face_wall then
       mytile.Tile.TileMap[x1,y1,z1].faces[angle1]:=face_value else mytile.Tile.TileMap[x1,y1,z1].faces[angle1]:=face_wall;
     if symmetricEdit.Checked then begin
       if ((x1+a_dx(angle1))>=1) and ((x1+a_dx(angle1))<=MyTile.Tile.TileSizeX) and
          ((y1+a_dy(angle1))>=1) and ((y1+a_dy(angle1))<=MyTile.Tile.TileSizeY) then begin
          if (mytile.Tile.TileMap[x1+a_dx(angle1),y1+a_dy(angle1),z1].base<>tile_na) and (mytile.Tile.TileMap[x1,y1,z1].base<>tile_na) then
             mytile.Tile.TileMap[x1+a_dx(angle1),y1+a_dy(angle1),z1].faces[InverseAngle(angle1)]:=mytile.Tile.TileMap[x1,y1,z1].faces[angle1];
       end;
     end;
   end;
   unsavedchanges:=true;
   draw_tile_map;
 end;
end;


{-----------------------------------------------------------------------}

//Draw the tile map.
procedure TForm1.Draw_Tile_Map;
var ix,iy,iz:integer;
    x1,y1,x2,y2,x3,y3,x4,y4:integer;
    scalex,scaley:double;
begin
 //currentZ:=scrollbar1.position;
 if MyTile.Tile.Tile3D=nil then exit;//ugly runaway if no tile loaded

 Label1.Caption:=inttostr(currentz);
 with image1.canvas do begin
   brush.color:=0;
   fillrect(0,0,image1.width,image1.height);
   iz:=currentZ;
   scalex:=image1.width/MyTile.Tile.TileSizex;
   scaley:=image1.height/MyTile.Tile.TileSizey;
   for ix:=1 to MyTile.Tile.TileSizex do
    for iy:=1 to MyTile.Tile.TileSizey do begin
      x1:=round((ix-1)*scalex);
      y1:=round((iy-1)*scaley);
      x2:=round(ix*scalex);
      y2:=round(iy*scaley);
      x3:=round((ix-(1-delta))*scalex);
      y3:=round((iy-(1-delta))*scaley);
      x4:=round((ix-delta)*scalex);
      y4:=round((iy-delta)*scaley);

      //draw central spot depending if it is floor, ceiling or else
      {if radiobutton1.checked then begin}
        brush.style:=bsSolid;
        brush.color:=BaseAtlas[mytile.Tile.TileMap[ix,iy,iz].base].color;
        fillrect(x3,y3,x4,y4);  //BUG: for some reasons it does not clear the base for ceiling and floor...
      {end else}
      if radiobutton2.checked then begin
        brush.style:=bsCross;
        brush.color:=FloorAtlas[mytile.Tile.TileMap[ix,iy,iz].floor[angle_stairs_up]].color;
        fillrect(x3,y3,x4,y4);
      end else if radiobutton3.checked then begin
        brush.style:=bsDiagCross;
        brush.color:=FloorAtlas[mytile.Tile.TileMap[ix,iy,iz].floor[angle_stairs_down]].color;
        fillrect(x3,y3,x4,y4);
      end;
      //draw tile faces
      with mytile.Tile.TileMap[ix,iy,iz] do begin
        brush.style:=bsSolid;
        brush.color:=FaceAtlas[faces[angle_top]].color;
        fillrect(x3,y1,x4,y3); //top

        brush.color:=FaceAtlas[faces[angle_left]].color;
        fillrect(x1,y3,x3,y4); //left

        brush.color:=FaceAtlas[faces[angle_bottom]].color;
        fillrect(x3,y4,x4,y2); //bottom

        brush.color:=FaceAtlas[faces[angle_right]].color;
        fillrect(x4,y3,x2,y4); //right
      end;
    end;
 end;
end;

{------------------------------------------------------------------------}
//Return the camera to strictly oriented position.
//It's really important to make the tile map correctly
procedure TForm1.resetCamera;
begin
 if (CastleControl1.scenemanager.camera<>nil) and (myTile.Tile.Tile3D<>nil) then begin
   //set upthe camera
   CastleControl1.scenemanager.camera.setView(MyTile.Tile_Scene.BoundingBox.middle+Vector3Single(0,0,MyTile.Tile_Scene.BoundingBox.maxsize+1),Vector3Single(0,0,-1),Vector3Single(0,1,0));
  //  CastleControl1.scenemanager.camera.ProjectionMatrix:=OrthoProjection(-1, 1, -1, 1, 0.1, 100);//OrthogonalProjection;
   CastleControl1.scenemanager.camera.input:=TCamera.DefaultInput;
   CastleControl1.update;
 end;
end;

{----------------------------------------------------}

// Another Lazarus bug??? Why can't I assign all three radio-s to 1 procedure, which works fine for other elements like checkboxes
procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  MakeBaseAtlasBox;
  Draw_Tile_Map;
end;

procedure TForm1.RadioButton2Change(Sender: TObject);
begin
  MakeBaseAtlasBox;
  Draw_Tile_Map;
end;

procedure TForm1.RadioButton3Change(Sender: TObject);
begin
  MakeBaseAtlasBox;
  Draw_Tile_Map;
end;

//this scrollbar changes z of the tile if TileSizeZ>1
procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  if currentz<>scrollbar1.position then begin
    currentz:=scrollbar1.position;
    Draw_Tile_Map;
  end;
end;

procedure TForm1.SymmetricEditChange(Sender: TObject);
begin
  if not SymmetricEdit.checked then showmessage('It is highly recommended to leave Symmetric Edit on, unles you know what you are doing. The tile must be 2-abundantly consistent and Symmetric Edit tries to do as much as possible of that automatically.');
end;

{------------------------------------------------------------------------}

//take a screenshot of the current render. Yet not needed.
procedure TForm1.TakeScreenshot;
begin
 //make and save a screenshot
 //maybe make them tiles 1x1 photo?
 SaveImage(CastleControl1.SaveScreen,CurrentFileName+'screen.png');
end;

{------------------------------------------------------------------------}

//this procedure 'clears' the tile map and automatically sets it empty inside and surrounded by walls
Procedure TForm1.Empty_Tile_Map;
var jx,jy,jz,j:integer;
begin
 if not MyTile.Tile.blocker then
 with MyTile.Tile do
  for jx:=1 to TileSizeX do
   for jy:=1 to TileSizeY do
    for jz:=1 to TileSizeZ do with TileMap[jx,jy,jz] do begin
      base:=tile_free;
      for j:=0 to maxangles do faces[j]:=face_free;
      if jx=1 then faces[angle_left]:=face_wall;
      if jy=1 then faces[angle_top]:=face_wall;
      if jx=TileSizeX then faces[angle_Right]:=face_wall;
      if jy=TileSizeY then faces[angle_bottom]:=face_wall;
      for j:=1 to 2 do floor[j]:=floor_free;
      if jz=1 then floor[angle_stairs_up]:=floor_wall;
      if jz=TileSizeZ then floor[angle_stairs_down]:=floor_wall;
    end
  else with MyTile.Tile.tileMap[1,1,1] do begin
    base:=Tile_na;
    for j:=0 to maxangles do faces[j]:=face_na;
    for j:=1 to 2 do floor[j]:=floor_na;
  end;
end;

{------------------------------------------------------------------------}

//load a new tile
procedure TForm1.Load_buttonClick(Sender: TObject);
//Const OrthogonalProjection:TMatrix4Single=((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,0));
var URL:String;
begin
 if unsavedChanges then begin
   if MessageDlg('Unsaved changes?', 'Your changes are unsaved! Really load a new tile?', mtConfirmation, [mbYes, mbNo],0) = mrNo then exit;
 end;
   memo1.clear;

  //if something is loaded - discard it.
  if Tile_transfrom<>nil then CastleControl1.Scenemanager.items.remove(Tile_transfrom);
  // load the tile
  CurrentFileName:=combobox1.Items[combobox1.itemindex];
  MyTile.Tile:=LoadTile(CurrentFileName);
  //not optimal search for blockers
  if (MyTile.Tile.tilesizex=1) and (MyTile.Tile.tilesizey=1) and (MyTile.Tile.tilesizez=1) and (MyTile.Tile.TileMap[1,1,1].base=tile_na) then
    MyTile.Tile.blocker:=true
  else
    MyTile.Tile.blocker:=false;
  if ErrorString<>'' then showmessage('ERROR!!! '+errorString);
  MyTile.Tile_Scene:=TCastleScene.create(CastleControl1);
  MyTile.Tile_scene.spatial := [ssRendering, ssDynamicCollisions];
  MyTile.Tile_scene.processevents:=true;
  MyTile.Tile_scene.Load(myTile.Tile.Tile3D,true);
  Tile_transfrom:=T3DTransform.Create(CastleControl1.SceneManager);
  Tile_transfrom.add(MyTile.Tile_scene);
  Tile_transfrom.translation:=Vector3Single(0,0,0);
  Tile_transfrom.scale:=Vector3Single(1,1,1);
  CastleControl1.Scenemanager.items.add(Tile_transfrom);

  ResetCamera;

  //now prepare the tile for work

  //  if no tilemap to load
  If not TileMapLoaded then begin
    MyTile.Tile.TileSizex:=round(MyTile.Tile_Scene.BoundingBox.sizex/2);
    MyTile.Tile.TileSizey:=round(MyTile.Tile_Scene.BoundingBox.sizey/2{+0.5}); //whhyyyyyyyy it falied at 1 of ~150 tiles ?????
    MyTile.Tile.TileSizez:=round(MyTile.Tile_Scene.BoundingBox.sizez/2);
    //not optimal search for blockers.
    MyTile.Tile.blocker:=false;

    if (MyTile.Tile.tilesizex=0) then begin
      MyTile.Tile.Blocker:=true;
    end;
    if (MyTile.Tile.tilesizey=0) then begin
      MyTile.Tile.Blocker:=true;
    end;
    if (MyTile.Tile.tilesizez=0) then begin
      MyTile.Tile.Blocker:=true;
    end;
    if MyTile.Tile.Blocker then begin
      MyTile.Tile.tilesizex:=1;
      MyTile.Tile.tilesizey:=1;
      MyTile.Tile.tilesizez:=1;
    end;
    Empty_Tile_map;
  end;
  image1.width:=tile_scale*MyTile.Tile.TileSizex;
  image1.height:=tile_scale*MyTile.Tile.TileSizey;
  //fix LazarusBug which is actually meant to be done automatically on OnResize but isn't
  image1.Picture.Bitmap.SetSize(image1.Width,image1.height);
  //set height and width of controls based on tile map size
  scrollbar1.height:=image1.height-label1.height;
  //memo1.lines.add(inttostr(MyTile.Tile.TileSizex)+'x'+ inttostr(MyTile.Tile.TileSizey)+'x'+ inttostr(MyTile.Tile.TileSizez));
  //memo1.lines.add(inttostr(image1.width)+'x'+inttostr(image1.height));

  //initialize z control
  currentz:=MyTile.Tile.TileSizez;
  Scrollbar1.min:=1;
  scrollbar1.max:=MyTile.Tile.TileSizeZ;
  Scrollbar1.position:=MyTile.Tile.TileSizez;
  if MyTile.Tile.TileSizeZ=1 then begin
    scrollbar1.visible:=false;
    Label1.visible:=false;
  end else begin
    scrollbar1.visible:=true;
    Label1.visible:=true;
  end;
  draw_tile_map;
  unsavedchanges:=false;
end;

//this button creates a tile png representation to use in minimap. On one hand it should be a 'top-down' screenshot
//however, at this moment I'm absolutely enough with this simple 'duplication' of the tile map.
//based on caeles CC0 template from OpenGameArt
procedure TForm1.MakeMapImageClick(Sender: TObject);
var PNG_tileset,PNG_up,PNG_down,PNG_updown,PNG_map:TCastleImage;
    ix,iy,iz:integer;
    tx,ty:integer;    // tile position in a PNG file
    corner_tl,corner_tr,corner_bl,corner_br:boolean; //are the corners filled? TL = top left, BR = bottom right etc,
    arrowup,arrowdown:boolean;

function CheckCorner(cornerx,cornery,cornerz,facea,faceb:integer):boolean;
  function getCornerFace(cox,coy,coz,myface:integer):boolean;
    begin
     GetCornerFace:=false;
     if (cox>0) and (coy>0) and (cox<=MyTile.Tile.TileSizex) and (coy<=MyTile.Tile.TileSizey) then begin
       if MyTile.Tile.TileMap[cox,coy,coz].faces[myface]=face_wall then GetCornerFace:=true else
       if (cox+a_dx(myface)>0) and (coy+a_dy(myface)>0) and (cox+a_dx(myface)<=MyTile.Tile.TileSizeX) and (coy+a_dy(myface)<=MyTile.Tile.TileSizeY) then
         if MyTile.Tile.TileMap[cox+a_dx(myface),coy+a_dy(myface),coz].faces[inverseAngle(myface)]=face_wall then GetCornerFace:=true
     end else GetCornerFace:=true;
    end;
 begin
  CheckCorner:=GetCornerFace(cornerx+a_dx(facea),cornery+a_dy(facea),cornerz,facea) or GetCornerFace(cornerx+a_dx(faceb),cornery+a_dy(faceb),cornerz,faceb);
 end;

begin
 if not MyTile.Tile.Blocker then begin
   //I don't care about efficiency or memory use here. It's just to make the tiles once.
   PNG_Tileset:=LoadImage(etc_models_folder+'template_CC0_by_caeles.png', [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_UP:=LoadImage(etc_models_folder+'template_CC0_by_caeles_up.png', [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_Down:=LoadImage(etc_models_folder+'template_CC0_by_caeles_down.png', [TRGBAlphaImage]) as TRGBAlphaImage;
   PNG_UpDown:=LoadImage(etc_models_folder+'template_CC0_by_caeles_updown.png', [TRGBAlphaImage]) as TRGBAlphaImage;
   //create map image.
   for iz:=1 to MyTile.Tile.TileSizez do begin
    PNG_map:=LoadImage(etc_Models_Folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;
    PNG_map.setsize((MyTile.Tile.TileSizeX)*16,(MyTile.Tile.TileSizeY)*16,1);
    PNG_map.Clear(Vector4Byte(0,0,0,0));
    for ix:=1 to MyTile.Tile.TileSizeX do
     for iy:=1 to MyTile.Tile.TileSizeY do with MyTile.Tile.TileMap[ix,iy,iz] do begin
       if base<>tile_na then begin
       //No-corner tiles
       if (faces[angle_top]=face_wall) and (faces[angle_bottom]=face_wall) then begin
         if (faces[angle_left]=face_wall) then begin
           // |=
           tx:=2;ty:=1;
           if (faces[angle_right]=face_wall) then begin
             //all blocked
             tx:=1;ty:=1;
           end;
         end else if (faces[angle_right]=face_wall) then begin
           // =|
           tx:=5;ty:=7;
         end else begin
           // =
           tx:=5;ty:=6;
         end;
       end else if (faces[angle_left]=face_wall) and (faces[angle_right]=face_wall) then begin
         if (faces[angle_top]=face_wall) then begin
           // П
           tx:=1;ty:=2;
         end else if (faces[angle_bottom]=face_wall) then begin
           // |_|
           tx:=7;ty:=5;
         end else begin
           // ||
           tx:=6;ty:=5;
         end;
       end else begin
         //corner_tiles;
         //I calculate all corners just for 'less code' reason
         Corner_TL:=CheckCorner(ix,iy,iz,angle_top,angle_left);
         Corner_TR:=CheckCorner(ix,iy,iz,angle_top,angle_right);
         Corner_BL:=checkCorner(ix,iy,iz,angle_bottom,angle_left);
         Corner_BR:=CheckCorner(ix,iy,iz,angle_bottom,angle_right);
         if faces[angle_left]=face_wall then begin
           if faces[angle_top]=face_wall then begin
             //Г
             if corner_BR then begin
               tx:=4;ty:=5;
             end else begin
               tx:=2;ty:=2;
             end;
           end else if faces[angle_bottom]=face_wall then begin
             //|_
             if corner_TR then begin
               tx:=1;ty:=7;
             end else begin
               tx:=6;ty:=7;
             end;
           end else begin
             // |:
             if corner_TR and corner_BR then begin
               tx:=1;ty:=3;
             end else if corner_TR then begin
               tx:=1;ty:=4;
             end else if corner_BR then begin
               tx:=1;ty:=6;
             end else begin
               tx:=1;ty:=5;
             end;
           end;
         end else if faces[angle_right]=face_wall then begin
           if faces[angle_top]=face_wall then begin
             //7
             if corner_BL then begin
               tx:=7;ty:=1;
             end else begin
               tx:=7;ty:=6;
             end;
           end else if faces[angle_bottom]=face_wall then begin
             //_|
             if corner_TL then begin
               tx:=5;ty:=5;
             end else begin
               tx:=7;ty:=7;
             end;
           end else begin
             // :|
             if corner_TL and corner_BL then begin
               tx:=7;ty:=4;
             end else if corner_TL then begin
               tx:=7;ty:=2;
             end else if corner_BL then begin
               tx:=7;ty:=3;
             end else begin
               tx:=3;ty:=5;
             end;
           end;
         end else begin
           if faces[angle_top]=face_wall then begin
             //--
             //..
             if corner_BL and corner_BR then begin
               tx:=3;ty:=1;
             end else if corner_BL then begin
               tx:=4;ty:=1;
             end else if corner_BR then begin
               tx:=6;ty:=1;
             end else begin
               tx:=5;ty:=1;
             end;
           end else if faces[angle_bottom]=face_wall then begin
             //..
             //__
             if corner_TL and corner_TR then begin
               tx:=4;ty:=7;
             end else if corner_TL then begin
               tx:=2;ty:=7;
             end else if corner_TR then begin
               tx:=3;ty:=7;
             end else begin
               tx:=4;ty:=4;
             end;
           end else begin
             //all empty
             if corner_TL and corner_TR then begin
               if corner_BL then begin
                 //three corners TL TR BL
                 tx:=6;ty:=6;
                 if corner_BR then begin
                   //all corners
                   tx:=4;ty:=6;
                 end;
               end else if corner_BR then begin
                 //three corners TL TR BR
                 tx:=3;ty:=2;
               end else begin
                 //only upper corners
                 tx:=2;ty:=4;
               end;
             end else if corner_BL and corner_BR then begin
               if corner_TL then begin
                 //three corners BL BR TL
                 tx:=2;ty:=3;
               end else if corner_TR then begin
                 //three corners BL BR TR
                 tx:=6; ty:=4;
               end else begin
                 //only lower corners
                 tx:=5;ty:=4;
               end;
             end else if corner_TL then begin
               if corner_BR then begin
                 // "\"
                 tx:=3;ty:=4;
               end else if corner_BL then begin
                 // ": "
                 tx:=4;ty:=2;
               end else begin
                 //only single corner
                 tx:=4;ty:=3;
               end;
             end else if corner_TR then begin
               if corner_BL then begin
                 // "/"
                 tx:=3;ty:=3;
               end else if corner_BR then begin
                 // " :"
                 tx:=3;ty:=6;
               end else begin
                 //only single corner
                 tx:=6;ty:=2;
               end;
             end else if corner_BL then begin
               //just a single variant left
               tx:=2;ty:=6;
             end else if corner_BR then begin
               //just a single variant left
               tx:=6;ty:=3;
             end else begin
               //all empty, no corners
               tx:=5;ty:=2;
             end;
           end;
         end;
       end;
     end else begin
       //n/a tile (transparent)
       tx:=7;ty:=5;
     end;

     //now, check if there are stairs up/down nearby?
     arrowup:=false;arrowdown:=false;
     if base=Tile_stairs_up then arrowup:=true;
     if base=Tile_stairs_down then arrowdown:=true;
     if (ix-1>0) and (MyTile.Tile.TileMap[ix,iy,iz].faces[angle_left]<>face_wall) then begin
       if MyTile.Tile.TileMap[ix-1,iy,iz].base=Tile_stairs_up then arrowup:=true;
       if MyTile.Tile.TileMap[ix-1,iy,iz].base=Tile_stairs_down then arrowdown:=true;
     end;
     if (ix+1<=MyTile.Tile.TileSizeX) and (MyTile.Tile.TileMap[ix,iy,iz].faces[angle_right]<>face_wall) then begin
       if MyTile.Tile.TileMap[ix+1,iy,iz].base=Tile_stairs_up then arrowup:=true;
       if MyTile.Tile.TileMap[ix+1,iy,iz].base=Tile_stairs_down then arrowdown:=true;
     end;
     if (iy-1>0) and (MyTile.Tile.TileMap[ix,iy,iz].faces[angle_top]<>face_wall) then begin
       if MyTile.Tile.TileMap[ix,iy-1,iz].base=Tile_stairs_up then arrowup:=true;
       if MyTile.Tile.TileMap[ix,iy-1,iz].base=Tile_stairs_down then arrowdown:=true;
     end;
     if (iy+1<=MyTile.Tile.TileSizeY) and (MyTile.Tile.TileMap[ix,iy,iz].faces[angle_bottom]<>face_wall) then begin
       if MyTile.Tile.TileMap[ix,iy+1,iz].base=Tile_stairs_up then arrowup:=true;
       if MyTile.Tile.TileMap[ix,iy+1,iz].base=Tile_stairs_down then arrowdown:=true;
     end;
     //place the tile at tx,ty to the resulting image;
     //PNG_map.DrawFrom(PNG_Tileset,(ix-1)*16,(iy-1)*16,(tx-1)*16,(ty-1)*16,16,16,dmBlend);
     if arrowUp and arrowDown then
       PNG_map.DrawFrom(PNG_updown,(ix-1)*16,PNG_map.Height-(iy)*16,(tx-1)*16,PNG_updown.Height-(ty)*16,16,16,dmBlend)
     else if arrowUp then
       PNG_map.DrawFrom(PNG_up,(ix-1)*16,PNG_map.Height-(iy)*16,(tx-1)*16,PNG_up.Height-(ty)*16,16,16,dmBlend)
     else if arrowDown then
       PNG_map.DrawFrom(PNG_down,(ix-1)*16,PNG_map.Height-(iy)*16,(tx-1)*16,PNG_down.Height-(ty)*16,16,16,dmBlend)
     else
       PNG_map.DrawFrom(PNG_Tileset,(ix-1)*16,PNG_map.Height-(iy)*16,(tx-1)*16,PNG_tileset.Height-(ty)*16,16,16,dmBlend);
    end;
    SaveImage(PNG_map,CurrentFileName+'_'+inttostr(iz)+'.png');
    FreeAndNil(PNG_map);
   end;
   FreeAndNil(PNG_tileset);FreeAndNil(PNG_up);FreeAndNil(PNG_down);FreeAndNil(PNG_updown);
 end else begin
   if MyTile.Tile.TileMap[1,1,1].faces[angle_top]>=face_free then
     PNG_map:=LoadImage(etc_models_folder+'blocker_bottom.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if MyTile.Tile.TileMap[1,1,1].faces[angle_bottom]>=face_free then
     PNG_map:=LoadImage(etc_models_folder+'blocker_top.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if MyTile.Tile.TileMap[1,1,1].faces[angle_left]>=face_free then
     PNG_map:=LoadImage(etc_models_folder+'blocker_right.png', [TRGBAlphaImage]) as TRGBAlphaImage
   else
   if MyTile.Tile.TileMap[1,1,1].faces[angle_right]>=face_free then
     PNG_map:=LoadImage(etc_models_folder+'blocker_left.png', [TRGBAlphaImage]) as TRGBAlphaImage;

   if png_map<>nil then begin
     SaveImage(PNG_map,CurrentFileName+'_1.png');
     FreeAndNil(PNG_map);
   end;
 end;
end;

{----------------------------------------------}

procedure TForm1.ResetCamera_buttonClick(Sender: TObject);
begin
  ResetCamera;
end;

procedure TForm1.ResetMapButtonClick(Sender: TObject);
begin
  if MessageDlg('Clear tile?', 'Clear tile map?', mtConfirmation, [mbYes, mbNo],0) = mrYes then begin
    Empty_Tile_map;
    draw_tile_map;
    unsavedchanges:=false;
  end;
end;

procedure TForm1.ScreenShot_buttonClick(Sender: TObject);
begin
  TakeScreenshot;
end;

procedure TForm1.SaveMapButtonClick(Sender: TObject);
var ix,iy,iz,j:integer;
begin
 assignFile(file1,tiles_models_folder+CurrentFileName+'.map');
 rewrite(file1);
  writeln(file1,TileSizeX_Record,MyTile.Tile.TileSizeX);
  writeln(file1,TileSizeY_Record,MyTile.Tile.TileSizeY);
  writeln(file1,TileSizeZ_Record,MyTile.Tile.TileSizeZ);
  for ix:=1 to MyTile.Tile.TileSizex do
   for iy:=1 to MyTile.Tile.TileSizey do
    for iz:=1 to MyTile.Tile.TileSizez do begin
      writeln(file1,'<',ix,',',iy,',',iz,'>');
      writeln(file1,TileBase_record,mytile.Tile.TileMap[ix,iy,iz].base);
      writeln(file1,TileFloorUp_record,mytile.Tile.TileMap[ix,iy,iz].floor[angle_stairs_up]);
      writeln(file1,TileFloorDown_record,mytile.Tile.TileMap[ix,iy,iz].floor[angle_stairs_down]);
      for j:=0 to maxangles do
        writeln(file1,'[',j,']',mytile.Tile.TileMap[ix,iy,iz].faces[j]);
    end;
 closeFile(file1);
 unsavedChanges:=false;
end;

{------------------------------------------------}


end.

