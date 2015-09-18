unit TileManager_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, castle_base, castle_window, CastleWindow, castlescene,
  castlescenecore, castlescenemanager, castle3d, castlevectors, X3DNodes,
  x3dload, CastleControl, CastleCameras,

  //CastleGLUtils,
  CastleFilesUtils, CastleImages,

  Tile_var, generic_var, Generator;

const delta=0.15;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CastleControl1: TCastleControl;
    ComboBox1: TComboBox;
    Image1: TImage;
    Memo1: TMemo;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RefreshFilesList;
    procedure Draw_Tile_Map;
    function face_color(face_type:integer):integer;

    procedure Empty_Tile_Map;

    procedure Fix_x3d(filename:string);

    procedure ResetCamera;
    procedure TakeScreenshot;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  MyTile: Map_Tile_type;
  Tile_transfrom:T3DTransform;

  file1,file2:text;

implementation
{$R+}{$Q+}
{$R *.lfm}

{-----------------------------------------------------------------------------------}

procedure TForm1.RefreshFilesList;
var Rec : TSearchRec;
begin
  combobox1.clear;
  if FindFirst (models_folder + '*.x3d', faAnyFile - faDirectory, Rec) = 0 then begin
    button1.enabled:=true;
    try
    repeat
      Combobox1.Items.Add(Rec.Name) ;
    until FindNext(Rec) <> 0;
    finally
      FindClose(Rec) ;
    end;
    combobox1.itemindex:=0;
  end else button1.enabled:=false;
end;

{--------------------------------------------------------------------}

procedure TForm1.Fix_x3d(filename:string);
var S,texture_diffuse,texture_normal:string;
    i1,i2:integer;
begin
  //this procedure makes blender-exported x3ds to have normal map and anisotropic filtering.
  //just a brutal fix to blender exporter
  assignFile(file1,models_folder+filename);
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

{---------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RefreshFilesList;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var scalex,scaley:double;
    ix,iy,iz:integer;
    dx,dy:byte;
    ptr:^integer;
begin
 if Tile_transfrom<>nil then begin
   scalex:=image1.width/MyTile.tilesizex;
   scaley:=image1.height/MyTile.tilesizey;
   iz:=1;
   ix:=round(X/scalex);
   iy:=round(Y/scaley);
   //determine shift from the center
   //*********** dx:=X-ix*scalex;
   dx:=round(3*x/scalex) mod 3;  {0,1,2}
   dy:=round(3*y/scaley) mod 3;  {0,1,2}
   if (dx=1) and (dy=1) then begin
     //central spot edit
   end else begin
     //if (dx=0) and (dy=0) then
   end;
 end;
end;


{-----------------------------------------------------------------------}

function Tform1.face_color(face_type:integer):integer;
begin
  case face_type of
    face_na:face_color:=0;
    face_wall:Face_color:=$FFFFFF;
    face_free:face_color:=$999999;
  end;
end;
{-----------------------------------------------------------------------}

procedure TForm1.Draw_Tile_Map;
var ix,iy,iz:integer;
    x1,y1,x2,y2,x3,y3,x4,y4:integer;
    scalex,scaley:double;
begin
 with image1.canvas do begin
   brush.color:=0;
   fillrect(0,0,image1.width,image1.height);
   iz:=1;
   scalex:=image1.width/MyTile.tilesizex;
   scaley:=image1.height/MyTile.tilesizey;
   for ix:=1 to MyTile.tilesizex do
    for iy:=1 to MyTile.tilesizey do begin
      x1:=round((ix-1)*scalex);
      y1:=round((iy-1)*scaley);
      x2:=round(ix*scalex);
      y2:=round(iy*scaley);
      x3:=round((ix-(1-delta))*scalex);
      y3:=round((iy-(1-delta))*scaley);
      x4:=round((ix-delta)*scalex);
      y4:=round((iy-delta)*scaley);
      brush.color:=$99FF99;
      fillrect(x3,y3,x4,y4); //center
      with MyTile.tileMap[ix,iy,iz] do begin

        brush.color:=face_color(faces[angle_top]);
        fillrect(x3,y1,x4,y3); //top

        brush.color:=face_color(faces[angle_left]);
        fillrect(x1,y3,x3,y4); //left

        brush.color:=face_color(faces[angle_bottom]);
        fillrect(x3,y4,x4,y2); //bottom

        brush.color:=face_color(faces[angle_right]);
        fillrect(x4,y3,x2,y4); //right
      end;
    end;
 end;
end;

{------------------------------------------------------------------------}
procedure TForm1.resetCamera;
begin
 //set up the camera
 CastleControl1.scenemanager.camera.setView(MyTile.Tile_Scene.BoundingBox.middle+Vector3Single(0,MyTile.Tile_Scene.BoundingBox.maxsize+1,0),Vector3Single(0,-1,0),Vector3Single(0,0,1));
//  CastleControl1.scenemanager.camera.ProjectionMatrix:=OrthoProjection(-1, 1, -1, 1, 0.1, 100);//OrthogonalProjection;
 CastleControl1.scenemanager.camera.input:=TCamera.DefaultInput;
 CastleControl1.update;
end;

{------------------------------------------------------------------------}

procedure TForm1.TakeScreenshot;
begin
 //make and save a screenshot
 //maybe make them tiles 1x1 photo?
 SaveImage(CastleControl1.SaveScreen,combobox1.Items[combobox1.itemindex]+'screen.png');
end;

{------------------------------------------------------------------------}

Procedure TForm1.Empty_Tile_Map;
var jx,jy,jz,j:integer;
begin
 with MyTile do
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
    end;


end;

{------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
//Const OrthogonalProjection:TMatrix4Single=((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,0));
var URL:String;
begin
  //if something is loaded - discard it.
  if Tile_transfrom<>nil then CastleControl1.Scenemanager.items.remove(Tile_transfrom);
  // load the tile
  MyTile.Tile3D:=Load3D(models_folder+combobox1.Items[combobox1.itemindex]);
  MyTile.Tile_Scene:=TCastleScene.create(CastleControl1);
  MyTile.Tile_scene.spatial := [ssRendering, ssDynamicCollisions];
  MyTile.Tile_scene.processevents:=true;
  MyTile.Tile_scene.Load(MyTile.Tile3D,true);
  Tile_transfrom:=T3DTransform.Create(CastleControl1.SceneManager);
  Tile_transfrom.add(MyTile.Tile_scene);
  Tile_transfrom.translation:=Vector3Single(0,0,0);
  Tile_transfrom.scale:=Vector3Single(1,1,1);
  CastleControl1.Scenemanager.items.add(Tile_transfrom);

  ResetCamera;

  //now prepare the image for work
  MyTile.tilesizex:=round(MyTile.Tile_Scene.BoundingBox.sizex/2);
  MyTile.tilesizey:=round(MyTile.Tile_Scene.BoundingBox.sizez/2);
  MyTile.tilesizez:=round(MyTile.Tile_Scene.BoundingBox.sizey/2);
  image1.width:=32*MyTile.tilesizex;
  image1.height:=32*MyTile.tilesizey;
  memo1.clear;
  memo1.lines.add(inttostr(MyTile.tilesizex)+'x'+ inttostr(MyTile.tilesizey)+'x'+ inttostr(MyTile.tilesizez));
///  if no tilemap to load
  Empty_Tile_map;
  draw_tile_map;
end;

{----------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  ResetCamera;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TakeScreenshot;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Fix_x3d(combobox1.Items[combobox1.itemindex]);
end;

procedure TForm1.Button5Click(Sender: TObject);
var im:integer;
begin
  for im:=0 to combobox1.Items.Count-1 do Fix_x3d(combobox1.Items[im]);
end;

{------------------------------------------------}


end.

