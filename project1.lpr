{$R-}{$Q-}
program project1;

uses
  SysUtils,
  castle_base, castle_window, CastleWindow,
  castlescene, castlescenecore, castlescenemanager,
  castle3d, castlevectors,  X3DNodes, x3dload,
  castleplayer, castlecameras, CastleImages,
  CastleControls{for TCastleLAbel},
   Tile_var, generic_var, Generator;
  ///, CastleFreeType, CastleFonts, CastleUnicode, CastleStringUtils,


const show_map_boolean=false;



var

  vis: array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of boolean;
  px,py,pz,oldz: integer;

  Player:TPlayer;

  Label_FPS,explored_label:TCastleLabel;
  RoseLabel:TCastleLabel;

  zero_png:TCastleImage;
  Map_img,Player_Img:TCastleImageControl;


{==========================================================================}
{============================ PROCEDURES ==================================}
{==========================================================================}


{---------------------------------------------------------------------------}
procedure GenerateMap;
var ix,iy,iz:integer;
begin
 MakeMap;

 //clear visible
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do begin
     vis[ix,iy,iz]:=show_map_boolean;
   end;
 oldz:=-1;
end;

{************************************************************************}
var lasttime:TDateTime=-1;
    framecount:integer=0;
    visible_changed:boolean=false;
procedure set_vis(vx,vy,vz:integer);
begin
 if (vx>0) and (vy>0) and (vz>0) and (vx<=maxx) and (vy<=maxy) and (vz<=maxz) then
   if not vis[vx,vy,vz] then begin
     vis[vx,vy,vz]:=true;
     Visible_Changed:=true;
     inc(explored_area);
   end;
end;

procedure Update(Container: TUIContainer);
var ix,iy:integer;
    copymap:TCastleImage;
begin
 //show fps
 if oldz>0 then visible_changed:=false else visible_changed:=true;
 inc(framecount);
 if (lasttime>0) and ((now-lasttime)>1/24/60/60) then begin
   label_FPS.text.text:=inttostr(framecount{round(1/(now-lasttime)/24/60/60)});
   framecount:=0;
   lasttime:=now;
 end;
 if lasttime<0 then  lasttime:=now;

 //get player location
 px:=round(-(Player.position[0])/myscale/2);
 if px<1 then px:=1;
 if px>maxx then px:=maxx;
 py:=round(-(Player.position[2])/myscale/2);
 if py<1 then py:=1;
 if py>maxy then py:=maxy;
 pz:=round(-(Player.position[1]-1)/myscale/2);
 if pz<1 then pz:=1;
 if pz>maxz then pz:=maxz;
 set_vis(px,py,pz);
 if (map[px,py,pz].faces[angle_left]<>face_wall) then set_vis(px-1,py,pz);
 if (map[px,py,pz].faces[angle_right]<>face_wall) then set_vis(px+1,py,pz);
 if (map[px,py,pz].faces[angle_top]<>face_wall) then set_vis(px,py-1,pz);
 if (map[px,py,pz].faces[angle_bottom]<>face_wall) then set_vis(px,py+1,pz);

 //show the minimap
 if (oldz<>pz) or (visible_changed) then begin
   Copymap:=LoadImage(Models_Folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;       //BUG: it's the only way I could initialize TCastleImage...????????
   Copymap.setsize((maxx)*16,(maxy)*16,1);
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     if vis[ix,iy,pz] then
       copymap.drawFrom(minimap[pz],(ix-1)*16,(maxy-iy)*16,(ix-1)*16,(maxy-iy)*16,16,16)
     else
       copymap.drawFrom(zero_png,(ix-1)*16,(maxy-iy)*16,0,0,16,16);
   Map_IMG.left:=Window.width-Map_Img.image.width;
   map_img.bottom:=0;//Map_Img.image.height;
   Map_IMG.image:=nil;
   Map_IMG.image:=Copymap.makecopy;
   freeandnil(copymap);
 end;
 //Check if Rose is found
 if visible_changed then
 if (not RoseFound) and (not show_map_boolean) then
   if vis[rosex,rosey,rosez] then begin
     RoseFound:=true;
     RoseLabel:=TCastleLabel.create(Window);
     RoseLabel.Left:=window.width div 2-200;
     RoseLabel.bottom:=window.height-100;
     RoseLabel.text.text:='CONGRATULATIONS!!! You have found the rose!';
     Window.Controls.InsertFront(RoseLabel);
   end;

 if visible_changed then
   Explored_Label.text.text:='Explored: '+inttostr(round(Explored_Area/MapArea*100))+'%';


 //show player location
 Player_IMG.left:=Map_IMG.left+round(((-Player.position[0]/myscale/2-0.5))*16-3);//Map_IMG.left+Map_IMG.width-;
 Player_IMG.bottom:=Map_IMG.bottom+round((maxy-(-Player.position[2]/myscale/2-0.5))*16-3);//Map_IMG.bottom-Map_IMG.height+round(*16);


 oldz:=pz;
end;

{==========================================================================}
{================================= MAIN ===================================}
{==========================================================================}
Var i:integer;
begin
  maxx:=maxmaxx;
  maxy:=maxmaxy;
  maxz:=maxmaxz;
  Window := TCastleWindow.Create(Application);

  Label_fps:=TCastleLabel.create(Window);
  label_fps.Left:=0;
  label_fps.bottom:=0;
  Label_fps.text.text:='-';
  Window.Controls.InsertFront(Label_fps);

  Explored_label:=TCastleLabel.create(Window);
  Explored_label.Left:=100;
  Explored_label.bottom:=0;
  Explored_label.text.text:='0%';
  Window.Controls.InsertFront(Explored_label);

  Zero_png:=LoadIMage(Models_folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;

  Map_Img:=TCastleImageControl.create(Window);
  Map_IMG.image:=LoadIMage(Models_folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;
  Map_IMG.image.setsize((maxx)*16-1,(maxy)*16-1,1);
  //Map_IMG.image.clear(Vector4single(0,0,0,0));  // NOT WORKING???
  Window.Controls.InsertFront(Map_IMG);

  Player_IMG:=TCastleImageControl.create(Window);
  Player_IMG.image:=LoadIMage(Models_Folder+'player.png', [TRGBAlphaImage]) as TRGBAlphaImage;
  Player_IMG.left:=0;
  Player_IMG.bottom:=0;
  Window.Controls.InsertFront(Player_IMG);

  LoadTiles;
  GenerateMap;

  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;
  player.Camera.MouseLook:=true;
  player.DefaultPreferredHeight:=1;
  player.DefaultMoveHorizontalSpeed:=3;
  player.Camera.MouseLookHorizontalSensitivity:=0.5;
  player.Camera.MouseLookVerticalSensitivity:=0.5;
  player.position:=Vector3Single(-2*myscale*(maxx div 2),-2*myscale+(player.DefaultPreferredHeight),-2*myscale*(maxy div 2));
  player.camera.FallingEffect:=false;
  Window.scenemanager.camera:=player.camera;

  Window.OnUpdate:=@Update;
  Window.Open;
  Application.Run;

  for i:=1 to maxTilesTypes do freeandnil(Tiles[i].Tile_PNG);
end.

