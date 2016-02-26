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

{$R+}{$Q+}
program Mazer;

uses
  SysUtils, {$IFDEF UNIX}cthreads,{$ENDIF} Classes,
  castle_base, castle_window, CastleWindow,
  castlescene, castlescenecore, castlescenemanager,
  castle3d, castlevectors,  X3DNodes, x3dload,
  castleplayer, castlecameras, CastleImages,
  CastleControls,
  CastleOpenAL, CastleSoundEngine, CastleTimeUtils,
  Tile_var, generic_var, Generator, MazerMapParser;
  ///, CastleFreeType, CastleFonts, CastleUnicode, CastleStringUtils,

const show_map_boolean=false;
const play_music=true;
const loadscreenanimationspeed=12;

const const_FPS_goal=70; //dynamic goal for FPS.

//this thread will generate the map in the background
type TGenerationThread = class(TThread)
  private
  protected
    procedure Execute; override;
end;
type TMusicLoadThread = class(TThread)
  private
  protected
    procedure Execute; override;
end;

var

  vis: array[1..maxmaxx,1..maxmaxy,1..maxmaxz] of boolean;
  px,py,pz,oldz: integer;

  Player:TPlayer;
  firstRender:boolean;

  Label_FPS,explored_label,Tile_label:TCastleLabel;
  RoseLabel:TCastleLabel;

  zero_png:TCastleImage;
  Map_img,Player_Img:TCastleImageControl;

  music: TSoundBuffer;
  music_duration: TFloatTime;
  oldmusic:integer;
  MusicLoadThread:TMusicLoadThread; //thread to load music in background to avoid lags
  //footsteps_sound:TSoundBuffer;

  gamemode:integer;
  loadscreen_img: TCastleImageControl;
  Loadscreen_label: TCastleLabel;
  GenerationThread:TGenerationThread; //thread for generating the dungeon in background

{==========================================================================}
{============================ PROCEDURES ==================================}
{==========================================================================}


{---------------------------------------------------------------------------}
procedure GenerateMap;
var ix,iy,iz:integer;
begin
 MakeMap; //ask Generator to produce a map

 //clear visible
 for ix:=1 to maxx do
  for iy:=1 to maxy do
   for iz:=1 to maxz do begin
     vis[ix,iy,iz]:=show_map_boolean;
   end;
 oldz:=-1;
end;

{=======================================================================}

{ This procedure is a Chunk manager.
During generation process we've chunked our map into areas
Those areas are everything that is visible from this tile.
Everything else is switched-off by TSwitchNode to keep FPS.
Later we'll make some LODs here also. Maybe.
}
var p0x:integer=-1;
    p0y:integer=-1;
    p0z:integer=-1;
procedure ChunkManager(Container: TUIContainer);
var i,j,px,py,pz:integer;
    GroupActive:array[1..MaxGroups]of boolean;
begin
 if not firstrender then begin
   px:=round((Player.position[0])/myscale/2);
   if px<1 then px:=1;
   if px>maxx then px:=maxx;
   py:=round(-(Player.position[1])/myscale/2);
   if py<1 then py:=1;
   if py>maxy then py:=maxy;
   pz:=round(-(Player.position[2]-1)/myscale/2);
   if pz<1 then pz:=1;
   if pz>maxz then pz:=maxz;
   if (p0x<>px) or (p0y<>py) or (p0z<>pz) then begin
      for j:=1 to n_groups do GroupActive[j]:=false;
      for i:=1 to n_tiles do if Neighbours[px,py,pz][i]>0 then GroupActive[groups[i]]:=true;
      for j:=1 to n_groups do if GroupActive[j] then GroupsSwitches[j].whichChoice:=0 else GroupsSwitches[j].whichChoice:=-1;
      p0x:=px;
      p0y:=py;
      p0z:=pz;
   end;
 end;
end;

{************************************************************************}
var lasttime:TDateTime=-1;
    starttime:TDateTime;
    loadscreentime:TDateTime=-1;
    framecount:integer=0;
    visible_changed:boolean=false;
// set visible for a tile with all range-checking and etc.
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
    if gamemode=gamemode_game then begin

     //show fps
     if oldz>0 then visible_changed:=false else visible_changed:=true;
     inc(framecount);
     if (lasttime>0) and ((now-lasttime)>1/24/60/60) then begin
       label_FPS.text.text:=inttostr(framecount{round(1/(now-lasttime)/24/60/60)});
       framecount:=0;
       lasttime:=now;
     end;

     if lasttime<0 then begin
       Window.Controls.InsertFront(Label_fps);
       Window.Controls.InsertFront(Explored_label);
       Window.Controls.InsertFront(Tile_label);
       Window.Controls.InsertFront(Map_IMG);
       Window.Controls.InsertFront(Player_IMG);

       lasttime:=now;
       starttime:=now;
     end else begin
      //animate the rose
       (RoseS.RootNode.FdChildren[2] as TTRansformNode).FdRotation.Value:=vector4Single(0,0,1,(now-starttime)*24*60*60*2);
       (RoseS.RootNode.FdChildren[2] as TTRansformNode).FdRotation.changed;
     end;

     //get player location
     px:=round((Player.position[0])/myscale/2);
     if px<1 then px:=1;
     if px>maxx then px:=maxx;
     py:=round(-(Player.position[1])/myscale/2);
     if py<1 then py:=1;
     if py>maxy then py:=maxy;
     pz:=round(-(Player.position[2]-1)/myscale/2);
     if pz<1 then pz:=1;
     if pz>maxz then pz:=maxz;
     //set visible all tiles ajacent to the player
     set_vis(px,py,pz);
     if (map[px,py,pz].faces[angle_left]<>face_wall) then set_vis(px-1,py,pz);
     if (map[px,py,pz].faces[angle_right]<>face_wall) then set_vis(px+1,py,pz);
     if (map[px,py,pz].faces[angle_top]<>face_wall) then set_vis(px,py-1,pz);
     if (map[px,py,pz].faces[angle_bottom]<>face_wall) then set_vis(px,py+1,pz);
     if (map[px,py,pz].floor[angle_stairs_up]<>floor_wall) then set_vis(px,py,pz-1);
     if (map[px,py,pz].floor[angle_stairs_down]<>floor_wall) then set_vis(px,py,pz+1);

{     PlayerTile:=MapTileIndex[px,py,pz];
     if playertile>0 then Tile_label.text.text:=(Tiles[GeneratorSteps[PlayerTile].Tile_Type].TileName)+' : ('+inttostr(px)+','+inttostr(py)+','+inttostr(pz)+')';}

     //show the minimap
     if (oldz<>pz) or (visible_changed) then begin
       //create a temporary image and copy current minimap[pz] if this tile is 'visible'
       Copymap:=LoadImage(etc_Models_Folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;       //BUG: it's the only way I could initialize TCastleImage properly...????????
       Copymap.setsize((maxx)*16,(maxy)*16,1);
       Copymap.Clear(Vector4Byte(0,0,0,0));
       for ix:=1 to maxx do
        for iy:=1 to maxy do
         if vis[ix,iy,pz] then
           copymap.drawFrom(minimap[pz],(ix-1)*16,(maxy-iy)*16,(ix-1)*16,(maxy-iy)*16,16,16,dmBlendSmart);
       //BUG! I need dmBlend (practically just replace/mov) here, not dmBlendSmart, but dmBlend leaves alpha=0.
         {else
           copymap.drawFrom(zero_png,(ix-1)*16,(maxy-iy)*16,0,0,16,16,dmBlend);}
         //BUG! this one stopped working at some point...
       Map_IMG.left:=Window.width-Map_Img.image.width; //plug it to right-bottom corner in case of window resize
       map_img.bottom:=0;//Map_Img.image.height;
       Map_IMG.image:=nil;
       Map_IMG.image:=Copymap.makecopy; //BUG: Don't I leave memory leaks here?
       freeandnil(copymap);

       firstrender:=false;
     end;
     //Check if Rose is found
     if visible_changed then
     if (not RoseFound) and (not show_map_boolean) then
       if vis[rosex,rosey,rosez] then begin
         RoseFound:=true;
         //if yes, then show the label
         RoseLabel:=TCastleLabel.create(Window);
         RoseLabel.Left:=window.width div 2-200;
         RoseLabel.bottom:=window.height-100;
         RoseLabel.text.text:='CONGRATULATIONS!!! You have found the rose!';
         Window.Controls.InsertFront(RoseLabel);
       end;

     //show % explored
     if visible_changed then begin
       ix:=round(Explored_Area/MapArea*100);
       if (ix=100) and (explored_Area<MapArea) then ix:=99;
       Explored_Label.text.text:='Explored: '+inttostr(ix)+'%';
     end;

     //show player location
     Player_IMG.left:=Map_IMG.left+round(((Player.position[0]/myscale/2-0.5))*16-3);//Map_IMG.left+Map_IMG.width-;
     Player_IMG.bottom:=Map_IMG.bottom+round((maxy-(-Player.position[1]/myscale/2-0.5))*16-3);//Map_IMG.bottom-Map_IMG.height+round(*16);


     oldz:=pz;
    end else
    //this shows animated load screen (should be a 'velocity' somewhere);
    if gamemode=gamemode_loadscreen then begin
      if loadscreen_IMG=nil then begin
        if loadscreentime=-1 then loadscreentime:=now;
        loadscreen_IMG:=TCastleImageControl.create(Window);
        case random(5) of
          0:loadscreen_IMG.image:=LoadIMage(loadscreen_folder+'lovely-image_CC0_by_Sharon_Apted.jpg', [TRGBAlphaImage]) as TRGBAlphaImage;
          1:loadscreen_IMG.image:=LoadIMage(loadscreen_folder+'fractal-in-green-1305526660FTL_CC0_by_Sharon_Apted+.jpg', [TRGBAlphaImage]) as TRGBAlphaImage;
          2:loadscreen_IMG.image:=LoadIMage(loadscreen_folder+'red-centred-fractal_CC0_by_Sharon_Apted+.jpg', [TRGBAlphaImage]) as TRGBAlphaImage;
          else loadscreen_IMG.image:=LoadIMage(loadscreen_folder+'Mosaic_Rose_CC0_by_Piotr_Siedlecki+.jpg', [TRGBAlphaImage]) as TRGBAlphaImage;
        end;
        loadscreen_IMG.Image.Resize(round(loadscreen_img.image.Width/loadscreen_img.image.height*window.Height), window.Height,  riBilinear);
        loadscreen_IMG.left:=0;
        loadscreen_IMG.bottom:=0;
        Window.Controls.InsertFront(loadscreen_IMG);
        loadscreen_label:=TCastleLabel.create(Window);
        loadscreen_label.Left:=window.width;
        loadscreen_label.bottom:=round(window.Height * sqr(1-0.61));
        loadscreen_label.text.text:='Welcome to MAZER :)';
        loadscreen_label.text.Add('Generating... Please, wait...');
        Window.Controls.InsertFront(loadscreen_label);
      end;
      loadscreen_IMG.Left:=round((now-loadscreentime)*24*60*60*loadscreenanimationspeed);
      loadscreen_label.Left:=window.width-length(loadscreen_label.Text.text)*12 div 2-round((now-loadscreentime)*24*60*60*loadscreenanimationspeed);
      if (Loadscreen_IMG.Left>=window.width-loadscreen_IMG.image.width) then begin
        window.Controls.Remove(loadscreen_img);
        window.Controls.Remove(loadscreen_label);
        freeandnil(loadscreen_IMG);
        freeandnil(loadscreen_label);
        loadscreentime:=now;
      end;
    end else
    if gamemode=gamemode_loadscreen_init then begin
      gamemode:=gamemode_loadscreen;
      GenerationThread:=TGenerationThread.Create(true);
      GenerationThread.FreeOnTerminate:=true;
      GenerationThread.Priority:=tpLower;
      writeln('Thread started...');
//      GenerationThread.Resume;
      GenerationThread.Start;
    end;
end;

{----- this thread will initialize the interface and generate the map --------}

procedure TGenerationThread.execute;
begin
 writeln('Loading tiles...');
 LoadTiles;    //read all tiles from the file
 writeln('Generating Map...');
 GenerateMap;  //generate the map;

 //final initializations
 window.Controls.Remove(loadscreen_img);
 window.Controls.Remove(loadscreen_label);
 Window.scenemanager.camera:=player.camera;

 GameMode:=GameMode_game;
end;

{---------------}

var MyMusicTimer:TDateTime;
    MusicReady:boolean;
procedure TMusicLoadThread.execute;
var nextmusic:integer;
    music_name:string;
begin
 //select the track and keep it different from the current one
 writeln('Starting music thread... ');
 repeat
   nextmusic:=trunc(random*6)+1;
   if nextmusic=0 then nextmusic:=1;
   if nextmusic>6 then nextmusic:=6;
 until nextmusic<>oldmusic;
 //load the track
 case nextmusic of
     1: music_name:='Cleyton_RX_Underwater_CC-BY_by_Doppelganger.ogg';
     2: music_name:='Crystal_cave_Mixdown_CC-BY_by_cynicmusic.ogg';
     3: music_name:='Lurid_Delusion_CC-BY_by_Matthew_Pablo.ogg';
     4: music_name:='Mysterious_Ambience_Mixdown_CC-BY_by_cynicmusic.ogg';
     5: music_name:='Mystic_theme_CC-BY_by_Alexandr_Zhelanov.ogg';
    else music_name:='Organ_Mixdown_CC-BY-SA_by_Devon_Baumgarten.ogg';
 end;
 //start music
 writeln('soundengine.loadbuffer ',music_name);
 music:=soundengine.loadbuffer(music_folder+music_name,music_duration);
 writeln('SoundEngine.PlaySound (outside the trhead)',music_name);
 //and finish
 oldmusic:=nextmusic;
 MyMusicTimer:=now;
 MusicReady:=true;
end;

procedure do_music;
begin
 if (MyMusicTimer>0) and ((now-MyMusicTimer)*60*60*24>music_duration+1) then MyMusicTimer:=-1;

 //if no initialized music then do a new one.
 if (MyMusicTimer=-1) and play_music then begin
   begin
     //init 'no music' to prevent another one from starting until this one is ready
     MyMusicTimer:=now;
     Music_duration:=10000;
     writeln('Starting music... ');
     //launching music through a thread to avoid lags both in music and gameplay
     MusicLoadThread:=TMusicLoadThread.Create(true);
     MusicLoadThread.FreeOnTerminate:=true;
     MusicLoadThread.Priority:=tpLower;
     MusicLoadThread.Start;
   end
 end;
end;


procedure do_timer;
begin
  if play_music then do_music; //each ontimer ms check if new music should be played - it's ugly, but I'll fix that one later, much later
  //I load music in a thread but start playing it outside the thread to avoid some stupid SIGSEGV... maybe SoundEngine.PlaySound cannot run in a thread?
  if MusicReady then begin
    SoundEngine.PlaySound(music, false, false, 0, 1, 0, 1, ZeroVector3Single);
    writeln('SoundEngine.PlaySound done');
    MusicReady:=false;
  end;

  if gamemode=gamemode_loadscreen then window.DoRender;
  if gamemode=gamemode_game then application.TimerMilisec:=1000; //reset timer back to 1s in-game, it was 60fps for loading screen
end;

{==========================================================================}
{================================= MAIN ===================================}
{==========================================================================}
//Var i,j:integer;
begin
  writeln('Starting...');
  Window := TCastleWindow.Create(Application);

  if play_music then begin
    //prepare music
    //music seems to be rather heavy on FPS... :(
    SoundEngine.ParseParameters;             //start castle sound engine
    SoundEngine.MinAllocatedSources := 1;
    oldmusic:=-1;
    MyMusicTimer:=-1;
    MusicReady:=false; {non-initialized}
  end;

  gamemode:=gamemode_loadscreen_init;

  //label for FPS dislpay
  Label_fps:=TCastleLabel.create(Window);
  label_fps.Left:=0;
  label_fps.bottom:=0;
  Label_fps.text.text:='-';

  //label for % explored display
  Explored_label:=TCastleLabel.create(Window);
  Explored_label.Left:=100;
  Explored_label.bottom:=0;
  Explored_label.text.text:='0%';

  //label to display tile info (debug)
  Tile_label:=TCastleLabel.create(Window);
  Tile_label.Left:=300;
  Tile_label.bottom:=0;
  Tile_label.text.text:='-';

  //a transparent 16x16 image, I use it sometimes (bug?).
  Zero_png:=LoadIMage(etc_Models_folder+'0.png', [TRGBAlphaImage]) as TRGBAlphaImage;

  //this is a minimap image
  Map_Img:=TCastleImageControl.create(Window);
  Map_IMG.image:=zero_png.MakeCopy; //TODO: don't I leave memory leaks here?
  Map_IMG.image.setsize((maxx)*16-1,(maxy)*16-1,1);

  //this image will display the player location
  Player_IMG:=TCastleImageControl.create(Window);
  Player_IMG.image:=LoadIMage(etc_Models_Folder+'player.png', [TRGBAlphaImage]) as TRGBAlphaImage;
  Player_IMG.left:=0;
  Player_IMG.bottom:=0;

  //set the player properties.
  Player := TPlayer.Create(Window.SceneManager);
  Window.SceneManager.Items.Add(Player);
  Window.SceneManager.Player := Player;
  player.Camera.MouseLook:=true;
  Player.Camera.GravityUp:=Vector3Single(0,0,1);
  Player.Up:=Vector3Single(0,0,1);
//  Player.Camera.Direction:=Vector3Single(0,1,0);
//  Player.SetView(Vector3Single(0,-1,0),Vector3Single(0,0,1),true);
  player.DefaultPreferredHeight:=1;
  player.DefaultMoveHorizontalSpeed:=3;
  player.Camera.MouseLookHorizontalSensitivity:=0.5;
  player.Camera.MouseLookVerticalSensitivity:=0.5;
  player.position:=Vector3Single(2*myscale*(maxx div 2),-2*myscale*(maxy div 2),-2*myscale+(player.DefaultPreferredHeight));
  player.FallingEffect:=false;
  player.HeadBobbing:=0.03;
  player.Camera.HeadBobbingTime:=0.5;

  //footsteps_sound:=soundengine.loadbuffer(sound_folder+'footsteps.wav');
  //stPlayerFootstepsDefault:=0;//SoundEngine.SoundFromName('footsteps.wav',true);

  //initialize CastleWindow
  Window.OnUpdate:=@Update;
  firstRender:=true; //just let the render know, if it's the first time rendering to properly initialize a few variables;
  window.OnBeforeRender:=@ChunkManager; //this one manages chunks (loads and unlads them)
  Window.ShadowVolumes := Shadow_volumes_enabled;  {?????}
  Window.ShadowVolumesRender:=Shadow_volumes_enabled; {?????????}
  Window.StencilBits := 8;     {?????????}
  window.FullScreen:=false;
  //window.doublebuffer:=true;

  application.TimerMilisec:=1000 div 60{1000 div loadscreenanimationspeed div 3}; //60 fps for loadscreen
  application.OnTimer:=@do_timer;

  {=== this will start the game ===}
  Window.Open;
  Application.Run;
  {=== ........................ ===}

  writeln('freeing all ...');
  //free everything unneeded.
{  for i:=1 to maxTilesTypes do
   for j:=1 to maxtilesizez do if Tiles[i].Tile_PNG[j]<>nil then freeandnil(Tiles[i].Tile_PNG[j]);}
  writeln('ending...')
end.

