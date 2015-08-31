unit Mazer3_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CastleControl,
  castlescene, castlescenecore, CastleCreatures, castle3d,
  castlevectors, castlescenemanager, CastleKeysMouse, X3DNodes,
  x3dload, castle_base,
  castleplayer,
  castlecameras;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CastleControl1: TCastleControl;
    Image1: TImage;
    Label1: TLabel;
    Memo1: TMemo;
    ScrollBar1: TScrollBar;
    procedure Button1Click(Sender: TObject);

    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;



implementation

{$R *.lfm}

{ TForm1 }
{---------------------------------------------------------------------------}

{---------------------------------------------------------------------------}
procedure TForm1.DrawMap;
var ix,iy,iz:integer;
    scalex,scaley:double;
    x1,y1,x2,y2:integer;
    MapExploredArea:integer;
begin
 iz:=scrollbar1.position;
 scalex:=1/(maxx)*(image1.width-1);
 scaley:=1/(maxy)*(image1.height-1);
 with Image1.Canvas do
  for ix:=1 to maxx do
   for iy:=1 to maxy do if (chngd[ix,iy,iz]) or (drawall) then begin
     chngd[ix,iy,iz]:=false;
     x1:=round((ix-1)*scalex);
     y1:=round((iy-1)*scaley);
     x2:=round((ix)*scalex)-1;
     y2:=round((iy)*scaley)-1;
     //draw base tile
     brush.style:=bssolid;
     case Map[ix,iy,iz].base of
       tile_free:brush.color:=$333333;
       tile_stairs_up: brush.color:=$00FF00;
       tile_stairs_down: brush.color:=$0000FF;
       tile_na:brush.color:=$442222;
     end;
     if not vis[ix,iy,iz] then brush.color:=$000000;
     fillrect(x1+1,y1+1,x2,y2);

{     if DistanceMap[ix,iy,iz]>=0 then begin
       font.color:=$FFFFFF;
       font.size:=7;
       textout(x1+2,y1+2,inttostr(DistanceMap[ix,iy,iz]));
     end;}

       //draw faces
       pen.width:=1;
       case Map[ix,iy,iz].faces[angle_top] of
         face_wall:pen.color:=$ffffff;
         face_free:pen.color:=$333333;
         face_na:pen.color:=$111111;
       end;
       if not vis[ix,iy,iz] then pen.color:=$000000;
       moveto(x1,y1);
       lineto(x2,y1);
       case Map[ix,iy,iz].faces[angle_bottom] of
         face_wall:pen.color:=$ffffff;
         face_free:pen.color:=$333333;
         face_na:pen.color:=$111111;
       end;
       if not vis[ix,iy,iz] then pen.color:=$000000;
       moveto(x1,y2);
       lineto(x2+1,y2);
       case Map[ix,iy,iz].faces[angle_left] of
         face_wall:pen.color:=$ffffff;
         face_free:pen.color:=$333333;
         face_na:pen.color:=$111111;
       end;
       if not vis[ix,iy,iz] then pen.color:=$000000;
       moveto(x1,y1);
       lineto(x1,y2);
       case Map[ix,iy,iz].faces[angle_right] of
         face_wall:pen.color:=$ffffff;
         face_free:pen.color:=$333333;
         face_na:pen.color:=$111111;
       end;
       if not vis[ix,iy,iz] then pen.color:=$000000;
       moveto(x2,y1);
       lineto(x2,y2);
   end;
 //draw player;
 if Player<>nil then begin
   x1:=round((-Player.position[0]/myscale/2-0.5)*scalex);
   y1:=round((-Player.position[2]/myscale/2-0.5)*scaley);
   x2:=round((-(Player.position[0]+Player.direction[0])/myscale/2-0.5)*scalex);
   y2:=round((-(Player.position[2]+Player.direction[2])/myscale/2-0.5)*scaley);
   image1.canvas.pen.color:=$0000FF;
   image1.canvas.moveto(x1,y1);
   image1.canvas.lineto(x2,y2);
   Image1.Canvas.brush.color:=$0000FF;
   image1.canvas.fillrect(x1-1,y1-1,x1+2,y1+2);

   MapExploredArea:=0;
   for ix:=1 to maxx do
    for iy:=1 to maxy do
     for iz:=1 to maxz do if (vis[ix,iy,iz]) and (map[ix,iy,iz].base=tile_free) then inc(mapExploredArea);
   label1.caption:='Explored: '+inttostr(round(mapExploredArea/mapArea*100))+'%';

   if (vis[RoseX,RoseY,RoseZ]) and (not RoseFound) and (not show_map_boolean) then begin
     player.camera.mouselook:=false;
     RoseFound:=true;
     Showmessage('CONGRATULATIONS!!! You have found the rose!');
   end;
 end;
end;

{---------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 GenerateMap;
 scrollbar1.Min:=1;
 scrollbar1.max:=maxz;
 scrollbar1.position:=1;
 //DrawMap;
end;

procedure TForm1.CastleControl1Render(Sender: TObject);
begin
 px:=round(-(Player.position[0])/myscale/2);
 if px<1 then px:=1;
 if px>maxx then px:=maxx;
 py:=round(-(Player.position[2])/myscale/2);
 if py<1 then py:=1;
 if py>maxy then py:=maxy;
 pz:=round(-(Player.position[1]-1)/myscale/2);
 if pz<1 then pz:=1;
 if pz>maxz then pz:=maxz;
 scrollbar1.position:=pz;

 if oldx=-1 then begin
   oldx:=px;
   oldy:=py;
   oldz:=pz;
   drawall:=true;
 end;

 vis[px,py,pz]:=true;
 chngd[px,py,pz]:=true;
 if (px>1) and (map[px,py,pz].faces[angle_left]<>face_wall) then begin
   vis[px-1,py,pz]:=true;
   chngd[px-1,py,pz]:=true;
 end;
 if (px<maxx) and (map[px,py,pz].faces[angle_right]<>face_wall) then begin
   vis[px+1,py,pz]:=true;
   chngd[px+1,py,pz]:=true;
 end;
 if (py>1) and (map[px,py,pz].faces[angle_top]<>face_wall) then begin
   vis[px,py-1,pz]:=true;
   chngd[px,py-1,pz]:=true;
 end;
 if (py<maxx) and (map[px,py,pz].faces[angle_bottom]<>face_wall) then begin
   vis[px,py+1,pz]:=true;
   chngd[px,py+1,pz]:=true;
 end;
 chngd[oldx,oldy,oldz]:=true;
 if oldz<>pz then drawall:=true;


 if (drawall) or ((now-lasttimer)*24*60*60>1/30) then DrawMap;
 oldx:=px;
 oldy:=py;
 oldz:=pz;

 if RoseTransform<>nil then begin
   RoseTransform.FdRotation.RotationRad := now*24*60*60;
   RoseTransform.FdRotation.Changed;
 end;
end;

{procedure TForm1.CastleControl1Render(Sender: TObject);
begin
 LightInstance.Location := player.position;
end;}

{---------------------------------------------------------------------------}
end.

