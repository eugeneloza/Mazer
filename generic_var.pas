unit generic_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleWindow;

const maxmaxx=10;
      maxmaxy=10;
      maxmaxz=4;
      maxMapTiles=maxmaxx*maxmaxy*maxmaxz;

      const Target_Map_Area=maxmaxx*maxmaxy*maxmaxz div 2;


      myscale =1;

const models_folder='DAT'+pathdelim+'models'+pathdelim;

var Window: TCastleWindow;

implementation

end.

