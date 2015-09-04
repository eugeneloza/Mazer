unit generic_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleWindow;

const maxmaxx=15;
      maxmaxy=15;
      maxmaxz=9;
      maxMapTiles=maxmaxx*maxmaxy*maxmaxz;

      const Target_Map_Area=maxmaxx*maxmaxy*maxmaxz div 2;


      myscale =1;

const models_folder='DAT'+pathdelim+'models'+pathdelim;

var Window: TCastleWindow;

implementation

end.

