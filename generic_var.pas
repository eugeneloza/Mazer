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

