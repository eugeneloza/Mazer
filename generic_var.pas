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

//this unit gives some very generic variables & constants that are used across other uinits.

unit generic_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleWindow;

const anisotropic_smoothing=8;        //rendering parameters for easy access
      Normal_map_enabled=false;
      Shadow_maps_enabled=true;
      Shadow_volumes_enabled=true;

const maxmaxx=15;                      //max map size
      maxmaxy=15;
      maxmaxz=3;
      maxMapTiles=maxmaxx*maxmaxy*maxmaxz;      //map volume
      maxGroups=maxMapTiles div 2;
      maxMapPlaceholders=MaxMapTiles*30;

const maxMaxPlaceholdersTypes=130;     //for static arrays size
      MaxMaxPlaceholderAtlasRecords=130;
      MaxMaxTilesTypes=130;            //tiles variants

const Target_Map_Area=maxmaxx*maxmaxy*maxmaxz div 5;

const base_models_folder='DAT'+pathdelim+'models'+pathdelim;
      tiles_models_folder=base_models_folder+'tiles'+pathdelim;
      items_models_folder=base_models_folder+'items'+pathdelim;
      placeholders_models_folder=base_models_folder+'placeholders'+pathdelim;
      etc_models_folder=base_models_folder+'etc'+pathdelim;
      //maps_folder='DAT'+pathdelim+'maps'+pathdelim;
      loadscreen_folder='DAT'+pathdelim+'loadscreen'+pathdelim;
      music_folder='DAT'+pathdelim+'music'+pathdelim;
      sound_folder='DAT'+pathdelim+'sound'+pathdelim;

const gamemode_loadscreen_init=100;
      gamemode_loadscreen=99;
      gamemode_game=1;

type shortstr=string[255]; //ansi string

var Window: TCastleWindow;
    myscale: single = 1.5;
    maxx:integer=maxmaxx;
    maxy:integer=maxmaxy;
    maxz:integer=maxmaxz;
    //currentMap:string='default';

implementation

end.

