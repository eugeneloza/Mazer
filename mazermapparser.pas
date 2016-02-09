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

//this unit contains generic parser procedures

unit MazerMapParser;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Dialogs;

const TileSizeX_Record='TileSizeX=';
      TileSizeY_Record='TileSizeY=';
      TileSizeZ_Record='TileSizeZ=';
      TileBase_record='Base=';
      TileFloorUp_record='FloorUp=';
      TileFloorDown_record='FloorDn=';
      //TileFace_Record='[';
      //tile record is <XXXXX,YYYY,ZZZZZ>

const PlaceholderAngleSignature1='/Angle=';
      PlaceholderAngleSignature2='/a=';
      PlaceholderRNDSignature1='/Random=';
      PlaceholderRNDSignature2='/r=';
      PlaceholderSymmetrySignature1='/Symmetry=';
      PlaceholderSymmetrySignature2='/s=';
      PlaceholderSymmetryRandomSignature1='/SymmetryRandom=';
      PlaceholderSymmetryRandomSignature2='/sr=';

const no_data_record=-9999;

type XYZ_record = record
 x,y,z:integer;
end;

//this defines info taken from the placeholder name:
//name, RND, Angle and Symmetry group
type PlaceholderStyle= record
  PlaceholderName:string;
  PlaceholderRND,PlaceholderAngle,PlaceholderSymmetry,PlaceholderSymmetryRandom:integer;
end;

var errorstring:string='';
    TileMapFile:Text;

Function ParseTileCoord(s_in:string):XYZ_record;
Function ParseBasicData(s_in,s_parse:string):integer;
function ParsePlaceholder(const s_in:string):PlaceholderStyle;

implementation

//convert tile map coordinates in text to binary
Function ParseTileCoord(s_in:string):XYZ_record;
var i,j:integer;
    s:array[1..3] of string;
begin
 i:=1; //first symbol is '<'
 j:=1;
 s[1]:='';s[2]:='';s[3]:='';
 repeat
   inc(i);
   if (copy(s_in,i,1)<>',') and (copy(s_in,i,1)<>'>') then s[j]+=copy(s_in,i,1) else begin
     inc(j);
   end;
 until copy(s_in,i,1)='>';
 val(s[1],i,j);
 if j=0 then ParseTileCoord.x:=i else ErrorString+='Parse error in x! ' + s[1];
 val(s[2],i,j);
 if j=0 then ParseTileCoord.y:=i else ErrorString+='Parse error in y! ' + s[2];
 val(s[3],i,j);
 if j=0 then ParseTileCoord.z:=i else ErrorString+='Parse error in z! ' + s[3];

end;

//basic parsing of the string.
//find s_parse in the s_in string and return its value as result
Function ParseBasicData(s_in,s_parse:string):integer;
var thisvalue,errorcode:integer;
begin
  ParseBasicData:=no_data_record;
  if copy(s_in,1,length(s_parse))=s_parse then begin
    val(copy(s_in,length(s_parse)+1,length(s_in)-length(s_parse)),thisvalue,errorcode);
    if errorcode=0 then ParseBasicData:=thisvalue else ErrorString+='Parse error in basic data! '+copy(s_in,length(s_parse)+1,length(s_in)-length(s_parse));
  end;
end;

//******************** not used yet
//parse the placeholder name string
function ParsePlaceholder(const s_in:string):PlaceholderStyle;
var i:integer;
    workString:string;
    thischaracter:string;

    //no support for floating numbers (and, maybe, unneeded - integer accuracy is absolutely enough)
    //especially due to blender automatically naming copies as .001
    //well... practically at this moment it degraded to "if thischar in ['0'..'9']"
    function IsNumber(const ThisChar:string):byte;
    begin
      case ThisChar of
       '0':isNumber:=0;
       '1':isNumber:=1;
       '2':isNumber:=2;
       '3':isNumber:=3;
       '4':isNumber:=4;
       '5':isNumber:=5;
       '6':isNumber:=6;
       '7':isNumber:=7;
       '8':isNumber:=8;
       '9':isNumber:=9;
       else IsNumber:=255; //not a number
      end;
    end;
    //read character-by-character until eol or not a number
    function getNumber:integer;
    var thisstring:string;
    begin
      thisstring:='';
      repeat
        thischaracter:=copy(s_in,i,1);
        if IsNumber(thisCharacter)<255 then begin
          thisstring:=thisstring+thischaracter;
          inc(i);  // I shouldn't miss the next '/' symbol so I inc(i) inside the if
        end;
      until (i=length(s_in)) or (IsNumber(thisCharacter)=255);
      getNumber:=strtoint(ThisString);
    end;
begin
 with ParsePlaceholder do begin
   workString:=trim(s_in);
   PlaceholderName:='';
   PlaceholderRND:=100; //set default values
   PlaceholderAngle:=0; //no random rotations
   PlaceholderSymmetry:=-1; //no symmetry

   //read placeholder name
   i:=1; //we start at 2nd symbol, because the first one is already '(' which is the placeholder signature
   if length(workString)>1 then
   repeat
     inc(i);
     thischaracter:=copy(s_in,i,1);
     if thischaracter<>')' then
       PlaceholderName:=PlaceholderName+thischaracter;
   until (thischaracter=')') or (i=length(WorkString));
   //and scan until the end of the line for additional placeholder parameters signatures
   if i<length(WorkString) then begin
     repeat
       inc(i);
       //check for specific strings at this location
       if (copy(s_in,i,length(PlaceholderAngleSignature1))=PlaceholderAngleSignature1) then begin
         inc(i,length(PlaceholderAngleSignature1));
         PlaceholderAngle:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderAngleSignature2))=PlaceholderAngleSignature2) then begin
         inc(i,length(PlaceholderAngleSignature2));
         PlaceholderAngle:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderRNDSignature1))=PlaceholderRNDSignature1) then begin
         inc(i,length(PlaceholderRNDSignature1));
         PlaceholderRND:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderRNDSignature2))=PlaceholderRNDSignature2) then begin
         inc(i,length(PlaceholderRNDSignature2));
         PlaceholderRND:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderSymmetrySignature1))=PlaceholderSymmetrySignature1) then begin
         inc(i,length(PlaceholderSymmetrySignature1));
         PlaceholderSymmetry:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderSymmetrySignature2))=PlaceholderSymmetrySignature2) then begin
         inc(i,length(PlaceholderSymmetrySignature2));
         PlaceholderSymmetry:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderSymmetryRandomSignature1))=PlaceholderSymmetryRandomSignature1) then begin
         inc(i,length(PlaceholderSymmetryRandomSignature1));
         PlaceholderSymmetryRandom:=GetNumber;
       end;
       if (copy(s_in,i,length(PlaceholderSymmetryRandomSignature2))=PlaceholderSymmetryRandomSignature2) then begin
         inc(i,length(PlaceholderSymmetryRandomSignature2));
         PlaceholderSymmetryRandom:=GetNumber;
       end;
     until i=length(WorkString)
  end;
 end;
end;

end.

