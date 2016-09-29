unit UoptionUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;


Function FontToString(f : TFont): String;
Function StringToFont(s : String): TFont;
function getTextColour(t1 : TColor ; t2 : TColor): TColor;
function getTextFont(f1 : TFont ; f2 : TFont): TFont;

implementation

Function FontToString(f : TFont): String;
{  Produces a string representation of a given font.
   All the font attributes are convered to strings and packed together.

   Does not yet handle errors - will there be any ;o)                                              }


VAR
  rtnStr : string;
begin
  rtnStr := '';
  rtnStr := rtnStr + intToStr(f.Charset) + ':';
  rtnStr := rtnStr + ColorToString(f.Color) + ':';
  rtnStr := rtnStr + IntToStr(f.Height) + ':';
  rtnStr := rtnStr + (f.Name) + ':';
  rtnStr := rtnStr + IntToStr(f.Orientation) + ':';
  rtnStr := rtnStr + IntToStr(f.size) + ':';
  rtnStr := rtnStr + IntToStr(ord(f.Pitch)) + ':';
  rtnStr := rtnStr + IntToStr(ord(f.Quality)) + ':';

  //                             TFontStyles is a set and scanned differently.
  if fsBold in f.Style then
    rtnStr := rtnStr + 'B'
  else
    rtnStr := rtnStr + '.';

  if fsItalic in f.Style then
    rtnStr := rtnStr + 'I'
  else
    rtnStr := rtnStr + '.';

  if fsStrikeOut in f.Style then
    rtnStr := rtnStr + 'S'
  else
    rtnStr := rtnStr + '.';

  if fsUnderline in f.Style then
    rtnStr := rtnStr + 'U'
  else
    rtnStr := rtnStr + '.';


  FontToString := rtnStr;
end;

Function StringToFont(s : String): TFont;
{  Produces a fon from a given string representation [produced by FonttoString.
   The string is read a bit at at time [upto the next :] and this is conversted
   to the fiven font attribute.

   NB :: pos starts from position 1, where copy start at position 1.
         TFontStyles is a set and scanned differently.

   Does not yet handle errors - will there be any ;o)                                              }

VAR
  p    : integer;
  err  : integer;
  chrs : integer;
  clr  : TColor;
  Hght : integer;
  nme  : String;
  Ortn : Integer;
  sze  : Integer;
  ptch : Integer;
  qlty : integer;

  fnt  : TFont;
  fstyles : TFontStyles;
begin
  fnt := TFont.Create;
  fstyles := [];             //  empty set ??

  p := Pos(':', s);                                   //  Characterset of font
  val(copy(s, 0, p - 1), chrs, err);
  delete(s, 1, p);

  p := Pos(':', s);                                   //  colour of font
  clr := StringToColor(copy(s, 0, p - 1));
  delete(s, 1, p);

  p := Pos(':', s);                                  //  height of font
  val(copy(s, 0, p - 1), Hght, err);
  delete(s, 1, p);

  p := Pos(':', s);                                  //  name of font
  nme := copy(s, 0, p - 1);
  delete(s, 1, p);

  p := Pos(':', s);                                  //  orientation of font
  val(copy(s, 0, p - 1), Ortn, err);
  delete(s, 1, p);

  p := Pos(':', s);                                  //  size of font
  val(copy(s, 0, p - 1), sze, err);
  delete(s, 1, p);

  p := Pos(':', s);                                  //  pitch of font
  val(copy(s, 0, p - 1), ptch, err);
  delete(s, 1, p);

  p := Pos(':', s);                                 //  quality of font
  val(copy(s, 0, p - 1), qlty, err);
  delete(s, 1, p);

  if Pos('B', s) <> 0 then include(fstyles, fsBold);
  if Pos('I', s) <> 0 then include(fstyles, fsItalic);
  if Pos('S', s) <> 0 then include(fstyles, fsStrikeOut);
  if Pos('U', s) <> 0 then include(fstyles, fsUnderline);

  fnt.Charset     := chrs;                      //  Characterset of font
  fnt.Color       := clr;                       //  colour of font
  fnt.Height      := Hght;                      //  height of font
  fnt.Name        := nme;                       //  name of font
  fnt.Orientation := Ortn;                      //  orientation of font
  fnt.Size        := sze;                       //  size of font
  fnt.Pitch       := TFontPitch(ptch);          //  pitch of font
  fnt.Quality     := TFontQuality(qlty);        //  quality of font
  fnt.Style       := fstyles;

  StringToFont := fnt;
end;


function getTextColour(t1 : TColor ; t2 : TColor): TColor;
{  takes two colors, global[t2] and local[t1] and returns on acording the the rule.
   If the local in not default [clNone], return the local or else return the global               }
begin
  if t1 = clNone then
    getTextColour := t2  //  use local colour
  else
    getTextColour := t1  //  use global colour
end;

function getTextFont(f1 : TFont ; f2 : TFont): TFont;
{  takes two colors, global[t2] and local[t1] and returns on acording the the rule.
   If the local in not default [clNone], return the local or else return the global               }
begin
  if f1.IsDefault then
    getTextFont := f2  //  use local colour
  else
    getTextFont := f1  //  use global colour
end;

end.


