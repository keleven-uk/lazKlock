unit UKlockUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Process, MMSystem;


Function FontToString(f : TFont): String;
Function StringToFont(s : String): TFont;
function getTextFont(f1 : TFont ; f2 : TFont): TFont;
procedure doSystemEvent(event : Integer);
procedure abortSystemEvent;
procedure doCommandEvent(command : String);
procedure doPlaySound(sound : String);

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


function getTextFont(f1 : TFont ; f2 : TFont): TFont;
{  takes two fonts, global[f2] and local[f1] and returns one acording the the rule.
   If the local is default return the local, else return the global.           }
begin
  if f1.IsDefault then
    getTextFont := f2  //  use local colour
  else
    getTextFont := f1  //  use global colour
end;

procedure doSystemEvent(event : Integer);
{  called to do a system event.
         0 = shut down PC
         1 = reboot PC
         2 = Hibernate PC
         3 = log off current user

    NB :: if cross platform, shutdown cammand would need changing.                                }
VAR
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);

  case event of
    0 : begin
      AProcess.Parameters.Add('-s');
      AProcess.Parameters.Add('"-t 10"');
      AProcess.Parameters.Add('"-c "Shuting Down PC in 10 Seconds by Klock');
    end;
    1 : begin
      AProcess.Parameters.Add('-r');
      AProcess.Parameters.Add('"-t 10"');
      AProcess.Parameters.Add('"-c "Restarting PC in 10 Seconds by Klock')
    end;
    2 : begin
      AProcess.Parameters.Add('-h');
      AProcess.Parameters.Add('"-t 10"');
      AProcess.Parameters.Add('"-c "Hibernate PC in 10 Seconds by Klock');
    end;
    3 : begin
      AProcess.Parameters.Add('-l');
      AProcess.Parameters.Add('"-t 10"');
      AProcess.Parameters.Add('"-c "Logging off user in 10 Seconds by Klock');
    end;
  end;

  try
    AProcess.Options := [poWaitOnExit];
    AProcess.Executable := 'shutdown';
    AProcess.Execute;
  finally
    AProcess.Free;
  end;

end;
procedure abortSystemEvent;
{  tries to abort a system event - i.e. shutdown                               }
begin
  with TProcess.Create(nil) do
    try
      Executable := 'shutdown.exe';
      Parameters.Add('-a');
      Execute;
    finally
      Free;
  end;    //  with TProcess.Create(nil)

end;

procedure doCommandEvent(command : String);
{  called to execute a command, which hopefully is in command.
   NB  :: does not check if the command is really executable.                                     }
begin
  if command <> '' then begin
    with TProcess.Create(nil) do
      try
        Executable := Command;
        Execute;
      finally
        Free;
    end;    //  with TProcess.Create(nil)
  end       //  if comand <> ''
  else
    ShowMessage('Even Klock cant run with nowt!!');

end;

procedure doPlaySound(sound : String);
VAR
  PCharSoundName : PChar;       // PlaySound needs to be passed PChar and not a string
begin
  PCharSoundname := @sound[1];  //  convert to PCHAR - a pointer to first character
                                             //  of the string - i think.
  try                                        //  in case sound file is not found.
    PlaySound(PCharSoundname, 0, SND_ASYNC);
  except
    on EInOutError do beep ;
  end;
end;

end.


