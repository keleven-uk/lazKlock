unit UKlockUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Process,
  MMSystem, dateutils, registry;

type                    //  used to hold the parsed data for a reminder.
  reminderData = record
    message: string;     //  the formatted message for the reminder.
    orDate : TDateTime;  //  date of original event.
    rmDate : TDateTime;  //  date of next reminder due.
    period : Integer;    //  period of reminder in months.
    toGo   : Double ;    //  days to go for reminder.
    active : boolean;
  end;

function FontToString(f: TFont): string;
function StringToFont(s: string): TFont;
function getTextFont(f1: TFont; f2: TFont): TFont;
function parseReminder(a: string): reminderData;
procedure doSystemEvent(event: integer);
procedure abortSystemEvent;
procedure doCommandEvent(command: string);
procedure doPlaySound(sound: string);
procedure applyRunAtStartUp(flag: Boolean);

implementation

function FontToString(f: TFont): string;
{  Produces a string representation of a given font.
   All the font attributes are converted to strings and packed together.

   Does not yet handle errors - will there be any ;o)                                              }
var
  rtnStr: string;
begin
  rtnStr := '';
  rtnStr := rtnStr + IntToStr(f.Charset) + ':';
  rtnStr := rtnStr + ColorToString(f.Color) + ':';
  rtnStr := rtnStr + IntToStr(f.Height) + ':';
  rtnStr := rtnStr + (f.Name) + ':';
  rtnStr := rtnStr + IntToStr(f.Orientation) + ':';
  rtnStr := rtnStr + IntToStr(f.size) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Pitch)) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Quality)) + ':';

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

function StringToFont(s: string): TFont;
{  Produces a font from a given string representation [produced by FonttoString.
   The string is read a bit at at time [upto the next :] and this is converted
   to the given font attribute.

   NB :: pos starts from position 1, where copy start at position 1.
         TFontStyles is a set and scanned differently.

   Does not yet handle errors - will there be any ;o)                                              }
var
  p: integer;
  err: integer;
  chrs: integer;
  clr: TColor;
  Hght: integer;
  nme: string;
  Ortn: integer;
  sze: integer;
  ptch: integer;
  qlty: integer;

  fnt: TFont;
  fstyles: TFontStyles;
begin
  fnt := TFont.Create;
  fstyles := [];             //  empty set ??

  p := Pos(':', s);                                   //  Character set of font
  val(copy(s, 0, p - 1), chrs, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                   //  colour of font
  clr := StringToColor(copy(s, 0, p - 1));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  height of font
  val(copy(s, 0, p - 1), Hght, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  name of font
  nme := copy(s, 0, p - 1);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  orientation of font
  val(copy(s, 0, p - 1), Ortn, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  size of font
  val(copy(s, 0, p - 1), sze, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  pitch of font
  val(copy(s, 0, p - 1), ptch, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  quality of font
  val(copy(s, 0, p - 1), qlty, err);
  Delete(s, 1, p);

  if Pos('B', s) <> 0 then
    include(fstyles, fsBold);
  if Pos('I', s) <> 0 then
    include(fstyles, fsItalic);
  if Pos('S', s) <> 0 then
    include(fstyles, fsStrikeOut);
  if Pos('U', s) <> 0 then
    include(fstyles, fsUnderline);

  fnt.Charset := chrs;                     //  Character set of font
  fnt.Color := clr;                        //  colour of font
  fnt.Height := Hght;                      //  height of font
  fnt.Name := nme;                         //  name of font
  fnt.Orientation := Ortn;                 //  orientation of font
  fnt.Size := sze;                         //  size of font
  fnt.Pitch := TFontPitch(ptch);           //  pitch of font
  fnt.Quality := TFontQuality(qlty);       //  quality of font
  fnt.Style := fstyles;

  StringToFont := fnt;
end;


function getTextFont(f1: TFont; f2: TFont): TFont;
{  takes two fonts, global[f2] and local[f1] and returns one according the the rule.
   If the local is default return the local, else return the global.           }
begin
  if f1.IsDefault then
    getTextFont := f2  //  use local colour
  else
    getTextFont := f1;  //  use global colour
end;

procedure doSystemEvent(event: integer);
{  called to do a system event.
         0 = shut down PC
         1 = reboot PC
         2 = Hibernate PC
         3 = log off current user

    NB :: if cross platform, shutdown command would need changing.

    It seems that things changed a little in windows 10, a little.
    The parameters now start with a / and not a -.
    Aslo, passing parameters with contain spaces seem to br problamatic for lazarus,
    i gave up.  Needs more work to make compatible with older versions of windows,
    to include a message and make cross platform.
}
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);

  case event of
    0:
    begin
      AProcess.Parameters.Add('/s');
    end;
    1:
    begin
      AProcess.Parameters.Add('/r');
    end;
    2:
    begin
      AProcess.Parameters.Add('/h');
    end;
    3:
    begin
      AProcess.Parameters.Add('/l');
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
      Executable := 'shutdown';
      Parameters.Add('/a');
      Execute;
    finally
      Free;
    end;    //  with TProcess.Create(nil)

end;

procedure doCommandEvent(command: string);
{  called to execute a command, which hopefully is in command.
   NB  :: does not check if the command is really executable.                                     }
begin
  if command <> '' then
  begin
    with TProcess.Create(nil) do
      try
        Executable := Command;
        Execute;
      finally
        Free;
      end;    //  with TProcess.Create(nil)
  end       //  if command <> ''
  else
    ShowMessage('Even Klock cant run with nowt!!');

end;

procedure doPlaySound(sound: string);
var
  PCharSoundName: PChar;       // PlaySound needs to be passed PChar and not a string
begin
  PCharSoundname := @sound[1];  //  convert to PCHAR - a pointer to first character
  //  of the string - i think.
  try                                        //  in case sound file is not found.
    PlaySound(PCharSoundname, 0, SND_ASYNC);
  except
    on EInOutError do
      beep;
  end;
end;

function parseReminder(a: string): reminderData;
{  a is a string containing the Reminder, comma delimited.
   pos 0 = name
       1 = date
       2 = period [Yearly/Monthly]
       3 = type[Wedding/Birthday/Motor/One Off/Other
       4 = active[-1/0]
}
var
  rmndrData : reminderData;

  name  : string;
  date  : string;
  period: string;
  Rtype : string;
  active: String;

  p : integer;
  y : integer;
begin
  p := Pos(',', a);                                  //  name of reminder
  name := copy(a, 0, p - 1);
  Delete(a, 1, p);

  p := Pos(',', a);                                  //  date of reminder
  date := copy(a, 0, p - 1);
  Delete(a, 1, p);
  rmndrData.orDate  := StrToDate(date);

  p := Pos(',', a);                                  //  period of reminder
  period := copy(a, 0, p - 1);
  Delete(a, 1, p);
  if period = ' Yearly' then
    rmndrData.period:= 12
  else
    rmndrData.period:= 1;

  p := Pos(',', a);                                  //  type of reminder
  Rtype := copy(a, 0, p - 1);
  Delete(a, 1, p);

  active := a;                                      //  is reminder active
  Delete(a, 1, p);
  rmndrData.active  := StrToBool(active);


  y := YearOf(Now);                                  //  check if reminder passed
  if MonthOf(rmndrData.orDate) < MonthOf(Now) then   //  for this year, if it has
    y += 1;                                          //  then increment year.

  if (MonthOf(rmndrData.orDate) = MonthOf(Now)) and  //  if this month, check the
     (Dayof(rmndrData.orDate) < Dayof(Now)) then     //  if the day has passed
       y += 1;

  rmndrData.rmDate  := EncodeDateTime(y,
                           MonthOf(rmndrData.orDate),
                           DayOf(rmndrData.orDate),
                           0, 0, 0, 0);


  rmndrData.toGo := DaySpan(Now, rmndrData.rmDate);

  case Rtype of
    ' Wedding'  : rmndrData.message := format('%s have a wedding anniversary, in %3.f days [%s]', [name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]) ;
    ' Birthday' : rmndrData.message := format('%s has a Birthday, in %3.f days [%s]', [name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]) ;
    ' Motor'    : rmndrData.message := format('%s , in %3.f days [%s]', [name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]) ;
  end;

  parseReminder := rmndrData;
end;
procedure applyRunAtStartUp(flag: Boolean);

CONST
  rootPath = HKEY_CURRENT_USER;
  regpath = '\Software\Microsoft\Windows\CurrentVersion\run';
VAR
  registry: TRegistry;
  AppPath: String;
  AppName: String;

  openResult: boolean;
begin
  registry := TRegistry.Create;
  registry.RootKey := rootpath;
  registry.Access := KEY_WRITE;

  AppPath := Application.ExeName;
  AppName := ExtractFileName(AppPath);

  try
    openResult := registry.OpenKey(regpath, true);

    if not openResult then
      showMessage('Error in opening key')
    else
      begin
        if flag then
         registry.WriteString(AppNAme, AppPath)
        else
          registry.DeleteValue(AppNAme)
      end;

    registry.CloseKey ;
  finally
    registry.Free;
  end;  //  try

end;

end.
