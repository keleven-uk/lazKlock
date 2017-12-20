unit UKlockUtils;

{$mode objfpc}{$H+}

interface

//  Graphics has to come after Windows - so TBitmap.Create works.
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Process,
  MMSystem, dateutils, registry, typinfo, LCLVersion, strutils, Windows, Graphics;

type                    //  used to hold the parsed data for a reminder.
  reminderData = record
    message: string;     //  the formatted message for the reminder.
    orDate: TDateTime;  //  date of original event.
    rmDate: TDateTime;  //  date of next reminder due.
    period: integer;    //  period of reminder in months.
    toGo: double;    //  days to go for reminder.
    active: boolean;
  end;
  //  Used for hour chimes file name.  Zero not used, inserted as a dummy to keep count correct.
  chimes = (zero, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve);
var
  isPlaying: Boolean = False;

function FontToString(f: TFont): string;
function StringToFont(s: string): TFont;
function getTextFont(f1: TFont; f2: TFont): TFont;
function parseReminder(a: string): reminderData;
function isTime(myNow: TdateTime; mins: integer): Boolean;
procedure doSystemEvent(event: integer);
procedure abortSystemEvent;
procedure doCommandEvent(command: string);
procedure doPlaySound(sound: string; volume: string);
procedure SendMCICommand(command: string);
procedure applyRunAtStartUp(flag: boolean);
procedure playChime(mode: String);
procedure logHeader;
procedure logFooter;
function getUpTime(system: string): string;
function getWindowsVersion: string;
function GetTextWidth(AText: String; AFont: TFont): Integer;
function isChristmas(): Boolean;

implementation

uses
  formklock;

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


  Result := rtnStr;
end;

function StringToFont(s: string): TFont;
{  Produces a font from a given string representation [produced by FonttoString.
   The string is read a bit at at time [up to the next :] and this is converted
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

  Result := fnt;
end;


function getTextFont(f1: TFont; f2: TFont): TFont;
{  takes two fonts, global[f2] and local[f1] and returns one according the rule.
   If the local is default return the local, else return the global.           }
begin
  if f1.IsDefault then
    Result := f2  //  use local colour
  else
    Result := f1;  //  use global colour
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
    Also, passing parameters with contain spaces seem to be problematic for Lazarus,
    Needs more work to make compatible with older versions of windows, to include a message and
    make cross platform.
}
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);

  case event of
    0: AProcess.Parameters.Add('/s');
    1: AProcess.Parameters.Add('/r');
    2: AProcess.Parameters.Add('/h');
    3: AProcess.Parameters.Add('/l');
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

procedure doPlaySound(sound: string; volume: string);
{  Plays a sound file at a specified volume, both passed in has strings.
   The file is assumeded to be a valid sound file and volume is between 1 - 1000.
   The correct path is attatched by this routine.

   TODO :: Work out how status works, so that it can be determined when play has finished.
           This would then eliminate the global variable isPlying.

   Fails silently if sound file does not exist.
}
VAR
  soundFile: String;
begin
  soundFile := ExtractFilePath(Application.ExeName) + '\sounds\' + sound;

  If FileExists(soundFile) Then
  begin
    if isPlaying then
    begin
      SendMCICommand('close KlockAudio');                       //  close player if alrady been used.
      isPlaying := false;
    end;

    isPlaying := True;
    SendMCICommand('open ' + soundFile + ' type mpegvideo alias KlockAudio');      //  open audio player.

    SendMCICommand('setaudio KlockAudio volume to ' + volume);                     //  Sets volume [1 - 1000].

    //  SendMCICommand('seek KlockAudio to nil');              //  finds starts of  file, not used.

    SendMCICommand('play KlockAudio');                         //  Play audio file.

    //  SendMCICommand('status KlockAudio mode')               //  supposed to return status of play.
  end;

end;

procedure SendMCICommand(command: string);
{  pinched from http://www.programmersforum.ru/showthread.php?t=133180}
var
  returnValue: integer;
  errorMessage: array [0 .. 254] of char;
begin
  returnValue := mciSendString(PChar(command), nil, 0, 0);
  if returnValue <> 0 then
  begin
    { get message for returned value }
    mciGetErrorString(returnValue, errorMessage, 255);
    ShowMessage(errorMessage);
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
  rmndrData: reminderData;

  Name: string;
  date: string;
  period: string;
  Rtype: string;
  active: string;

  p: integer;
  y: integer;
begin
  p := Pos(',', a);                                  //  name of reminder
  Name := copy(a, 0, p - 1);
  Delete(a, 1, p);

  p := Pos(',', a);                                  //  date of reminder
  date := copy(a, 0, p - 1);
  Delete(a, 1, p);
  rmndrData.orDate := StrToDate(date);

  p := Pos(',', a);                                  //  period of reminder
  period := copy(a, 0, p - 1);
  Delete(a, 1, p);
  if period = ' Yearly' then
    rmndrData.period := 12
  else
    rmndrData.period := 1;

  p := Pos(',', a);                                  //  type of reminder
  Rtype := copy(a, 0, p - 1);
  Delete(a, 1, p);

  active := a;                                      //  is reminder active
  Delete(a, 1, p);
  rmndrData.active := StrToBool(active);


  y := YearOf(Now);                                  //  check if reminder passed
  if MonthOf(rmndrData.orDate) < MonthOf(Now) then   //  for this year, if it has
    y += 1;                                          //  then increment year.

  if (MonthOf(rmndrData.orDate) = MonthOf(Now)) and  //  if this month, check the
    (Dayof(rmndrData.orDate) < Dayof(Now)) then     //  if the day has passed
    y += 1;

  rmndrData.rmDate := EncodeDateTime(y,
    MonthOf(rmndrData.orDate),
    DayOf(rmndrData.orDate),
    0, 0, 0, 0);


  rmndrData.toGo := DaySpan(Now, rmndrData.rmDate);

  case Rtype of
    ' Wedding': rmndrData.message := format('%s have a wedding anniversary, in %3.f days [%s]', [Name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]);
    ' Birthday': rmndrData.message := format('%s has a Birthday, in %3.f days [%s]', [Name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]);
    ' Motor': rmndrData.message := format('%s , in %3.f days [%s]', [Name, rmndrData.toGo, DateToStr(rmndrData.rmDate)]);
  end;

  Result := rmndrData;
end;

procedure applyRunAtStartUp(flag: boolean);

const
  rootPath = HKEY_CURRENT_USER;
  regpath = '\Software\Microsoft\Windows\CurrentVersion\run';
var
  registry: TRegistry;
  AppPath: string;
  AppName: string;

  openResult: boolean;
begin
  registry := TRegistry.Create;
  registry.RootKey := rootpath;
  registry.Access := KEY_WRITE;

  AppPath := Application.ExeName;
  AppName := ExtractFileName(AppPath);

  try
    openResult := registry.OpenKey(regpath, True);

    if not openResult then
      ShowMessage('Error in opening key')
    else
    begin
      if flag then
        registry.WriteString(AppNAme, AppPath)
      else
        registry.DeleteValue(AppNAme);
    end;

    registry.CloseKey;
  finally
    registry.Free;
  end;  //  try

end;

function isTime(myNow: TdateTime; mins: integer): Boolean;
{  Returns true if the time is at the hour.    }
Var
  hour, minute, second,  millisecond: word;
begin
  DecodeTime(myNow, hour, minute, second, millisecond);
  result := (minute = mins) and (second = 0);
end;

procedure playChime(mode: String);
var
  hour, minute, second,  millisecond: word;
  arg: string;
begin
  DecodeTime(Time, hour, minute, second, millisecond);

  if hour > 12 then
    hour := hour - 12;

  case mode of
    'pips': arg := 'thepips.mp3';
    'hour': arg := GetEnumName(TypeInfo(chimes), hour) + '.mp3';
    'half': arg := 'halfchime.mp3';
    'quarter': arg := 'quarterchime.mp3';
    'threequarter': arg := 'threequarterchime.mp3';
  else ;
    arg := '';
  end;

  doPlaySound(arg, userOptions.volume);
end;

procedure logHeader;
{  Write header infomation to log file.    }
begin
  kLog.writeLog('............................................................');
  kLog.writeLog(userOptions.InternalName);
  kLog.writeLog('User : ' + SysUtils.GetEnvironmentVariable('USERNAME'));  // not the one in windows.
  kLog.writeLog('PC   : ' + SysUtils.GetEnvironmentVariable('COMPUTERNAME'));
  kLog.writeLog('OS   : ' + getWindowsVersion);
  kLog.writeLog(format('lazKlock Build   :: %s', [userOptions.productVersion]));
  kLog.writeLog(format('lazKlock Version :: %s', [userOptions.fileVersion]));
  {$ifdef WIN32}
    kLog.writeLog(format('Built with 32 bit Lazarus Version :: %s', [lcl_version]));
  {$else}
    kLog.writeLog(format('Built with 64 bit Lazarus Version :: %s', [lcl_version]));
  {$endif}
  kLog.writeLog('App Dir : ' + ExtractFilePath(Application.ExeName));
  kLog.writeLog('............................................................');
end;

procedure logFooter;
{  Write fotter to log file.    }
begin
  kLog.writeLog('............................................................');
  kLog.writeLog('Klock has been running for ' + getUpTime('Application'));
  kLog.writeLog('Klock Ending [normaly]');
  kLog.writeLog('............................................................');
end;

function getUpTime(system: string): string;
{  Determines the up time of either System or Application - depending on argument S or A.
   Used in formAbout & uKlockUtils.

   appStartTime := GetTickCount64; needs to be run when the app starts.

   NOTE :: Windows Only - use LclIntf.GetTickCount for cross platform.

   TODO : Need to check for roll over and account for it.
}
var
  noTicks: int64;
  noSeconds: integer;
  noOfDays: integer;
  noOfHours: integer;
  noOfMinutes: integer;
  noOfSeconds: integer;
begin
  system := AnsiLowerCase(system);

  if AnsiStartsStr('s', system) then
    noTicks := GetTickCount64                     //  How long has the system been running.
  else
    noTicks := GetTickCount64 - appStartTime;     //  How long has the application been running.

  noSeconds := noTicks div 1000;                  //  1000 ticks per second.

  noOfDays := noSeconds div 86400;
  noSeconds := noSeconds - (noOfDays * 86400);
  noOfHours := noSeconds div 3600;
  noSeconds := noSeconds - (noOfHours * 3600);
  noOfMinutes := noSeconds div 60;
  noSeconds := noSeconds - (noOfMinutes * 60);
  NoOfSeconds := noSeconds;

  Result := format('%d days : %d hours : %d mins : %d secs', [noOfDays, noOfHours, noOfMinutes, noOfSeconds]);
end;

function getWindowsVersion: string;
{  Gets the version of the windows OS.
   This is achieved by caputuring the output from the DOS command ver.
   The ver command is run  into a file and then the file is read back.
}
VAR
  AProcess: TProcess;
  winVer: TStringList;
  tmpFileName: string;
begin
  tmpFileName := GetTempDir(true) + 'ver.txt';

  AProcess := TProcess.Create(nil);
  AProcess.Options := [poWaitOnExit, poUsePipes];
  AProcess.Executable := 'CMD ';
  AProcess.Parameters.Add('/C ver >' + tmpFileName);
  AProcess.Execute;

  winVer := TStringList.Create;
  winVer.LoadFromFile(tmpFileName);

  AProcess.Free;

  Result := winVer[1];
end;

function GetTextWidth(AText: String; AFont: TFont): Integer;
var
  bmp: TBitmap;
begin
  Result := 0;
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(AFont);
    Result := bmp.Canvas.TextWidth(AText);
  finally
    bmp.Free;
  end;
end;

function isChristmas: Boolean;
VAR
  chritmasDay: TDateTime;
  currentDate: TDateTime;
begin
  chritmasDay := EncodeDate(Currentyear, 12, 25);
  currentDate := Date;

  if DaysBetween(chritmasDay, currentDate) < 13 then
    Result := true
  else
    Result := False;
end;

end.
