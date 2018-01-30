unit formBinaryKlock;

{  Implements a binary Klock, which can be run in either true binary
   or BCD [Binary Coded decimal] mode.  This mode is selected via a
   menu activated by right clicking on the LED Klock.

   The L.E.D's are actually implemented by using rectangular shapes,
   these can be either ON_COLOUR or OFF_COLOUR.
   The shape was used instead of a L.E.D. component because they
   can be positioned closer together and they use LED system resources.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, LCLIntf, LCLType, Menus, LMessages, StdCtrls, strutils;

{ TODO : Should these be user selectable. }
const
  ON_COLOUR = clLIME;
  OFF_COLOUR = clBlack;


type

  { TfrmBinaryKlock }

  TfrmBinaryKlock = class(TForm)
    lblStatusBar: TLabel;
    MnItmNewStickNote: TMenuItem;
    MnItmBCD: TMenuItem;
    MnItmBinary: TMenuItem;
    MnItmExit: TMenuItem;
    MnItmAbout: TMenuItem;
    PnlStatusbar: TPanel;
    popUpMenuBinaryKlock: TPopupMenu;
    Shp00: TShape;
    Shp31: TShape;
    Shp41: TShape;
    Shp51: TShape;
    Shp02: TShape;
    Shp12: TShape;
    Shp22: TShape;
    Shp32: TShape;
    Shp42: TShape;
    Shp52: TShape;
    Shp03: TShape;
    Shp10: TShape;
    Shp13: TShape;
    Shp23: TShape;
    Shp33: TShape;
    Shp43: TShape;
    Shp53: TShape;
    Shp20: TShape;
    Shp30: TShape;
    Shp40: TShape;
    Shp50: TShape;
    Shp01: TShape;
    Shp11: TShape;
    Shp21: TShape;
    tmrBinartKlock: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmBCDClick(Sender: TObject);
    procedure MnItmBinaryClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure MnItmNewStickNoteClick(Sender: TObject);
    procedure tmrBinartKlockTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure UpdateStatusBar(KTime: TDateTime);
    procedure setShapes;

    procedure decodeBCDTime;
    function BCDTime: string;
    procedure decodeBinaryTime;
    function binaryTime: string;
  public

  end;

var
  frmBinaryKlock: TfrmBinaryKlock;

implementation

uses
  formklock, formAbout;

{$R *.lfm}

{ TfrmBinaryKlock }

procedure TfrmBinaryKlock.tmrBinartKlockTimer(Sender: TObject);
{  Called on each timer tick.    }
begin
  if MnItmBinary.Checked then
    decodeBinaryTime
  else
    decodeBCDTime;

  UpdateStatusBar(now);
end;

procedure TfrmBinaryKlock.FormCreate(Sender: TObject);
begin
  kLog.writeLog('FormBinaryKlock Create');
  Application.AddOnUserInputHandler(@MouseHook);

  setShapes;
end;

procedure TfrmBinaryKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmBinaryKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('formBinaryKlock Show');

  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;

  frmMain.Visible := False;
  tmrBinartKlock.Enabled := true;

  if userOptions.BinaryFormat then
    MnItmBinary.Checked := true
  else
    MnItmBCD.Checked := true;

  setShapes;

  if userOptions.BinaryScreenSave then
  begin
    klog.writeLog('Setting Binary Klock screen position');
    Left := userOptions.BinaryFormLeft;
    Top := userOptions.BinaryFormTop;
  end;
end;

procedure TfrmBinaryKlock.setShapes;
{  The binary Klock only uses 4 rows of L.E.D's, so if in this mode
   switch off the bottom row.  The BCD klock uses all 5 rows.
}
begin
  if MnItmBinary.Checked then
  begin
    klog.writeLog('Binary Klock');
    Shp03.Visible := false;
    Shp13.Visible := false;
    Shp23.Visible := false;
    Shp33.Visible := false;
    Shp43.Visible := false;
    Shp53.Visible := false;
  end
  else
  begin
    Shp03.Visible := true;
    Shp13.Visible := true;
    Shp23.Visible := true;
    Shp33.Visible := true;
    Shp43.Visible := true;
    Shp53.Visible := true;
  end;
end;

procedure TfrmBinaryKlock.MouseHook(Sender: TObject; Msg: Cardinal);
{  Implements a dragable window.  Because the control fills the complete window
   We cant just catch the forms mouse events - so we use a global hook and
   filter out just the mouse movements.
}
begin
  if (Msg <> LM_MOUSEMOVE) and (Msg <> LM_LBUTTONUP) and (Msg <> LM_LBUTTONDOWN) then Exit;

  { MouseMove - Code to drag the main window using the mouse}
  if msg = LM_MOUSEMOVE then
  begin
    if WindowDragStarted then
      begin
        Left := WindowDragTopLeft.X + (Mouse.CursorPos.X - WindowDragMousePos.X);
        Top := WindowDragTopLeft.Y + (Mouse.CursorPos.Y - WindowDragMousePos.Y);
      end;
  end;

  { MouseUp - Code to drag the main window using the mouse }
  if msg = LM_LBUTTONUP then
  begin
    WindowDragStarted := False;
  end;

  { MouseDown - Code to drag the main window using the mouse}
  if msg = LM_LBUTTONDOWN then
  begin
    WindowDragStarted := True;
    WindowDragMousePos := Mouse.CursorPos;
    WindowDragTopLeft.X := Left;
    WindowDragTopLeft.Y := Top;
  end;
end;

procedure TfrmBinaryKlock.UpdateStatusBar(KTime: TDateTime);
{  Updates the status bar with current time, date and Key States.
   NB, status bar is implemented as a panel allows proper background
   colour change and transparency.
}
VAR
  keyResult: string;
begin
  keyResult := ' :: cns ';
  if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then
    keyResult := StringReplace(keyResult, 'c', 'C', [rfReplaceAll]);
  if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then
    keyResult := StringReplace(keyResult, 'n', 'N', [rfReplaceAll]);
  if LCLIntf.GetKeyState(VK_SCROLL) <> 0 then
    keyResult := StringReplace(keyResult, 's', 'S', [rfReplaceAll]);

  if userOptions.displayIdleTime then
    keyResult += '  :: Idle Time :: ' + FormatDateTime('hh:nn:ss', tick / SecsPerDay);

  lblStatusBar.Caption := FormatDateTime('hh:nn:ss am/pm ', KTime) +
                          FormatDateTime(' DD MMM YYYY ', KTime) +
                          keyResult;


end;
//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmBinaryKlock.MnItmAboutClick(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmBinaryKlock.MnItmBCDClick(Sender: TObject);
begin
  setShapes;
end;

procedure TfrmBinaryKlock.MnItmBinaryClick(Sender: TObject);
begin
  setShapes;
end;

procedure TfrmBinaryKlock.MnItmExitClick(Sender: TObject);
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible := True;
  tmrBinartKlock.Enabled := false;

  if userOptions.BinaryScreenSave then
  begin
    klog.writeLog('Saving Binary Klock screen position');
    userOptions.BinaryFormLeft := Left;
    userOptions.BinaryFormTop := Top;
    userOptions.writeCurrentOptions;
  end;

  close;
end;

procedure TfrmBinaryKlock.MnItmNewStickNoteClick(Sender: TObject);
begin
  stickies.new;
end;

//
// ****************************************************** Binary Stuff *********
//
function TfrmBinaryKlock.binaryTime: string;
{  Returns a binary string of the current time.    }
var
  hrs: word;
  min: word;
  sec: word;
  msc: word;

  ssec: string;
  smin: string;
  shrs: string;
begin
  DecodeTime(Time, hrs, min, sec, msc);

  if not userOptions.display24Hour and (hrs > 12) then        //  use 12 hour klock.
    hrs -= 12;

  shrs := Dec2Numb(hrs, 5, 2);
  smin := Dec2Numb(min, 6, 2);
  ssec := Dec2Numb(sec, 6, 2);

  Result := format('%s %s %s', [shrs, smin, ssec]);
end;

procedure TfrmBinaryKlock.decodeBinaryTime;
{  Parses the binary string and turns on or off the required L.E.D's.    }
var
  f : integer;
  tmpButton: TShape;
  sl: TstringList;
begin
  sl := TstringList.Create;
  sl.StrictDelimiter:=true;
  sl.LineBreak:=' ';
  sl.text := binaryTime;

  for f := 0 to 5 do
  begin
    // hour
    tmpButton := FindComponent('Shp' + intToStr(f) + '0') as TShape;
    if tmpButton <> nil then
    begin
      if sl[0][f] = '1' then
        tmpButton.Brush.Color := ON_COLOUR
      else
        tmpButton.Brush.Color := OFF_COLOUR;

    end;

    // mins
    tmpButton := FindComponent('Shp' + intToStr(f) + '1') as TShape;
    if tmpButton <> nil then
    begin
      if sl[1][f+1] = '1' then
        tmpButton.Brush.Color := ON_COLOUR
      else
        tmpButton.Brush.Color := OFF_COLOUR;
    end;

    // secs
    tmpButton := FindComponent('Shp' + intToStr(f) + '2') as TShape;
    if tmpButton <> nil then
    begin
      if sl[2][f+1] = '1' then
        tmpButton.Brush.Color := ON_COLOUR
      else
        tmpButton.Brush.Color := OFF_COLOUR;
     end;
  end; // for

  sl.free;
end;

function TfrmBinaryKlock.BCDTime: string;
{  Returns a BCD binary string of the current time.    }
var
  hrs: word;
  min: word;
  sec: word;
  msc: word;

  ssec: string;
  smin: string;
  shrs: string;
begin
  DecodeTime(Time, hrs, min, sec, msc);

  if not userOptions.display24Hour and (hrs > 12) then        //  use 12 hour klock.
    hrs -= 12;

  if hrs < 9 then
    shrs := format('0000%s', [Dec2Numb(hrs, 4, 2)])
  else
    shrs := format('%s%s', [Dec2Numb(hrs div 10, 4, 2), Dec2Numb(hrs mod 10, 4, 2)]);

  if min < 9 then
    smin := format('0000%s', [Dec2Numb(min, 4, 2)])
  else
    smin := format('%s%s', [Dec2Numb(min div 10, 4, 2), Dec2Numb(min mod 10, 4, 2)]);

  if sec < 9 then
    ssec := format('0000%s', [Dec2Numb(sec, 4, 2)])
  else
    ssec := format('%s%s', [Dec2Numb(sec div 10, 4, 2), Dec2Numb(sec mod 10, 4, 2)]);

  Result := format('%s %s %s', [shrs, smin, ssec]);
end;

procedure TfrmBinaryKlock.decodeBCDTime;
{  Parses the binary string and turns on or off the required L.E.D's.    }
var
  f : integer;
  tmpShape: TShape;
  sl: TstringList;
begin
  sl := TstringList.Create;
  sl.StrictDelimiter:=true;
  sl.LineBreak:=' ';
  sl.text := BCDTime;

  for f := 1 to 4 do
  begin
    // hour
    tmpShape := FindComponent('Shp0' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[0][f] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;
    tmpShape := FindComponent('Shp1' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[0][f+4] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;

     // mins
    tmpShape := FindComponent('Shp2' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[1][f] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;
    tmpShape := FindComponent('Shp3' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[1][f+4] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;

    // secs
    tmpShape := FindComponent('Shp4' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[2][f] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;
    tmpShape := FindComponent('Shp5' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[2][f+4] = '1' then
        tmpShape.Brush.Color := ON_COLOUR
      else
        tmpShape.Brush.Color := OFF_COLOUR;
     end;

   end;   //  for
end;      //  procedure

end.

