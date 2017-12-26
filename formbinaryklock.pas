unit formBinaryKlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, LCLIntf, LCLType, Menus, LMessages, StdCtrls, strutils;

type

  { TfrmBinaryKlock }

  TfrmBinaryKlock = class(TForm)
    MnItmBCD: TMenuItem;
    MnItmBinary: TMenuItem;
    MnItmExit: TMenuItem;
    MnItmAbout: TMenuItem;
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
    StsBinaryKlock: TStatusBar;
    tmrBinartKlock: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmBCDClick(Sender: TObject);
    procedure MnItmBinaryClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure StsBinaryKlockDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
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
begin
  if MnItmBinary.Checked then
    decodeBinaryTime
  else
    decodeBCDTime;

  UpdateStatusBar(now);
end;

procedure TfrmBinaryKlock.FormCreate(Sender: TObject);
begin
  kLog.writeLog('FormBinaryKlock Klock Create');
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
  kLog.writeLog('formBinaryKlock Klock Show');

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
{  Updates the status bar.    }
VAR
  keyResult: string;
begin
  keyResult := ' cns ';
    if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then
      keyResult[2] := 'C';
    if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then
      keyResult[3] := 'N';
    if LCLIntf.GetKeyState(VK_SCROLL) <> 0 then
      keyResult[4] := 'S';

    if userOptions.display24Hour then
      StsBinaryKlock.Panels.Items[0].Text := FormatDateTime('hh:nn:ss', KTime)
    else
      StsBinaryKlock.Panels.Items[0].Text := FormatDateTime('hh:nn:ss am/pm', KTime);

    StsBinaryKlock.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', KTime);
    StsBinaryKlock.Panels.Items[2].Text := keyResult;

    if userOptions.displayIdleTime then
      StsBinaryKlock.Panels.Items[3].Text := 'Idle Time :: ' + FormatDateTime('hh:nn:ss', tick / SecsPerDay)
    else
      StsBinaryKlock.Panels.Items[3].Text := '';
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
//
// ****************************************************** Binary Stuff *********
//
function TfrmBinaryKlock.binaryTime: string;
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
        tmpButton.Brush.Color := clGreen
      else
        tmpButton.Brush.Color := clBlack;

    end;

    // mins
    tmpButton := FindComponent('Shp' + intToStr(f) + '1') as TShape;
    if tmpButton <> nil then
    begin
      if sl[1][f+1] = '1' then
        tmpButton.Brush.Color := clGreen
      else
        tmpButton.Brush.Color := clBlack;
    end;

    // secs
    tmpButton := FindComponent('Shp' + intToStr(f) + '2') as TShape;
    if tmpButton <> nil then
    begin
      if sl[2][f+1] = '1' then
        tmpButton.Brush.Color := clGreen
      else
        tmpButton.Brush.Color := clBlack;
     end;
  end; // for

  sl.free;
end;

function TfrmBinaryKlock.BCDTime: string;
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
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;
    tmpShape := FindComponent('Shp1' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[0][f+4] = '1' then
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;

     // mins
    tmpShape := FindComponent('Shp2' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[1][f] = '1' then
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;
    tmpShape := FindComponent('Shp3' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[1][f+4] = '1' then
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;

    // secs
    tmpShape := FindComponent('Shp4' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[2][f] = '1' then
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;
    tmpShape := FindComponent('Shp5' + intToStr(f-1)) as TShape;
    if tmpShape <> nil then
    begin
      if sl[2][f+4] = '1' then
        tmpShape.Brush.Color := clGreen
      else
        tmpShape.Brush.Color := clBlack;
     end;

   end;   //  for
end;      //  procedure
//
// ****************************************************** Status Bar ***********
//
procedure TfrmBinaryKlock.StsBinaryKlockDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
VAR
  OldColor, OldBrushColor : TColor;
  OldStyle : TFontStyles;
  f: integer;
begin

  for f:= 0 to 4 do
  begin
    if Panel.Index = f then begin
      with StatusBar.Canvas do begin
        // store off the original settings
        OldColor := Font.Color;
        OldStyle := Font.Style;
        OldBrushColor := Brush.Color;
        try
          //  set the Brush Color
          //// fill the panel with the brush color (ie background color)
          //brush.Color := clBlack;
          FillRect(Rect);
          // set the text font color / style
          Font.Color := clGreen;
          //Font.Style := [fsBold];
          //display the text from the panel
          TextOut(Rect.Left + 3, Rect.Top, Panel.Text);
        finally  // restore the original settings
          Font.Color := OldColor;
          Font.Style := OldStyle;
          Brush.Color := OldBrushColor;
        end;  //  try
      end;    //  with StatusBar.Canvas
    end;      //  if Panel.Index = 0
  end;        //  for f:= 0 to 4

end;

end.

