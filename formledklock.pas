unit formLEDKlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, VpLEDLabel, LMessages;

type

  { TfrmLEDKlock }

  TfrmLEDKlock = class(TForm)
    LEDKlock: TVpLEDLabel;
    MnItmExit: TMenuItem;
    MnItmAbout: TMenuItem;
    popUpMenuLEDKlock: TPopupMenu;
    TmrLEDKlock: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure TmrLEDKlockTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
  public

  end;

var
  frmLEDKlock: TfrmLEDKlock;

implementation

uses
  formklock, formAbout;

{$R *.lfm}

{ TfrmLEDKlock }

procedure TfrmLEDKlock.FormCreate(Sender: TObject);
begin
  kLog.writeLog('FormLEDKlock Klock Create');
  Application.AddOnUserInputHandler(@MouseHook);
end;

procedure TfrmLEDKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmLEDKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('FormLEDKlock Klock Show');
  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;

  frmMain.Visible := False;
  TmrLEDKlock.Enabled := True;

  if userOptions.LEDScreenSave then
  begin
    Left := userOptions.LEDFormLeft;
    Top := userOptions.LEDFormTop;
  end;
end;

procedure TfrmLEDKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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

procedure TfrmLEDKlock.TmrLEDKlockTimer(Sender: TObject);
VAR
  topLine: string;
  btmLine: string;
begin
  if userOptions.display24Hour then
    topLine := FormatDateTime('   hh:nn:ss', now)
  else
    topLine := FormatDateTime('  hh:nn:ss am/pm', now);

  if userOptions.LEDlongDate then
    btmLine := FormatDateTime('ddd dd MMM YYYY', now)
  else
    btmLine := FormatDateTime('  dd MM YYYY', now);

  LEDKlock.Caption := topLine  + lineending + btmLine;
end;
//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmLEDKlock.MnItmAboutClick(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmLEDKlock.MnItmExitClick(Sender: TObject);
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible := True;
  TmrLEDKlock.Enabled := False;

  if userOptions.analogueScreenSave then
  begin
    userOptions.LEDFormLeft := Left;
    userOptions.LEDFormTop := Top;
    userOptions.writeCurrentOptions;
  end;

  close;
end;


end.

