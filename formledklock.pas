unit formLEDKlock;

{  Implements a L.E.D. Klock using the L.E.D. Component from Visual Planit.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, VpLEDLabel, LMessages, formClipBoard;

type

  { TfrmLEDKlock }

  TfrmLEDKlock = class(TForm)
    LEDKlock: TVpLEDLabel;
    MnItmShowClipBoard: TMenuItem;
    MnItmAlwaysOnTop: TMenuItem;
    MnItmStickyNote: TMenuItem;
    MnItmExit: TMenuItem;
    MnItmAbout: TMenuItem;
    popUpMenuLEDKlock: TPopupMenu;
    TmrLEDKlock: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmAlwaysOnTopClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure MnItmShowClipBoardClick(Sender: TObject);
    procedure MnItmStickyNoteClick(Sender: TObject);
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
  kLog.writeLog('FormLEDKlock Create');
  Application.AddOnUserInputHandler(@MouseHook);
end;

procedure TfrmLEDKlock.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  Stop timer on close, so not running when form not in use.    }
begin
  TmrLEDKlock.Enabled := false;

  CloseAction := caFree;
end;

procedure TfrmLEDKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmLEDKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('FormLEDKlock Show');

  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;
  frmMain.Visible := False;


  frmLEDKlock.DoubleBuffered := true;
  TmrLEDKlock.Enabled        := True;         //  Start timer.

  if userOptions.LEDScreenSave then
  begin
    Left := userOptions.LEDFormLeft;
    Top := userOptions.LEDFormTop;
  end;

  MnItmAlwaysOnTop.Checked := userOptions.LEDAlwaysOnTop;
  if userOptions.LEDAlwaysOnTop then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TfrmLEDKlock.MouseHook(Sender: TObject; Msg: Cardinal);
{  Implements a draggable window.  Because the control fills the complete window
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
        Top  := WindowDragTopLeft.Y + (Mouse.CursorPos.Y - WindowDragMousePos.Y);
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
    WindowDragStarted   := True;
    WindowDragMousePos  := Mouse.CursorPos;
    WindowDragTopLeft.X := Left;
    WindowDragTopLeft.Y := Top;
  end;
end;

procedure TfrmLEDKlock.TmrLEDKlockTimer(Sender: TObject);
{  Sets the time to the LED Klock component.    }
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
procedure TfrmLEDKlock.MnItmAlwaysOnTopClick(Sender: TObject);
{  Toggle Always On Top for the form.    }
begin
  if MnItmAlwaysOnTop.Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

  userOptions.LEDAlwaysOnTop := MnItmAlwaysOnTop.Checked;
  userOptions.writeCurrentOptions;
end;

procedure TfrmLEDKlock.MnItmAboutClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
  frmAbout.ShowModal;                 //frmAbout is displayed
  FreeAndNil(frmAbout);               //frmAbout is released
end;

procedure TfrmLEDKlock.MnItmStickyNoteClick(Sender: TObject);
{  Create a new sticky note.    }
begin
  stickies.new(userOptions.stickyColor, userOptions.stickyFont);
end;

procedure TfrmLEDKlock.MnItmShowClipBoardClick(Sender: TObject);
{  Show the clipboard, if not visable.    }
begin
  frmClipBoard.Visible := true;
end;


procedure TfrmLEDKlock.MnItmExitClick(Sender: TObject);
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible     := True;
  TmrLEDKlock.Enabled := False;

  if userOptions.analogueScreenSave then
  begin
    userOptions.LEDFormLeft := Left;
    userOptions.LEDFormTop  := Top;
    userOptions.writeCurrentOptions;
  end;

  close;
end;

end.

