unit formAnalogueKlock;

{  Implements a Analogue Klock, using the analogue Klock from BGRA Controls.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, dtthemedclock, Forms, Controls,
  Graphics, Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, Windows, LMessages;

const
  LWA_COLORKEY = 1;
  LWA_ALPHA = 2;
  LWA_BOTH = 3;
  WS_EX_LAYERED = $80000;
  GWL_EXSTYLE = -20;

{Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal Color As Long, ByVal X As Byte, ByVal alpha As Long) As Boolean }
function SetLayeredWindowAttributes(hWnd: longint; Color: longint; X: byte; alpha: longint): bool stdcall; external 'USER32';

{not sure how to alias these functions here ????   alias setwindowlonga!!}
{Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long }
function SetWindowLongA(hWnd: longint; nIndex: longint; dwNewLong: longint): longint stdcall; external 'USER32';


{Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long }
function GetWindowLongA(hWnd: longint; nIndex: longint): longint stdcall; external 'user32';

type

  { TfrmAnalogueKlock }

  TfrmAnalogueKlock = class(TForm)
    DTThemedClock1: TDTThemedClock;
    MnItmNewStickyNote: TMenuItem;
    MnItmAbout: TMenuItem;
    MnItmExit: TMenuItem;
    popUpMenuAnalogueKlock: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure MnItmNewStickyNoteClick(Sender: TObject);

  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
  public

  end;

var
  frmAnalogueKlock: TfrmAnalogueKlock;

implementation

{$R *.lfm}

uses
  formklock, formAbout;


{ TfrmAnalogueKlock }

procedure SetTranslucent(ThehWnd: longint; Color: longint; nTrans: integer);
{  Used to make the form transparent.

   See http://lazplanet.blogspot.co.uk/2013/04/make-your-forms-transparent.html
}
var
  attrib: longint;
begin

  {SetWindowLong and SetLayeredWindowAttributes are API functions, see MSDN for details }
  attrib := GetWindowLongA(ThehWnd, GWL_EXSTYLE);
  SetWindowLongA(ThehWnd, GWL_EXSTYLE, attrib or WS_EX_LAYERED);

  {anything with color value color will completely disappear if flag = 1 or flag = 3  }
  SetLayeredWindowAttributes(ThehWnd, Color, nTrans, 1);
end;

procedure TfrmAnalogueKlock.FormCreate(Sender: TObject);
{  On form create set up transparency stuff.  }
var
  transparency: longint;
begin
  kLog.writeLog('FormAnalogue Klock Create');

  Application.AddOnUserInputHandler(@MouseHook);

  {the color were going to make transparent the red that the form background is set to}
  transparency := clBlack;

  {call the function to do it}
  SetTranslucent(frmAnalogueKlock.Handle, transparency, 0);
end;

procedure TfrmAnalogueKlock.FormDestroy(Sender: TObject);
begin
    // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmAnalogueKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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

procedure TfrmAnalogueKlock.FormShow(Sender: TObject);
{  When starting the analogue klock, start the tray icon and hide the main klock.  }
begin
  kLog.writeLog('FormAnalogue Klock Show');
  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;

  frmMain.Visible := False;

  if userOptions.analogueScreenSave then
  begin
    Left := userOptions.analogueFormLeft;
    Top := userOptions.analogueFormTop;
  end;
end;
//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmAnalogueKlock.MnItmExitClick(Sender: TObject);
{  When exiting the analogue klock, kill off the tray icon and restore the main klock.
}
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible := True;

  if userOptions.analogueScreenSave then
  begin
    userOptions.analogueFormLeft := Left;
    userOptions.analogueFormTop := Top;
    userOptions.writeCurrentOptions;
  end;

  Close;
end;

procedure TfrmAnalogueKlock.MnItmAboutClick(Sender: TObject);
{  Load the about page.  }
begin
  frmAbout.Show;
end;

procedure TfrmAnalogueKlock.MnItmNewStickyNoteClick(Sender: TObject);
begin
  stickies.new(userOptions.stickyColor, userOptions.stickyFont);
end;
end.
