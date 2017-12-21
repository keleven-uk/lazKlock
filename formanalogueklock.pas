unit formAnalogueKlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, dtthemedclock, DTAnalogClock, Forms, Controls,
  Graphics, Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, Windows, formAbout;

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
function GetWindowLongA(hWnd: longint; nIndex: longint): longint stdcall;
  external 'user32';

type

  { TfrmAnalogueKlock }

  TfrmAnalogueKlock = class(TForm)
    DTThemedClock1: TDTThemedClock;
    MnItmAbout: TMenuItem;
    MnItmExit: TMenuItem;
    popUpMenuAnalogueKlock: TPopupMenu;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure Shape1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  private

  public
    moveAnalogueKlock: boolean;
  end;

var
  frmAnalogueKlock: TfrmAnalogueKlock;

implementation

{$R *.lfm}

uses
  formklock;


{ TfrmAnalogueKlock }

procedure SetTranslucent(ThehWnd: longint; Color: longint; nTrans: integer);
{  Used to make the form transparen.

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

  {the color were going to make transparent the red that the form background is set to}
  transparency := clBlack;

  {call the function to do it}
  SetTranslucent(frmAnalogueKlock.Handle, transparency, 0);
end;

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

procedure TfrmAnalogueKlock.MnItmAboutClick(Sender: TObject);
{  Load the about page.  }
begin
  frmAbout.Show;
end;

procedure TfrmAnalogueKlock.Shape1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
{  update mouse position when form is dragged i.e. left mouse button down  }
begin
  moveAnalogueKlock := True;
end;

procedure TfrmAnalogueKlock.Shape1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
{  make klock follow the mouse, when left button is held down.  }
begin
  if moveAnalogueKlock then
  begin
    frmAnalogueKlock.Left := mouse.CursorPos.x - Height;
    frmAnalogueKlock.Top := mouse.CursorPos.y - Width;
  end;
end;

procedure TfrmAnalogueKlock.Shape1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
{  Left mouse button released, stop dragging.  }
begin
  moveAnalogueKlock := False;
end;

end.
