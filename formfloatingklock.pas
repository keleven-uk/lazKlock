unit formFloatingKlock;

{  Implements a Floating Text Klock, the textual display of the time seems to
   float on the screen - all other item i.e. form is transparent.

   The time format is taken from the main klock.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LMessages,
  Menus, StdCtrls, ExtCtrls, Windows;

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

  { TfrmFloatingKlock }

  TfrmFloatingKlock = class(TForm)
    FontDialog1: TFontDialog;
    lblFloatingTime: TLabel;
    MnItmFont: TMenuItem;
    MnItmAbout: TMenuItem;
    MnItmNewStickNote: TMenuItem;
    MnItmExit: TMenuItem;
    PopupMenu1: TPopupMenu;
    TmrFloatingText: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmFontClick(Sender: TObject);
    procedure MnItmAboutClick(Sender: TObject);
    procedure MnItmExitClick(Sender: TObject);
    procedure MnItmNewStickNoteClick(Sender: TObject);
    procedure TmrFloatingTextTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
  public

  end;

var
  frmFloatingKlock: TfrmFloatingKlock;

implementation

uses
  formklock, formAbout;

{$R *.lfm}

{ TfrmFloatingKlock }

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

procedure TfrmFloatingKlock.FormCreate(Sender: TObject);
var
  transparency: longint;
begin
  kLog.writeLog('formFloatingKlock Create');

  TmrFloatingText.Enabled := false;

  Application.AddOnUserInputHandler(@MouseHook);

  {the color were going to make transparent the red that the form background is set to}
  transparency := clBlack;

  {call the function to do it}
  SetTranslucent(frmFloatingKlock.Handle, transparency, 0);

  frmFloatingKlock.Color := clBlack;
end;

procedure TfrmFloatingKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmFloatingKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('formFloatingKlock Show');
  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;

  frmMain.Visible := False;

  TmrFloatingText.Enabled := true;

  if userOptions.floatingTextScreenSave then
  begin
    Left := userOptions.floatingTextFormLeft;
    Top := userOptions.floatingTextFormTop;
  end;
end;

procedure TfrmFloatingKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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

procedure TfrmFloatingKlock.TmrFloatingTextTimer(Sender: TObject);
{  Updates the time in the correct font.
   The time details are taken from the the main Klock, that is
   this will display what ever Klock was displaying when this was called.
}
begin
  lblFloatingTime.Top := 8;
  lblFloatingTime.Left := 4;
  lblFloatingTime.Font.Size := 22;
  lblFloatingTime.AutoSize := true;

  if userOptions.floatingTextUseKlockFont then      //  if true use main form font
    lblFloatingTime.Font := frmMain.lblfuzzy.font
  else                                              //  else use custom font, if set
    lblFloatingTime.Font := userOptions.floatingTextFont;

  //  Guards agains the font colour being black or default,
  //  this would cause the label to apear transparent and not been seen.
  if (lblFloatingTime.Font.Color = clDefault) or
     (lblFloatingTime.Font.Color = clBlack) then
    lblFloatingTime.Font.Color := clLime;

  lblFloatingTime.Caption := ft.getTime;

  lblFloatingTime.AdjustFontForOptimalFill;
end;

//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmFloatingKlock.MnItmAboutClick(Sender: TObject);
{  Load the about form.    }
begin
  frmAbout.Show;
end;

procedure TfrmFloatingKlock.MnItmExitClick(Sender: TObject);
{  Close the Floating Text Klock.    }
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible := True;

  TmrFloatingText.Enabled := false;

  if userOptions.floatingTextScreenSave then
  begin
    userOptions.floatingTextFormLeft := Left;
    userOptions.floatingTextFormTop := Top;
    userOptions.writeCurrentOptions;
  end;

  close;
end;

procedure TfrmFloatingKlock.MnItmFontClick(Sender: TObject);
{  Load the font dialog and save to user options if succesfull.    }
begin
  if FontDialog1.Execute then
  begin
    userOptions.floatingTextFont := FontDialog1.Font;
    userOptions.writeCurrentOptions;
  end;
end;

procedure TfrmFloatingKlock.MnItmNewStickNoteClick(Sender: TObject);
{  Create a new sticky note.    }
begin
  stickies.new(userOptions.stickyColor, userOptions.stickyFont);
end;

end.
