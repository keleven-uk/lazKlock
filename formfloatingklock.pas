unit formFloatingKlock;

{  Implements a Floating Text Klock, the textual display of the time seems to
   float on the screen - all other item i.e. form is transparent.

   The time format is taken from the main klock.
}

{$mode objfpc}{$H+}

interface

//  Graphics has to come after Windows - so TBitmap.Create works.
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Windows, Graphics, Dialogs,
  LMessages, Menus, StdCtrls, ExtCtrls, formClipBoard;

const
  LWA_COLORKEY  = 1;
  LWA_ALPHA     = 2;
  LWA_BOTH      = 3;
  WS_EX_LAYERED = $80000;
  GWL_EXSTYLE   = -20;

{Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal Color As Long, ByVal X As Byte, ByVal alpha As Long) As Boolean }
function SetLayeredWindowAttributes(hWnd: longint; Color: longint; X: byte; alpha: longint): bool stdcall; external 'USER32';

{not sure how to alias these functions here ????   alias setwindowlonga!!}
{Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long }
function SetWindowLongA(hWnd: longint; nIndex: longint; dwNewLong: longint): longint stdcall; external 'USER32';


{Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long }
function GetWindowLongA(hWnd: longint; nIndex: longint): longint stdcall; external 'user32';


type

  { TfrmFloatingKlock }

  textSize = record
    width : integer;
    height: integer;
  end;

  TfrmFloatingKlock = class(TForm)
    FontDialog1       : TFontDialog;
    lblFloatingTime   : TLabel;
    MnItmShowClipBoard: TMenuItem;
    MnItmAlwaysOnTop  : TMenuItem;
    MnItmFont         : TMenuItem;
    MnItmAbout        : TMenuItem;
    MnItmNewStickNote : TMenuItem;
    MnItmExit         : TMenuItem;
    PopupMenu1        : TPopupMenu;
    TmrFloatingText   : TTimer;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure menuClick(Sender: TObject);
    procedure TmrFloatingTextTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft : TPoint;
    WindowDragStarted : Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    function GetTextSize(AText: String; AFont: TFont): textSize;
  public

  end;

var
  frmFloatingKlock: TfrmFloatingKlock;
  nowTime         : string;
  prvTime         : string;

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

  {the color were going to make transparent the black that the form background is set to}
  transparency := clBlack;

  {call the function to do it}
  SetTranslucent(frmFloatingKlock.Handle, transparency, 0);

  frmFloatingKlock.Color := clBlack;
end;

procedure TfrmFloatingKlock.FormClose(Sender: TObject;  var CloseAction: TCloseAction);
{  Stop timer on close, so not running when form not in use.    }
begin
  kLog.writeLog('formFloatingKlock Close');
  TmrFloatingText.Enabled := false;

  if userOptions.floatingTextScreenSave then
  begin
    userOptions.floatingTextFormLeft := Left;
    userOptions.floatingTextFormTop  := Top;
    userOptions.writeCurrentOptions;
  end;
  CloseAction := caFree;
end;

procedure TfrmFloatingKlock.FormDestroy(Sender: TObject);
begin
  kLog.writeLog('formFloatingKlock Destroy');
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmFloatingKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('formFloatingKlock Show');

  frmMain.TrayIcon.Visible := True;
  frmMain.Visible          := False;
  frmMain.TrayIcon.Show;

  TmrFloatingText.Enabled := true;         //  Start timer.

  if userOptions.floatingTextScreenSave then
  begin
    Left := userOptions.floatingTextFormLeft;
    Top  := userOptions.floatingTextFormTop;
  end;

  MnItmAlwaysOnTop.Checked := userOptions.floatingAlwaysOnTop;
  if userOptions.floatingAlwaysOnTop then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

  nowTime := ft.getTime;
  prvTime := 'Klock';     //  so times are different first time

  lblFloatingTime.Top      := 4;
  lblFloatingTime.Left     := 4;
  lblFloatingTime.AutoSize := true;
end;

procedure TfrmFloatingKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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

procedure TfrmFloatingKlock.TmrFloatingTextTimer(Sender: TObject);
{  Updates the time in the correct font.
   The time details are taken from the main Klock.

   The dimensions of the form are adjusted so that the text string will fit.
}
begin
  nowTime := ft.getTime;      //  No need to update ever second in showing
                              //  time in words or fuzzytime etc
                              //  i.e. time changes every one or five minutes.

  if nowTime = prvTime then   //  if times are the same, no need to do owt.
    exit
  else
    prvTime := nowTime;

  if userOptions.floatingTextUseKlockFont then      //  if true use main form font
    lblFloatingTime.Font := frmMain.lblfuzzy.font
  else                                              //  else use custom font, if set
    lblFloatingTime.Font := userOptions.floatingTextFont;

  //  Guards against the font colour being black or default,
  //  this would cause the label to apear transparent and not been seen.
  if (lblFloatingTime.Font.Color  = clDefault) or
     (lblFloatingTime.Font.Color  = clBlack) then
      lblFloatingTime.Font.Color := clLime;

  lblFloatingTime.Caption := nowTime;

  width  := GetTextSize(lblFloatingTime.Caption, lblFloatingTime.Font).width;
  height := GetTextSize(lblFloatingTime.Caption, lblFloatingTime.Font).height;

  lblFloatingTime.AdjustFontForOptimalFill;
end;

function TfrmFloatingKlock.GetTextSize(AText: String; AFont: TFont): textSize;
{  Returns the height and width of a text string with a given font.    }
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font.Assign(AFont);

    Result.width  := bmp.Canvas.TextWidth(AText);
    Result.height := bmp.Canvas.TextHeight(AText);
  finally
    bmp.Free;
  end;
end;

//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmFloatingKlock.menuClick(Sender: TObject);
{  A generic click routine called for menu actions.

   The sender should be a TMenuItem.
}
VAR
  itemName   : string;
begin
  itemName := '';

  //  set the appropiate name.
  if (Sender is TMenuItem) then
    itemName := TMenuItem(Sender).Name;

  if itemName = '' then exit;                                //  not called by a TMenuItem.

  case itemName of
    'MnItmAlwaysOnTop'  :
    begin
      if MnItmAlwaysOnTop.Checked then
        FormStyle := fsSystemStayOnTop
      else
        FormStyle := fsNormal;

      userOptions.floatingAlwaysOnTop := MnItmAlwaysOnTop.Checked;
      userOptions.writeCurrentOptions;
    end;
    'MnItmFont'         :
    begin
      if FontDialog1.Execute then
      begin
        userOptions.floatingTextFont := FontDialog1.Font;
        lblFloatingTime.Font         := FontDialog1.Font;   //  so the new font is used at once.

        width  := GetTextSize(lblFloatingTime.Caption, lblFloatingTime.Font).width;
        height := GetTextSize(lblFloatingTime.Caption, lblFloatingTime.Font).height;

        lblFloatingTime.AdjustFontForOptimalFill;

        userOptions.writeCurrentOptions;
      end;
    end;
    'MnItmNewStickNote' : stickies.new(userOptions.stickyColor, userOptions.stickyFont);
    'MnItmShowClipBoard': frmClipBoard.Visible := true;
    'MnItmAbout'        :
    begin
      frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
      frmAbout.ShowModal;                 //frmAbout is displayed
      FreeAndNil(frmAbout);               //frmAbout is released
    end;
    'MnItmExit'         :
    begin
    frmMain.TrayIcon.Visible := False;
    frmMain.TrayIcon.Hide;

    frmMain.Visible := True;

    TmrFloatingText.Enabled := false;

    close;
    end;
  end;
end;

end.

