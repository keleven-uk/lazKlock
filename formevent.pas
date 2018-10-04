unit formEvent;

{  when called, displays an events message.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Windows, LMessages, ComObj, Variants;

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

  { TfrmEvent }

  TfrmEvent = class(TForm)
    btnAcknowledge: TButton;
    btnMute       : TButton;
    lblEvent      : TLabel;
    lblInfo       : TLabel;
    tmrEvent      : TTimer;

    procedure btnAcknowledgeClick(Sender: TObject);
    procedure btnMuteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrEventTimer(Sender: TObject);

  private
    _pos  : integer;
    _stage: integer;

    WindowDragMousePos: TPoint;
    WindowDragTopLeft : TPoint;
    WindowDragStarted : Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure speakEvent;
  public
    property pos  : integer read _pos   write _pos;
    property stage: integer read _stage write _stage;
  end;

var
  frmEvent: TfrmEvent;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmEvent }

procedure TfrmEvent.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  kLog.writeLog('formEvent Close : ' + name);
  tmrEvent.Enabled := false;
end;

procedure TfrmEvent.btnAcknowledgeClick(Sender: TObject);
{  Acknowledge has been clicked, event will not be displayed again.

   This is acieved by calling ev.acknowledgeEvent from the main event store.}
begin
  ev.acknowledgeEvent(pos, stage);
end;

procedure TfrmEvent.btnMuteClick(Sender: TObject);
{  Mute had been clicked, event will displayed when next called.    }
begin
 close;
end;

procedure TfrmEvent.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formEvent : ' + name);
  tmrEvent.Enabled := true;
  AlphaBlend       := true;

  Application.AddOnUserInputHandler(@MouseHook);
end;

procedure TfrmEvent.FormShow(Sender: TObject);
{  When event is first shown, speak the message - if desired.    }
begin
  if userOptions.eventsSpeakMesssage then speakEvent;
end;

procedure TfrmEvent.speakEvent;
{  Speak [using SAPI] the event message.    }
var
 SavedCW       : Word;
 SpVoice       : Variant;
 TextToBeSpoken: Variant;
begin
 TextToBeSpoken := lblEvent.Caption + lblInfo.Caption;

 SpVoice := CreateOleObject('SAPI.SpVoice');

 // Change FPU interrupt mask to avoid SIGFPE exceptions
 SavedCW := Get8087CW;
 try
   Set8087CW(SavedCW or $4);
   SpVoice.Volume := formklock.userOptions.speakTimeVolume;  //  Use speak time volume for speak events volume.
   SpVoice.Speak(TextToBeSpoken, 0);
 finally
   // Restore FPU mask
   Set8087CW(SavedCW);
   SpVoice := Unassigned;
 end;  // try
end;

procedure TfrmEvent.FormDestroy(Sender: TObject);
begin
    // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmEvent.FormMouseEnter(Sender: TObject);
begin
  tmrEvent.Enabled := false;
  AlphaBlend       := false;
  AlphaBlendValue  := 255;
end;

procedure TfrmEvent.FormMouseLeave(Sender: TObject);
begin
  AlphaBlend       := true;
  tmrEvent.Enabled := true;
end;

procedure TfrmEvent.tmrEventTimer(Sender: TObject);
begin
  if AlphaBlendValue > 100 then
    AlphaBlendValue := AlphaBlendValue - 1
  else
    tmrEvent.Enabled := false;
end;

procedure TfrmEvent.MouseHook(Sender: TObject; Msg: Cardinal);
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

end.

