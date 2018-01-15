unit formSmallTextKlock;

{  Implements a Text Klcok, where the time is highlighted within a text matrix.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, LCLType, LCLIntf, LMessages;

const
  LWA_COLORKEY = 1;
  LWA_ALPHA = 2;
  LWA_BOTH = 3;
  WS_EX_LAYERED = $80000;
  GWL_EXSTYLE = -20;
  ON_COLOUR = clLIME;
  OFF_COLOUR = clGray;

{Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal Color As Long, ByVal X As Byte, ByVal alpha As Long) As Boolean }
function SetLayeredWindowAttributes(hWnd: longint; Color: longint; X: byte; alpha: longint): bool stdcall; external 'USER32';

{not sure how to alias these functions here ????   alias setwindowlonga!!}
{Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long }
function SetWindowLongA(hWnd: longint; nIndex: longint; dwNewLong: longint): longint stdcall; external 'USER32';


{Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long }
function GetWindowLongA(hWnd: longint; nIndex: longint): longint stdcall; external 'user32';

type

  { TfrmSmallTextKlock }

  TfrmSmallTextKlock = class(TForm)
    lblSmallTextKlock: TLabel;
    MnuItmNewStickyNote: TMenuItem;
    MnuItmTransparent: TMenuItem;
    MnItmClose: TMenuItem;
    MnuItmAbout: TMenuItem;
    Panel1: TPanel;
    popUpMenuSmallTextKlock: TPopupMenu;
    tmrSmallTextKlock: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MnItmCloseClick(Sender: TObject);
    procedure MnuItmAboutClick(Sender: TObject);
    procedure MnuItmNewStickyNoteClick(Sender: TObject);
    procedure MnuItmTransparentClick(Sender: TObject);
    procedure tmrSmallTextKlockTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft: TPoint;
    WindowDragStarted: Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure UpdateStatusBar(KTime: TDateTime);
    procedure createlabels;
    procedure clearlabels;
    procedure setTime;

    procedure setIN;
    procedure setTHE;
    procedure setEVENING;
    procedure setMIDNIGHT;
    procedure setMORNING;
    procedure setON;
    procedure setAFTER;
    procedure setNOON;
    procedure setNINE;
    procedure setELEVEN;
    procedure setTWELVE;
    procedure setISH;
    procedure setFIVE;
    procedure setSIX;
    procedure setSEVEN;
    procedure setEIGHT;
    procedure setTEN;
    procedure setPAST;
    procedure setONE;
    procedure setTWO;
    procedure setTHREE;
    procedure setFOUR;
    procedure setTWENTY;
    procedure setTWENTYFIVE;
    procedure setABOUT;
    procedure setTO;
    procedure setIT;
    procedure setIS;
    procedure setTENM;
    procedure setHALF;
    procedure setQUARTER;
  public

  end;

var
  frmSmallTextKlock: TfrmSmallTextKlock;
  MyLabels: array [0..7, 0..19] of TLabel;

implementation

uses
  formklock, formAbout;

{$R *.lfm}

{ TfrmSmallTextKlock }

procedure SetTranslucent(ThehWnd: longint; Color: longint; nTrans: integer; flag: integer);
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
  SetLayeredWindowAttributes(ThehWnd, Color, nTrans, flag);
end;

procedure TfrmSmallTextKlock.FormCreate(Sender: TObject);

begin
  kLog.writeLog('formSmallTextKlock Klock Create');

  Application.AddOnUserInputHandler(@MouseHook);

  createlabels;

  lblSmallTextKlock.font.Name := 'hack';
  lblSmallTextKlock.Font.Color := ON_COLOUR;
  lblSmallTextKlock.Font.size := 12;
end;

procedure TfrmSmallTextKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmSmallTextKlock.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  tmrSmallTextKlock.Enabled := false;
  CloseAction := caFree;
end;

procedure TfrmSmallTextKlock.FormShow(Sender: TObject);
var
  transparency: longint;
begin
  kLog.writeLog('formSmallTextKlock Klock Show');

  {the color were going to make transparent the black that the form background is set to}
  transparency := clBlack;

  {call the function to do it}
  if userOptions.smallTextTransparent then
    SetTranslucent(frmSmallTextKlock.Handle, transparency, 0, 1)
  else
    SetTranslucent(frmSmallTextKlock.Handle, transparency, 0, 0);

  frmMain.TrayIcon.Visible := True;
  frmMain.TrayIcon.Show;
  frmMain.Visible := False;

  tmrSmallTextKlock.Enabled := true;

  if userOptions.smallTextScreenSave then
  begin
    Left := userOptions.smallTextFormLeft;
    Top := userOptions.smallTextFormTop;
  end;
end;

procedure TfrmSmallTextKlock.tmrSmallTextKlockTimer(Sender: TObject);
{  Called on each timer tick.    }
begin
  UpdateStatusBar(now);
  clearlabels;
  setTime;
end;

procedure TfrmSmallTextKlock.UpdateStatusBar(KTime: TDateTime);
{  Updates the status bar with current time, date and Key States.
   NB, status bar is implemented as a panel allows proper background
   colour change and transparency.
}
VAR
  keyResult: string;
begin
  keyResult := ' cns ';
  if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then
    keyResult := StringReplace(keyResult, 'c', 'C', [rfReplaceAll]);
  if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then
    keyResult := StringReplace(keyResult, 'n', 'N', [rfReplaceAll]);
  if LCLIntf.GetKeyState(VK_SCROLL) <> 0 then
    keyResult := StringReplace(keyResult, 's', 'S', [rfReplaceAll]);

  lblSmallTextKlock.Caption := FormatDateTime('hh:nn:ss am/pm ', KTime) +
                               FormatDateTime(' DD MMM YYYY ', KTime) +
                               keyResult;
end;

procedure TfrmSmallTextKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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
//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmSmallTextKlock.MnItmCloseClick(Sender: TObject);
begin
  frmMain.TrayIcon.Visible := False;
  frmMain.TrayIcon.Hide;

  frmMain.Visible := True;
  tmrSmallTextKlock.Enabled := False;

  if userOptions.smallTextScreenSave then
  begin
    userOptions.smallTextFormLeft := Left;
    userOptions.smallTextFormTop := Top;
    userOptions.writeCurrentOptions;
  end;

  close;
end;

procedure TfrmSmallTextKlock.MnuItmAboutClick(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmSmallTextKlock.MnuItmNewStickyNoteClick(Sender: TObject);
begin
  stickies.new;
end;

procedure TfrmSmallTextKlock.MnuItmTransparentClick(Sender: TObject);
{  Set the form transparency according to the menu option.    }
var
  transparency: longint;
begin
  MnuItmTransparent.Checked := not MnuItmTransparent.Checked;

  transparency := clBlack;

  if MnuItmTransparent.Checked then
    SetTranslucent(frmSmallTextKlock.Handle, transparency, 0, 1)
  else
    SetTranslucent(frmSmallTextKlock.Handle, transparency, 0, 0);

end;

//
// ******************************************************* Label Stuff *********
//
procedure TfrmSmallTextKlock.createlabels;
{  Creates a matrix of labels and assigns each with a random character.    }
var
  f, g: integer;
begin
  for g := 0 to 7 do
  begin
    for f := 0 to 19 do
    begin
      MyLabels[g, f] := TLabel.Create(frmSmallTextKlock);
      with MyLabels[g, f] do
      begin
        name := 'lbl' + intToStr(g)+ intToStr(f);
        font.Name := 'Hack';
        font.Size := 16;
        caption := Chr(ord('A') + Random(26));
        left := 4 + (f * 15);
        top := 4 + (g * 22);
        visible := true;
        parent := frmSmallTextKlock;
      end;  //  with MyLabels[f] do
    end;    //  for f := 0 to 19 do
  end;      //  for g := 0 to 7 do
end;

procedure TfrmSmallTextKlock.clearlabels;
var
  f, g: integer;
  tmpLbl: TLabel;
begin
  for g := 0 to 7 do
  begin
    for f := 0 to 19 do
    begin
      tmplbl := FindComponent('lbl' + intToStr(g)+ intToStr(f)) as TLabel;
      if tmplbl <> nil then
        tmplbl.Font.Color := OFF_COLOUR;
    end;
  end;
end;

procedure TfrmSmallTextKlock.setTime;
Var
  hour, minute, second,  millisecond: word;
  am: boolean;
  nrms: integer;
begin
  DecodeTime(Time, hour, minute, second, millisecond);

  nrms := minute - (minute mod 5);  //  gets nearest five minutes.
  if (minute mod 5) > 2 then          //   closer to next five minutes, go to next.
    nrms += 5;

  if hour < 12 then      //   if hour less then 12, in the morning else afternoon
    am := true
  else
    am := false;

  setIT;
  setIS;
                        //  handle the minutes.
  case nrms of
    0:
    begin
      setAbout;
      setISH;
    end;
    5:
    begin
      setTWENTYFIVE;              //  Use the uper five for minutes.
      setPAST
    end;
    10:
    begin
      setTENM;
      setPAST;
    end;
    15:
    begin
      setQUARTER;
      setPAST;
    end;
    20:
    begin
      setTWENTY;
      setPAST;
    end;
    25:
    begin
      setTWENTY;
      setTWENTYFIVE;
      setPAST;
    end;
    30:
    begin
      setHALF;
      setPAST;
    end;
    35:
    begin
      setTWENTY;
      setTWENTYFIVE;
      setTO;
    end;
    40:
    begin
      setTWENTY;
      setTO;
    end;
    45:
    begin
      setQUARTER;
      setTO;
    end;
    50:
    begin
      setTENM;
      setTO;
    end;
    55:
    begin
      setTWENTYFIVE;              //  Use the uper five for minutes.
      setTO
    end;
    60:
    begin
     setAbout;
     setISH;
    end;
  end;

                          //  handle the Hours.

  if nrms >30 then        //  ater half past, so increment hour.
    hour += 1;

   //   if the hour is 0 or 24 and no minutes - it must be midnight.
   //   if the hour is 12 and no minutes - it must be noon.

   //   if "pm" then afternoon, subtract 12 - only use 12 hour clock.

   if (hour = 12) and (nrms = 0) then      //  must be about noon.
   begin
     setABOUT;
     setNOON;
   end
   else if (hour = 0) and (nrms = 0) then  //  must be midnight
   begin
     setABOUT;
     setMIDNIGHT;
   end
   else                                    //  any other time, do the following.
   begin

     if am then                            //  in the morning
     begin
     setIN;
       setTHE;
       setMORNING;
     end
     else                                  //  in the afternoon / evening.
     begin
       hour -= 12;
       if hour >= 5 then                   //  in the evening
       begin
        setIN;
        setTHE;
        setEVENING;;
       end
       else                               //  in the afternoon
       begin
        setIN;
        setTHE;
        setAFTER;
        setNOON;
       end;
     end;

     case hour of                        //  determine the hour.
       0: setTWELVE;
       1: setONE;
       2: setTWO;
       3: setTHREE;
       4: setFOUR;
       5: setFIVE;
       6: setSIX;
       7: setSEVEN;
       8: setEIGHT;
       9: setNINE;
       10: setTEN;
       11: setELEVEN;
       12: setTWELVE;
     end;
   end;

end;

procedure TfrmSmallTextKlock.setIN;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl50') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl51') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setON;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl56') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl57') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setNOON;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl513') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl514') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl515') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl515') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setTHE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl53') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl54') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
  tmplbl := FindComponent('lbl55') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setAFTER;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl58') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'A';
  tmplbl := FindComponent('lbl59') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'F';
  tmplbl := FindComponent('lbl510') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl511') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl512') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
end;

procedure TfrmSmallTextKlock.setEVENING;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl70') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl71') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl72') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl73') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl74') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl75') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl76') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'G';
end;

procedure TfrmSmallTextKlock.setMORNING;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl66') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'M';
  tmplbl := FindComponent('lbl67') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl68') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
  tmplbl := FindComponent('lbl69') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl610') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl611') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl612') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'G';
end;

procedure TfrmSmallTextKlock.setMIDNIGHT;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl712') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'M';
  tmplbl := FindComponent('lbl713') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl714') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'D';
  tmplbl := FindComponent('lbl715') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl716') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl717') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'G';
  tmplbl := FindComponent('lbl718') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
  tmplbl := FindComponent('lbl719') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
end;

procedure TfrmSmallTextKlock.setNINE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl40') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl41') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl42') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl43') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setELEVEN;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl44') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl45') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'L';
  tmplbl := FindComponent('lbl46') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl47') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl48') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl49') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setTWELVE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl410') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl411') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'W';
  tmplbl := FindComponent('lbl412') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl413') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'L';
  tmplbl := FindComponent('lbl414') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl415') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setISH;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl416') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl417') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'S';
  tmplbl := FindComponent('lbl418') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
end;

procedure TfrmSmallTextKlock.setFIVE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl30') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'F';
  tmplbl := FindComponent('lbl31') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl32') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl33') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setSIX;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl34') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'S';
  tmplbl := FindComponent('lbl35') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl36') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'X';
end;

procedure TfrmSmallTextKlock.setSEVEN;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl37') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'S';
  tmplbl := FindComponent('lbl38') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl39') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl310') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl311') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
end;

procedure TfrmSmallTextKlock.setEIGHT;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl312') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl313') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl314') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'G';
  tmplbl := FindComponent('lbl315') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
  tmplbl := FindComponent('lbl316') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
end;

procedure TfrmSmallTextKlock.setTEN;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl317') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl318') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl319') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setPAST;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl20') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'P';
  tmplbl := FindComponent('lbl21') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'A';
  tmplbl := FindComponent('lbl22') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'S';
  tmplbl := FindComponent('lbl23') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
end;

procedure TfrmSmallTextKlock.setONE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl25') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl26') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl27') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setTWO;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl27') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl28') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'W';
  tmplbl := FindComponent('lbl29') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
end;

procedure TfrmSmallTextKlock.setTHREE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl210') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl211') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
  tmplbl := FindComponent('lbl212') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
  tmplbl := FindComponent('lbl213') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl214') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setFOUR;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl215') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'F';
  tmplbl := FindComponent('lbl216') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl217') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'U';
  tmplbl := FindComponent('lbl218') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
end;

procedure TfrmSmallTextKlock.setTWENTY;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl10') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl11') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'W';
  tmplbl := FindComponent('lbl12') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl13') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
  tmplbl := FindComponent('lbl14') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl15') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'Y';
end;

procedure TfrmSmallTextKlock.setTWENTYFIVE;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl16') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'F';
  tmplbl := FindComponent('lbl17') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl18') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'V';
  tmplbl := FindComponent('lbl19') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
end;

procedure TfrmSmallTextKlock.setABOUT;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl110') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'A';
  tmplbl := FindComponent('lbl111') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'B';
  tmplbl := FindComponent('lbl112') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
  tmplbl := FindComponent('lbl113') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'U';
  tmplbl := FindComponent('lbl114') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
end;

procedure TfrmSmallTextKlock.setTO;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl115') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl116') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'O';
end;

procedure TfrmSmallTextKlock.setIT;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl00') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl01') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
end;

procedure TfrmSmallTextKlock.setIS;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl03') as TLabel;
  tmpLbl.Font.Color := clLIME;
  tmpLbl.Caption := 'I';
  tmplbl := FindComponent('lbl04') as TLabel;
  tmpLbl.Font.Color := clLIME;
  tmpLbl.Caption := 'S';
end;

procedure TfrmSmallTextKlock.setTENM;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl06') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl07') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl08') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'N';
end;

procedure TfrmSmallTextKlock.setHALF;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl09') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'H';
  tmplbl := FindComponent('lbl010') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'A';
  tmplbl := FindComponent('lbl011') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'L';
  tmplbl := FindComponent('lbl012') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'F';
end;

procedure TfrmSmallTextKlock.setQUARTER;
VAR
  tmpLbl: TLabel;
begin
  tmplbl := FindComponent('lbl013') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'Q';
  tmplbl := FindComponent('lbl014') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'U';
  tmplbl := FindComponent('lbl015') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
  tmplbl := FindComponent('lbl016') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'T';
  tmplbl := FindComponent('lbl017') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'E';
  tmplbl := FindComponent('lbl018') as TLabel;
  tmpLbl.Font.Color := ON_COLOUR;
  tmpLbl.Caption := 'R';
end;

end.

