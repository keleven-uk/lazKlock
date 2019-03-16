unit formScrollingKlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, VpLEDLabel,
  FileUtil, Windows, LMessages, StdCtrls, ExtCtrls, formClipBoard, strUtils;

const
  NO_OF_COLUMNS = 50;    //  Number for columns of the LED display.


type

  { TfrmScrollingKlock }

  TfrmScrollingKlock = class(TForm)

    mnItmScroll       : TMenuItem;
    MnItmNewStickNote : TMenuItem;
    MnItmShowClipBoard: TMenuItem;
    N2                : TMenuItem;
    N1                : TMenuItem;
    MnItmAlwaysOnTop  : TMenuItem;
    MnItmAbout        : TMenuItem;
    MnItmExit         : TMenuItem;
    PopupMenu1        : TPopupMenu;
    ledKlock          : TVpLEDLabel;
    tmrScrollingKlock : TTimer;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure menuClick(Sender: TObject);
    procedure tmrScrollingKlockTimer(Sender: TObject);
  private
    WindowDragMousePos: TPoint;
    WindowDragTopLeft : TPoint;
    WindowDragStarted : Boolean;

    procedure MouseHook(Sender: TObject; Msg: Cardinal);
    procedure loadEvents;
    procedure setDisplay;
  public

  end;

var
  frmScrollingKlock: TfrmScrollingKlock;
  lines            : TStringList;
  scrollPos        : integer;

implementation

uses
  formklock, formAbout;

{$R *.lfm}

{ TfrmScrollingKlock }

procedure TfrmScrollingKlock.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  kLog.writeLog('formScollingKlock Close');

  lines.Free;
  tmrScrollingKlock.Enabled := false;

  if userOptions.scrollingTextScreenSave then
  begin
    userOptions.scrollingTextFormLeft := Left;
    userOptions.scrollingTextFormTop  := Top;
    userOptions.writeCurrentOptions;
  end;
  CloseAction := caFree;
end;

procedure TfrmScrollingKlock.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formScollingKlock Create');

  Application.AddOnUserInputHandler(@MouseHook);
end;

procedure TfrmScrollingKlock.FormDestroy(Sender: TObject);
begin
  // To prevent possible system resource leaks
  Application.RemoveOnUserInputHandler(@MouseHook);
end;

procedure TfrmScrollingKlock.FormShow(Sender: TObject);
begin
  kLog.writeLog('formScollingKlock Show');

  frmMain.TrayIcon.Visible := True;
  frmMain.Visible          := False;
  frmMain.TrayIcon.Show;

  if userOptions.scrollingTextScreenSave then
  begin
    Left := userOptions.scrollingTextFormLeft;
    Top  := userOptions.scrollingTextFormTop;
  end;

  MnItmAlwaysOnTop.Checked := userOptions.scrollingAlwaysOnTop;
  if userOptions.floatingAlwaysOnTop then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

  lines := TStringList.Create;               //  create a store for the events

  loadEvents;                                //  loads evenst file.

  tmrScrollingKlock.Enabled := true;
end;

procedure TfrmScrollingKlock.MouseHook(Sender: TObject; Msg: Cardinal);
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

procedure TfrmScrollingKlock.loadEvents;
{  Loads the events from a text file, if present.
   The events file should be called events.tx.
   The location of the events.txt is in userOptions.eventsName.
     something like - C:\Users\keleven\AppData\Local\lazKlock
}
VAR
  fileLocation: string;
begin
  fileLocation := userOptions.eventsName;
  try
    lines.LoadFromFile(fileLocation);
  except
    //  no events file - complain silently
  end;

  setDisplay;
end;

procedure TfrmScrollingKlock.setDisplay;
{  Sets up the display depending weather the message will be scrolling
   or multi-line, this is set by a menu item.
}
begin
  if mnItmScroll.Checked then
    ledKlock.Rows := 1
  else
    ledKlock.Rows := lines.Count + 1;

  ledKlock.Columns := NO_OF_COLUMNS;

  frmScrollingKlock.Width  := ledKlock.Width;
  frmScrollingKlock.Height := ledKlock.Height;
end;

//
// ******************************************************* timer **************
//
procedure TfrmScrollingKlock.tmrScrollingKlockTimer(Sender: TObject);
{  On every timer tick, The scrolling events text is read and the
   scrolling text is created, doubled and then the start position is
   incremented.  This gives a moving window over the text.
}
var
  split   : TStringArray;
  display : string;
  sTime   : string;
  line    : string;
  eName   : string;
  oLength : integer;
  flag    : boolean;
  eDate   : TDateTime;
  timediff: TDateTime;
begin
  setDisplay;

  if mnItmScroll.Checked then
    sTime   := FormatDateTime('hh:nn:ss', now)
  else
    sTime   := PadRight(FormatDateTime('hh:nn:ss', now), 10);

  display := format('%s : %s', [sTime, FormatDateTime('ddd dd MMM YYYY', now)]);

  if lines.Count <> 0 then
  begin
    for line in lines do
    begin
      split := line.Split(',');

      try
        eDate    := StrToDate(split[1]) + StrToTime('23:59');  //  To make it midnight, so day count works
        timeDiff := eDate - Now;                               //  Need to add time to event.
        flag     := true;
      except  //  no date on time set.
        flag := false;
      end;

      if mnItmScroll.Checked then
      begin
        eName := split[0];
        display += '    ';
      end
      else
      begin
        eName := PadRight(split[0], 10);
        display += lineEnding;
      end;

      if flag then
        display  += Format('%s : %d days %s',
                         [eName,
                          trunc(timediff),
                          FormatDateTime('h" hrs "n" mins "s" secs"', timediff)])
      else  //  no date or time set - so just use name.
        display  += eName;

    end;  //  for line in lines do
  end;    //  if lines.Count <> 0 then

  if mnItmScroll.Checked then
  begin
    oLength := length(display) + 4;  //  We add 4 for the 4 spaces on the next line.
    display := display + '    ' + display;
    ledKlock.Caption := Copy(display, scrollPos, NO_OF_COLUMNS);

    inc(scrollPos);
    if scrollPos > oLength then scrollPos := 1;
  end
  else
    ledKlock.Caption := display;

end;
//
// ******************************************************* Pop Up Menu *********
//
procedure TfrmScrollingKlock.menuClick(Sender: TObject);
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

    userOptions.scrollingAlwaysOnTop := MnItmAlwaysOnTop.Checked;
    userOptions.writeCurrentOptions;
  end;
  'MnItmReloadEvents' : loadEvents;  //  re-loads evenst file.
  'MnItmNewStickNote' : stickies.new(userOptions.stickyColor, userOptions.stickyFont);
  'MnItmShowClipBoard': frmClipBoard.Visible := true;
  'MnItmAbout':
    begin
    frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
    frmAbout.ShowModal;                 //frmAbout is displayed
    FreeAndNil(frmAbout);               //frmAbout is released
  end;
  'MnItmExit':
      begin
      frmMain.TrayIcon.Visible := False;
      frmMain.TrayIcon.Hide;

      frmMain.Visible := True;

      close;
      end;
  end;
end;

end.

