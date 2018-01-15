unit formClipBoard;

{  Implements a Clipboard managed, the form display item that have been copied
   to the clipboard, so they can be further copied at a later date.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, UformClipBoardUtils, LCLIntf, LCLType, Clipbrd, dateUtils,
  LazFileUtils, ShellApi;

type

  { TfrmClipBoard }

  TfrmClipBoard = class(TForm)
    btnClpBrdClearClipboard: TButton;
    btnClpBrdClearMonitor: TButton;
    btnClpBrdLoad: TButton;
    btnClpBrdSave: TButton;
    btnClpBrdClose: TButton;
    ImgClipBoard: TImage;
    LstVwClipBoard: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    StsBrClipBoard: TStatusBar;
    Timer1: TTimer;
    procedure btnClpBrdClearClipboardClick(Sender: TObject);
    procedure btnClpBrdClearMonitorClick(Sender: TObject);
    procedure btnClpBrdCloseClick(Sender: TObject);
    procedure btnClpBrdLoadClick(Sender: TObject);
    procedure btnClpBrdSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LstVwClipBoardClick(Sender: TObject);
    procedure LstVwClipBoardCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FListener: TClipboardListener;

    procedure UpdateStatusBar(KTime: TDateTime);
    procedure ClipboardChanged(Sender: TObject);
    procedure deleteToRycycle(aFile: string);
    function isThere(s: string): boolean;
  public
    procedure cullTmpFiles;
  end;

var
  frmClipBoard: TfrmClipBoard;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmClipBoard }
//
//....................................... Form Stuff ...........................
//
procedure TfrmClipBoard.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  kLog.writeLog('formClipBoard Close');

  if userOptions.CB_ScreenSave then
  begin
    userOptions.CB_formTop := Top;
    userOptions.CB_formLeft := Left;
    userOptions.writeCurrentOptions;
  end;
  CloseAction := caFree;
end;

procedure TfrmClipBoard.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formClipBoard Create');
  FListener := TClipboardListener.Create;
  FListener.OnClipboardChange := @ClipboardChanged;
end;

procedure TfrmClipBoard.FormDestroy(Sender: TObject);
begin
  FListener.Free;
end;

procedure TfrmClipBoard.FormShow(Sender: TObject);
begin
  if userOptions.CB_ScreenSave then
  begin
    Top := userOptions.CB_formTop;
    Left := userOptions.CB_formLeft;
    userOptions.writeCurrentOptions;
  end;
end;

procedure TfrmClipBoard.FormActivate(Sender: TObject);
begin
  kLog.writeLog('formClipBoard Activate');
end;
//
//....................................... Buttons ..............................
//
procedure TfrmClipBoard.btnClpBrdClearMonitorClick(Sender: TObject);
begin
  lstVwClipBoard.Clear;
end;

procedure TfrmClipBoard.btnClpBrdCloseClick(Sender: TObject);
begin
    close;
end;

procedure TfrmClipBoard.btnClpBrdLoadClick(Sender: TObject);
begin

end;

procedure TfrmClipBoard.btnClpBrdSaveClick(Sender: TObject);
begin

end;

procedure TfrmClipBoard.btnClpBrdClearClipboardClick(Sender: TObject);
begin
  clipboard.Clear;
end;
//
//....................................... ListView .............................
//
procedure TfrmClipBoard.LstVwClipBoardClick(Sender: TObject);
{  When a row of the listview is clicked, pass that row to the clipboard.    }
var
  item: TListItem;
  bitmap: TBitmap;
begin
  item := LstVwClipBoard.Selected;

  if Item.Caption = 'Image' then
  begin
    bitmap := TBitmap.Create;
    bitmap.LoadFromFile(Item.SubItems.Strings[1]);
    bitmap.Canvas.StretchDraw(rect(0, 0, ImgClipBoard.Width, ImgClipBoard.Height), bitmap);
    bitmap.SetSize(ImgClipBoard.Width, ImgClipBoard.Height);
    ImgClipBoard.Picture.Bitmap := bitmap;
    bitmap.Free;
  end
  else
    ImgClipBoard.Picture.Clear;

  FListener.CopyToClipboard(item);
end;

procedure TfrmClipBoard.LstVwClipBoardCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Item.caption = 'Text' then
    Sender.Canvas.Font.Color := clRed;
  if Item.caption = 'File' then
    Sender.Canvas.Font.Color := clgreen;
  if Item.caption = 'Dir' then
    Sender.Canvas.Font.Color := clolive;
  if Item.caption = 'Image' then
    Sender.Canvas.Font.Color := clblue;
end;
//
//....................................... Timer ...............................
//
procedure TfrmClipBoard.Timer1Timer(Sender: TObject);
begin
  UpdateStatusBar(now)
end;

procedure TfrmClipBoard.UpdateStatusBar(KTime: TDateTime);
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
      StsBrClipBoard.Panels.Items[0].Text := FormatDateTime('hh:nn:ss', KTime)
    else
      StsBrClipBoard.Panels.Items[0].Text := FormatDateTime('hh:nn:ss am/pm', KTime);

    StsBrClipBoard.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', KTime);
    StsBrClipBoard.Panels.Items[2].Text := keyResult;

    if userOptions.displayIdleTime then
      StsBrClipBoard.Panels.Items[3].Text := 'Idle Time :: ' + FormatDateTime('hh:nn:ss', tick / SecsPerDay)
    else
      StsBrClipBoard.Panels.Items[3].Text := '';
end;

procedure TfrmClipBoard.ClipboardChanged(Sender: TObject);
{  Called by the Clipboard Listener.
   The contents of the clipboard are added to the listview, if not already present.
}
var
  newItem: TListItem;
  bitmap: TBitmap;
  dirName: string;

begin
  if LstVwClipBoard.Focused then exit;

  if not frmClipBoard.Visible then frmClipBoard.Visible := True;

  if FListener.category = 'Image' then
  begin
    dirname  := GetTempFileName(GetAppConfigDir(False), 'Klock');
    FListener.text := dirname;
    FListener.image.SaveToFile(dirname);
    bitmap := TBitmap.Create;
    bitmap := FListener.image;
    bitmap.Canvas.StretchDraw(rect(0, 0, ImgClipBoard.Width, ImgClipBoard.Height), bitmap);
    bitmap.SetSize(ImgClipBoard.Width, ImgClipBoard.Height);
    ImgClipBoard.Picture.Bitmap := bitmap;
    bitmap.Free;
  end;
  if not isthere(FListener.text) then
    begin
      newItem := LstVwClipBoard.Items.Add;
      newItem.Caption := FListener.category;
      newItem.SubItems.add(FListener.epoch);
      newItem.SubItems.add(FListener.text);
    end;
end;

procedure TfrmClipBoard.cullTmpFiles;
{  Will delete [to recycle bin] all log files older then a 10 days.  }
var
  tmpFiles:TStringlist;
  tmpFile: string;
  tmpDate:TDateTime;
  tmpAge:longInt;
begin
  tmpFiles := TstringList.Create;
  FindAllFiles(tmpFiles, GetAppConfigDir(False), '*.tmp', True);

  for tmpFile in tmpFiles do
    begin
      tmpDate := FileDateTodateTime(FileAgeUTF8(tmpFile));
      tmpAge := DaysBetween(Now, tmpDate);

      if tmpAge > 10 then
      begin
        deleteToRycycle(tmpFile);
        klog.writeLog(format('Deleting %S with age %D  %S', [tmpFile, tmpAge, FormatDateTime('DD MMM YYYY', tmpDate)]));
      end;
    end;

  freeandnil(tmpFiles);
end;

procedure TfrmClipBoard.deleteToRycycle(aFile: string);
{  Deletes files to the Rycycle bin.
    Thanks to Lush - http://forum.lazarus-ide.org/index.php?topic=12288.0

    FOF_ALLOWUNDO -> moves file to the bin.
    FOF_SILENT -> deletes the file permanently.
    Add FOF_NOCONFIRMATION to any of the previous constants to disable the "are you sure" dialog box.

    NB : Seems to ignore directories at the moment.
}

var
  fileOpStruct: TSHFileOpStruct;

begin
  with fileOpStruct do
  begin
    Wnd := frmMain.Handle;      //  Hmm, seems to need a handle of the main form.
    wFunc := FO_DELETE;
    pFrom := PChar(aFile + #0#0);
    pTo := nil;
    hNameMappings := nil;

    fFlags := FOF_ALLOWUNDO;                    //  Use recycle bin.
    fFlags := fFlags or FOF_NOCONFIRMATION;
  end;

  try
    SHFileOperation(fileOpStruct);
  except
    on E: Exception do
      klog.writeLog(E.Message);
  end;

end;

function TfrmClipBoard.isThere(s: string): boolean;
{  Returns true if the string s already exists in the listview.
   Could not see a easy way to do this, so the listview is iterated
   over and every item is checked in turn.

   This not only stops duplicate entries, but also stops the annoyance of
   the entry being entered again when the lixtview is clicked.
}
var
  Item: TListItem;
  f: integer;
  max: integer;
  found: boolean = false;
begin
  max := LstVwClipBoard.Items.Count;
  if max <> 0 then
  begin
    for f := 0 to max-1 do     //  zero based.
    begin
      item := LstVwClipBoard.Items.Item[f];
      if item.SubItems.Strings[1] = s then
        found := True;
    end;
  end;

  result := found;
end;

end.

