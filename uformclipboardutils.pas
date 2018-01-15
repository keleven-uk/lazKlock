unit UformClipBoardUtils;

{  Code based on submission by ASerge in
   http://forum.lazarus.freepascal.org/index.php?topic=15488.0
}


{$mode objfpc}{$H+}

interface

uses
  SysUtils, Windows, LCLIntf, Classes, Clipbrd, dialogs, LazFileUtils, StdCtrls, ComCtrls,
  shlobj, LCLType, Graphics;

type
  { TClipboardListener }

  TClipboardListener = class(TObject)
  strict private
    FOnClipboardChange: TNotifyEvent;
    FWnd: HWND;

    _text: string;                   //  the data on the clipboard.
    _category: string;               //  The category of the data [text, file, dir].
    _epoch: string;                  //  The date added.
    _image: TBitmap;

    class function GetSupported: Boolean; static;
    procedure WindowProc(var Msg: TMessage);
    procedure setFileameData(fn: string);
    procedure getFileData;
    procedure getTextData;
    procedure getPicData;

  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyToClipboard(Item: TListItem);
    procedure CopyFileToClipboard(FileList: string);
    procedure CopyImageToClipboard(FileList: string);

    property OnClipboardChange: TNotifyEvent read FOnClipboardChange write FOnClipboardChange;
    property text: string read _text write _text;
    property category: string read _category write _category;
    property epoch: string read _epoch write _epoch;
    property image: TBitmap read _image write _image;

    class property Supported: Boolean read GetSupported;
  end;

implementation

uses
  formklock, formClipBoard;

var
  AddClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;
  RemoveClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;

procedure InitClipboardFormatListener;
var
  HUser32: HMODULE;
begin
  HUser32 := GetModuleHandle(user32);
  Pointer(AddClipboardFormatListener) := GetProcAddress(HUser32, 'AddClipboardFormatListener');
  Pointer(RemoveClipboardFormatListener) := GetProcAddress(HUser32, 'RemoveClipboardFormatListener');
end;

{ TClipboardListener }

constructor TClipboardListener.Create;
begin
  inherited;
  if GetSupported then
  begin
    FWnd := LCLIntf.AllocateHWnd(@WindowProc);
    if not AddClipboardFormatListener(FWnd) then
      RaiseLastOSError;
  end;
end;

destructor TClipboardListener.Destroy;
begin
  if FWnd <> 0 then
  begin
    RemoveClipboardFormatListener(FWnd);
    LCLIntf.DeallocateHWnd(FWnd);
  end;
  inherited;
end;

class function TClipboardListener.GetSupported: Boolean;
begin
  Result := Assigned(AddClipboardFormatListener) and
    Assigned(RemoveClipboardFormatListener);
end;

procedure TClipboardListener.WindowProc(var Msg: TMessage);
{  Called when the clipboard changes.    }
begin
  if (Msg.msg = WM_CLIPBOARDUPDATE) and Assigned(FOnClipboardChange) then
  begin

    if clipboard.HasFormat(CF_TEXT) then                                         // Text
      getTextData;

    if clipboard.HasFormat(CF_HDROP) then                                        //  Filename or directory
      getFileData;

    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
      getPicData;

    epoch := FormatDateTime('DD MMM YYYY hh:nn:ss', now);

    Msg.Result := 0;
    FOnClipboardChange(Self);
  end;
end;

procedure TClipboardListener.getPicData;
{  Sets the image values.}
var
  bitmap: TBitmap;
begin
  bitmap := TBitmap.Create;
  Bitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
  image := bitmap;
  category := 'Image';

  //  This causes an Project lazKlock raised exception class 'External: SIGSEGV' ERROR.
  //  Not sure why at the moment.
  //  bitmap.Free;
end;

procedure TClipboardListener.getTextData;
{  Sets the text values.    }
begin
  text := clipboard.AsText;
  category := 'Text';
end;

procedure TClipboardListener.getFileData;
{  Retrieves the actual filename, using pointer black magic.    }
var
  hDropHandle: HDROP;
  iCount, iIndex: Integer;
  iLength: Integer;
  szBuffer: PChar;
  filename: string;
begin
  OpenClipboard(0);                                                          // lock clipboard
  hDropHandle := GetClipboardData(CF_HDROP);                                 // get drop handle from the clipboard
  if hDropHandle = 0 then
    showMessage('Zero');
  iCount := DragQueryFile(hDropHandle, $FFFFFFFF, nil, 0);
  for iIndex := 0 to iCount - 1 do
  begin
    iLength := DragQueryFile(hDropHandle, iIndex, nil, 0);                   // get length of filename
    szBuffer := StrAlloc(iLength + 1);                                       // allocate the memory, the #0 is not included in "iLength"
    try
      if DragQueryFile(hDropHandle, iIndex, szBuffer, iLength + 1) > 0 then  // get filename
        filename := szBuffer
    finally
      StrDispose(szBuffer);                                                  // free the memory
    end;  //  try
  end;  //  for iIndex := 0 to iCount - 1 do
  CloseClipboard;
  setFileameData(filename);
end;  //  if clipboard.HasFormat(CF_HDROP) then

procedure TClipboardListener.setFileameData(fn: string);
{  Determines if the filename is a file or directory.    }
begin
  if FileExists(fn) then
    category := 'File'
  else if DirectoryExists(fn) then
    category := 'Dir'
  else
    category := 'Unkown';

  text := fn;
end;

procedure TClipboardListener.CopyToClipboard(Item: TListItem);
{  Copies data back to the clipboard.
   Error should be trapped but not logged.}
begin
  category := Item.Caption;
  epoch := Item.SubItems.Strings[0];
  text := Item.SubItems.Strings[1];

  if category = 'Text' then
    clipboard.AsText := text;

  if category = 'File' then
    if FileExists(text) then CopyFileToClipboard(text) ;

  if category = 'Dir' then
    if DirectoryExists(text) then CopyFileToClipboard(text);

  if category = 'Image' then
    if FileExists(text) then CopyImageToClipboard(text);

  EmptyClipboard;
end;

procedure TClipboardListener.CopyImageToClipboard(FileList: string);
var
  bitmap: TBitmap;
begin
  bitmap := TBitmap.Create;
  bitmap.LoadFromFile(FileList);
  clipboard.Assign(bitmap);
  bitmap.Free;
end;

procedure TClipboardListener.CopyFileToClipboard(FileList: string);
{  Copies data back to the clipboard, if a filename.    }
var
  DropFiles: PDropFiles;
  hGlobal: THandle;
  iLen: integer;
begin
  iLen := Length(FileList) + 2;
  FileList := FileList + #0#0;
  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, SizeOf(TDropFiles) + iLen);
  if (hGlobal = 0) then
    raise Exception.Create('Could not allocate memory.');
  begin
    DropFiles := GlobalLock(hGlobal);
    DropFiles^.pFiles := SizeOf(TDropFiles);
    Move(FileList[1], (PChar(DropFiles) + SizeOf(TDropFiles))^, iLen);
    GlobalUnlock(hGlobal);
    OpenClipboard(frmClipBoard.Handle);
    EmptyClipboard;
    SetClipboardData(CF_HDROP,hGlobal);
    CloseClipboard;
   end;
end;


initialization
  InitClipboardFormatListener;
end.
