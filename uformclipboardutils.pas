unit UformClipBoardUtils;

{  Code based on submission by ASerge in
   http://forum.lazarus.freepascal.org/index.php?topic=15488.0
}


{$mode objfpc}{$H+}

interface

uses
  SysUtils, Windows, LCLIntf, Classes, Clipbrd, dialogs, LazFileUtils, StdCtrls, ComCtrls,
  shlobj, LCLType, Graphics, Controls;

type
  { TClipboardListener }

  TClipboardListener = class(TObject)
  strict private
    FOnClipboardChange: TNotifyEvent;
    FWnd: HWND;

    _text    : string;                   //  the data on the clipboard.
    _category: string;                   //  The category of the data [text, file, dir].
    _epoch   : string;                   //  The date added.
    _image   : TBitmap;

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
    procedure loadCSV;
    procedure saveCSV(data: TListItems);

    property OnClipboardChange: TNotifyEvent read FOnClipboardChange write FOnClipboardChange;
    property text             : string       read _text              write _text;
    property category         : string       read _category          write _category;
    property epoch            : string       read _epoch             write _epoch;
    property image            : TBitmap      read _image             write _image;
    class property Supported: Boolean read GetSupported;
  end;

implementation

uses
  formklock, formClipBoard;

CONST
  LF    = '^*';
  CR    = '^%';
  COMMA = '^&';

var
  AddClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;
  RemoveClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;

procedure InitClipboardFormatListener;
var
  HUser32: HMODULE;
begin
  HUser32 := GetModuleHandle(user32);
  Pointer(AddClipboardFormatListener)    := GetProcAddress(HUser32, 'AddClipboardFormatListener');
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

    if clipboard.HasFormat(CF_TEXT) then               // Text
    begin
      getTextData;
    end
    else if clipboard.HasFormat(CF_HDROP) then              //  Filename or directory
    begin
      getFileData;
    end
    else if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
    begin
      getPicData;
    end
    else
    begin
      getTextData;
    end;

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
  bitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
  image    := bitmap;
  category := 'Image';
  text     := '';

  //  This causes an Project lazKlock raised exception class 'External: SIGSEGV' ERROR.
  //  Not sure why at the moment.
  //  bitmap.Free;
end;

procedure TClipboardListener.getTextData;
{  Sets the text values.    }
begin
  text     := clipboard.AsText;
  category := 'Text';
end;

procedure TClipboardListener.getFileData;
{  Retrieves the actual filename, using pointer black magic.    }
var
  hDropHandle: HDROP;
  iCount     : Integer;
  iIndex     : Integer;
  iLength    : Integer;
  szBuffer   : PChar;
  filename   : string;
begin
  OpenClipboard(0);                                                          // lock clipboard
  hDropHandle := GetClipboardData(CF_HDROP);                                 // get drop handle from the clipboard
  if hDropHandle = 0 then
    showMessage('Zero');
  iCount := DragQueryFile(hDropHandle, $FFFFFFFF, nil, 0);
  for iIndex := 0 to iCount - 1 do
  begin
    iLength  := DragQueryFile(hDropHandle, iIndex, nil, 0);                  // get length of filename
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
  epoch    := Item.SubItems.Strings[0];
  text     := Item.SubItems.Strings[1];

  if category = 'Text' then clipboard.AsText := text;

  if category = 'File' then if FileExists(text) then CopyFileToClipboard(text) ;

  if category = 'Dir' then if DirectoryExists(text) then CopyFileToClipboard(text);

  if category = 'Image' then if FileExists(text) then CopyImageToClipboard(text);

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
  iLen     := Length(FileList) + 2;
  FileList := FileList + #0#0;
  hGlobal  := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, SizeOf(TDropFiles) + iLen);

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
//
//------------------------------------------- CSV stuff ------------------------
//
procedure TClipboardListener.loadCSV;
{  Populates the clipboard listview form a csv file.

   The three fields of the csv file are extracted using string splicing.
   I know this is a bit klunky, but theirs only three and will be only three.

   A extension of .clip is suggested, but can be changed.

   TODO : this procedure directly talks to the listview, maybe should not.
}
VAR
  newItem: TListItem;
  csvFile: TextFile;
  csvLine: string;
  itmCat : string;
  itmDate: string;
  itmData: string;
  itmPos : integer;
begin
  with TOpenDialog.Create(frmClipBoard) do
  begin
    filter     := '*.clip';
    InitialDir := GetAppConfigDir(False);
    Title      := 'Choose a Clipboard file to Load';
    if Execute then
    begin
      AssignFile(csvFile, fileName.ToLower);
      try
        // Open the file for reading
        reset(csvFile);

        frmClipBoard.LstVwClipBoard.Items.Clear;

        // Keep reading lines until the end of the file is reached
        while not eof(csvFile) do
        begin
          readln(csvFile, csvLine);

          itmpos := Pos(',', csvLine);
          itmCat := copy(csvLine, 0, itmpos - 1);
          Delete(csvLine, 1, itmpos);

          itmpos := Pos(',', csvLine);
          itmDate := copy(csvLine, 0, itmpos - 1);
          Delete(csvLine, 1, itmpos);

          itmData := csvLine;

          //  Return the data back to original.
          //  All special markers for linefeed, carriage return and comma
          //  are replaced with the original meaning.
          itmData := StringReplace(itmData, LF   , #10, [rfReplaceAll]);
          itmData := StringReplace(itmData, CR   , #13, [rfReplaceAll]);
          itmData := StringReplace(itmData, COMMA, ',', [rfReplaceAll]);

          newItem         := frmClipBoard.LstVwClipBoard.Items.Add;;
          newItem.Caption := itmCat;
          newItem.SubItems.add(itmDate);
          newItem.SubItems.add(itmData);

        end;  //  while not eof(csvFile) do
      finally
        CloseFile(csvFile);
      end;  //  try
    end;    //  if Execute then
  end;      //  with TOpenDialog.Create(frmClipBoard) do
end;

procedure TClipboardListener.saveCSV(data: TListItems);
{  Saves the contents of the clipboard viewer to a file.
   The data is save as a csv file.

   A extension of .clip is suggested, but can be changed.
}
VAR
  itemCount: integer;
  itemData : string;
  cleanData: string;
  csvFile  : TextFile;
  f        : integer;
begin
  with TSaveDialog.Create(frmClipBoard) do
  begin
    filter     := '*.clip';
    InitialDir := GetAppConfigDir(False);
    Title      := 'Choose a Clipboard file To save to';
    if Execute then
    begin
      AssignFile(csvFile, fileName.ToLower);
      try
        rewrite(csvFile);
        itemCount := data.Count - 1;

        for f := 0 to itemCount do
        begin
          cleanData := data.Item[f].SubItems.Strings[1];

          //  replace each linefeed, carriage return and comma with
          //  a special marker, so the data is not save on multiple
          //  lines and extra commas will upset the load procedure.
          cleanData := StringReplace(cleanData, #10, LF   , [rfReplaceAll]);
          cleanData := StringReplace(cleanData, #13, CR   , [rfReplaceAll]);
          cleanData := StringReplace(cleanData, ',', COMMA, [rfReplaceAll]);

          itemData := format('%s,%s,%s', [data.Item[f].Caption,
                                          data.Item[f].SubItems.Strings[0],
                                          cleanData]);

          writeLn(csvFile, itemData);
        end;  //  for f := 0 to itemCount do
      finally
        CloseFile(csvFile);
      end;    //  try
    end;      //  if Execute then
  end;        //  with TSaveDialog.Create(frmClipBoard) do
end;
//
//
//
initialization
  InitClipboardFormatListener;
end.
