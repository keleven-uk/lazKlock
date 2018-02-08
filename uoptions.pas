unit uOptions;

{  Holds the current user options.  The options are hard coded in this class.

   When the class is created, the constructor first checks that the user directory
   exists - this should be something like c:\Users\<user>\AppData\Local\<app Name>\Options.xml.
   The file name can be passed to the constructor, if it's absent the 'Options.xml' is used.
   If this directory does not exist, it will be created.

   Next, the user options file is checked.  If it is exists, it is read.
   If the file does not exist - the file will be created with default values.

   class methods -
       create                       - creates the options class with a default filename.
       create(fileName: string)     - creates the options class with a specified filename.
       readOptions                  - reads the options file and populates the options class
       writeCurrentOptions          - writes the current options to the options filename [in XML].
       writeDefaultOptions          - writes the options file with default files [internal use really]
                                    - could be used to reset options.

   The writeCurrentOptions should be executed when the program closes, or when options change.
   That is if the changes need to be saved.

   NOTE :: If there is an error while either reading or writing the options file, the application is halted.

   NOTE :: All XML values are string, so need to be casted before use - this is done in the read / write routines.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, fileinfo, winpeimagereader, Dialogs,
  Graphics, UKlockUtils;

type

  { Options }

  Options = class

  private
    _dirName: string;
    //  Global
    _Comments: string;
    _companyName: string;
    _fileDescription: string;
    _fileVersion: string;
    _InternalName: string;
    _legalCopyright: string;
    _originalFileName: string;
    _productName: string;
    _productVersion: string;

    _optionsName: string;           //  full path to the options file.
    _memoName: string;              //  full path to the memo file.
    _stickyName: string;            //  full path to the Sticky Notes file.
    _unitsName: string;             //  full path to the Units file.
    _reminderName: string;          //  full path to the Reminder file.
    _runAtStartUp: boolean;         //  run Klock at windows start up - Current user only.
    _screenSave: boolean;           //  do we save Klock position or not.
    _formTop: integer;              //  the forms top left.
    _formLeft: integer;
    _defaultTab: integer;
    _volume:String;
    _monitorClipboard: boolean;     //  Moinitor Clipboard i.e. Klock captures all clipboard activities.
    _CB_ScreenSave: boolean;        //  do we save clipboard manager position or not.
    _CB_formTop: integer;           //  the clipboard manager top left.
    _CB_formLeft: integer;

    //  Time
    _defaultTime: integer;
    _netTimeSeconds: boolean;
    _swatchCentibeats: boolean;
    _fuzzyTimeBalloon: boolean;
    _displayIdleTime: boolean;
    _display24Hour: boolean;        //  Disply time has 24 hour if true, else 12 hour.
    _hourPips: boolean;
    _hourChimes: boolean;
    _halfChimes: boolean;
    _quarterChimes: boolean;
    _threeQuarterChimes: boolean;
    _christmasFont: boolean;
    _easterFont: boolean;
    _valentinesFont: boolean;
    _haloweenFont: boolean;

    //  Timer
    _timerMilliSeconds: boolean;

    //  Analogue Klock
    _analogueScreenSave: boolean;           //  do we save from position or not.
    _analogueFormTop: integer;              //  the forms top left.
    _analogueFormLeft: integer;

    // LED Klock
    _LEDScreenSave: boolean;           //  do we save from position or not.
    _LEDFormTop: integer;              //  the forms top left.
    _LEDFormLeft: integer;
    _LEDlongDate: boolean;

    // Binary Klock
    _BinaryScreenSave: boolean;           //  do we save from position or not.
    _BinaryFormTop: integer;              //  the forms top left.
    _BinaryFormLeft: integer;
    _BinaryFormat: boolean;               //  Binary or BCD format - true for binary.

    // Small Text Klock
    _smallTextScreenSave: boolean;        //  do we save from position or not.
    _smallTextFormTop: integer;           //  the forms top left.
    _smallTextFormLeft: integer;
    _smallTextTransparent: boolean;       //  is Small Text Klock transparent?

    // memos
    _useDefaultpassWord: boolean;         //  Memo allowed to use default pass word.
    _defaultpassWord: string;             //  Memo default pass word.
    _decryptTimeOut: integer;             //  Memo decrypt Time Out.

    //Sticky Notes
    _stickyColor: TColor;                   //  Sticky Note colour.
    _stickyFont:TFont;                      //  Sticky Note Font.

    //  Logging
    _logging: Boolean;
    _cullLogs: Boolean;
    _CullLogsDays: integer;

    procedure checkDirectory;
    Function readChild(PassNode: TDOMNode;  name: string): string;
    Function readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
    function writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
    function writeFontChild(Doc: TXMLDocument; name: string; value: Tfont): TDOMNode;
    function writeColChild(Doc: TXMLDocument; name: string; value: TColor): TDOMNode;
    function writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
    function writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
    function writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;

  public
    //  Global - file stuff
    property Comments: string read _Comments write _Comments;
    property companyName: string read _companyName write _companyName;
    property fileDescription: string read _fileDescription write _fileDescription;
    property fileVersion: string read _fileVersion write _fileVersion;
    property InternalName: string read _InternalName write _InternalName;
    property legalCopyright: string read _legalCopyright write _legalCopyright;
    property originalFileName: string read _originalFileName write _originalFileName;
    property productName: string read _productName write _productName;
    property productVersion: string read _productVersion write _productVersion;

    //  Global - other stuff
    property optionsName: string read _optionsName write _optionsName;
    property memoName: string read _memoName write _memoName;
    property stickyName: string read _stickyName write _stickyName;
    property unitsName: string read _unitsName write _unitsName;
    property reminderName: string read _reminderName write _reminderName;
    property runAtStartUp: boolean read _runAtStartUp write _runAtStartUp;
    property screenSave: boolean read _screenSave write _screenSave;
    property formTop: integer read _formTop write _formTop;
    property formLeft: integer read _formLeft write _formLeft;
    property defaultTab: integer read _defaultTab write _defaultTab;
    property volume: string read _volume write _volume;
    property monitorClipboard: boolean read _monitorClipboard write _monitorClipboard;
    property CB_ScreenSave: boolean read _CB_ScreenSave write _CB_ScreenSave;
    property CB_formTop: integer read _CB_formTop write _CB_formTop;
    property CB_formLeft: integer read _CB_formLeft write _CB_formLeft;

    //  Time
    property defaultTime: integer read _defaultTime write _defaultTime;
    property netTimeSeconds: boolean read _netTimeSeconds write _netTimeSeconds;
    property swatchCentibeats: boolean read _swatchCentibeats write _swatchCentibeats;
    property fuzzyTimeBalloon: boolean read _fuzzyTimeBalloon write _fuzzyTimeBalloon;
    property displayIdleTime: boolean read _displayIdleTime write _displayIdleTime;
    property display24Hour: boolean read _display24Hour write _display24Hour;
    property hourPips: boolean read _hourPips write _hourPips;
    property hourChimes: boolean read _hourChimes write _hourChimes;
    property halfChimes: boolean read _halfChimes write _halfChimes;
    property quarterChimes: boolean read _quarterChimes write _quarterChimes;
    property threeQuarterChimes: boolean read _threeQuarterChimes write _threeQuarterChimes;
    property christmasFont: boolean read _christmasFont write _christmasFont;
    property easterFont: boolean read _easterFont write _easterFont;
    property valentinesFont: boolean read _valentinesFont write _valentinesFont;
    property haloweenFont: boolean read _haloweenFont write _haloweenFont;

    //  Timer
    property timerMilliSeconds: boolean read _timerMilliSeconds write _timerMilliSeconds;

    // Analogue Kock
    property analogueScreenSave: boolean read _analogueScreenSave write _analogueScreenSave;
    property analogueFormTop: integer read _analogueFormTop write _analogueFormTop;
    property analogueFormLeft: integer read _analogueFormLeft write _analogueFormLeft;

    // LED Kock
    property LEDScreenSave: boolean read _LEDScreenSave write _LEDScreenSave;
    property LEDFormTop: integer read _LEDFormTop write _LEDFormTop;
    property LEDFormLeft: integer read _LEDFormLeft write _LEDFormLeft;
    property LEDlongDate: boolean read _LEDlongDate write _LEDlongDate;

    // Binary Kock
    property BinaryScreenSave: boolean read _BinaryScreenSave write _BinaryScreenSave;
    property BinaryFormTop: integer read _BinaryFormTop write _BinaryFormTop;
    property BinaryFormLeft: integer read _BinaryFormLeft write _BinaryFormLeft;
    property BinaryFormat: boolean read _BinaryFormat write _BinaryFormat;

    // Small Text Klock
    property smallTextScreenSave: boolean read _smallTextScreenSave write _smallTextScreenSave;
    property smallTextFormTop: integer read _smallTextFormTop write _smallTextFormTop;
    property smallTextFormLeft: integer read _smallTextFormLeft write _smallTextFormLeft;
    property smallTextTransparent: boolean read _smallTextTransparent write _smallTextTransparent;

    // Memos
    property useDefaultpassWord: boolean read _useDefaultpassWord write _useDefaultpassWord;
    property defaultpassWord: string read _defaultpassWord write _defaultpassWord;
    property decryptTimeOut: integer read _decryptTimeOut write _decryptTimeOut;

    //Sticky Notes
    property stickyColor: TColor read _stickyColor write _stickyColor;
    property stickyFont: TFont read _stickyFont write _stickyFont;

    //  Logging
    property logging: boolean read _logging write _logging;
    property cullLogs: boolean read _cullLogs write _cullLogs;
    property CullLogsDays: integer read _CullLogsDays write _CullLogsDays;

    constructor Create; overload;
    constructor Create(filename: string); overload;

    procedure readOptions;
    procedure writeCurrentOptions;
    procedure writeDefaultOptions;
    procedure Assign(o: Options);

  end;  //  class

  { myFileVersionInfo }

  myFileVersionInfo = class
  {  Retrieves the current file info.

     see http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company.

     NOTE :: The information in Project / Project Options / Version Ino in the IDE
     must be up to date.
  }

  private
    _comments: string;
    _companyName: string;
    _fileDescription: string;
    _fileVersion: string;
    _InternalName: string;
    _legalCopyright: string;
    _originalFileName: string;
    _productName: string;
    _productVersion: string;

  public
    property fileComments: string read _comments write _comments;
    property fileCompanyName: string read _companyName write _companyName;
    property fileFileDescription: string read _fileDescription write _fileDescription;
    property fileFileVersion: string read _fileVersion write _fileVersion;
    property fileInternalName: string read _InternalName write _InternalName;
    property fileLegalCopyright: string read _legalCopyright write _legalCopyright;
    property fileOriginalFileName: string read _originalFileName write _originalFileName;
    property fileProductName: string read _productName write _productName;
    property fileProductVersion: string read _productVersion write _productVersion;

    procedure GetFileInfo;
  end;

implementation

//
//............................................ Options methods ............................................
//
constructor Options.Create; overload;
  {  creates the options class with a default filename.  }
var
  optnFile: String;
begin
  checkDirectory;

  {$IFDEF TEST}
    optnFile := 'TEST_Options';
  {$else}
    optnFile := 'Options';
  {$endif}
  {$ifdef WIN32}
    optionsName := _dirName + optnFile + '32.xml';
  {$else}
    optionsName := _dirName + optnFile + '64.xml';
  {$endif}

  if FileExists(optionsName) then
    begin
      writeDefaultOptions;       //  Set up the defaults [which includes any new values]
      readOptions;               //  read the exiting options file.
      writeCurrentOptions;       //  write out a new options file [which includes any new values]
    end
  else
    writeDefaultOptions;         //  options file not found, write out a new one.
end;

constructor Options.Create(filename: string);
{  creates the options class with a specified filename.  }
begin
  checkDirectory;

  optionsName := _dirName + fileName;

  if FileExists(optionsName) then
    begin
      writeDefaultOptions;       //  Set up the defaults [which includes any new values]
      readOptions;               //  read the exiting options file.
      writeCurrentOptions;       //  write out a new options file [which includes any new values]
    end
  else
    writeDefaultOptions;         //  options file not found, write out a new one.
end;

procedure Options.checkDirectory;
{  Checks that the options directory exists.

   GetAppConfigDir(False) -> c:\Users\<user>\AppData\Local\<app Name>\
   GetAppConfigDir(True)  -> c:\ProgramData\<app Name>\
}
begin
  _dirName := GetAppConfigDir(False);

  if not DirectoryExists(_dirName) then
    if not CreateDir(_dirName) then
      ShowMessage('Failed to create directory !');
end;

procedure Options.Assign(o: Options);
{  Copy all fields from one options object to another.
   Because Options is derived from TObjects and not TPersistent, we don't get assign for free.

   NOTE :: When a new field of added to the Option class, it HAS to be added here.
           Must be a better way of doing this.
}
begin
  //  Global - file stuff
  Comments := o.Comments;
  companyName := o.companyName;
  fileDescription := o.fileDescription;
  fileVersion := o.fileVersion;
  InternalName := o.InternalName;
  legalCopyright := o.legalCopyright;
  originalFileName := o.originalFileName;
  productName := o.productName;
  productVersion := o.productVersion;

  //  Global - other stuff
  optionsName := o.optionsName;
  memoName := o.memoName;
  stickyName := o.stickyName;
  unitsName := o.unitsName;
  reminderName := o.reminderName;
  runAtStartUp := o.runAtStartUp;
  screenSave := o.screenSave;
  formTop := o.formTop;
  formLeft := o.formLeft;
  defaultTab := o.defaultTab;
  volume := o.volume;
  monitorClipboard := o.monitorClipboard;
  CB_screenSave := o.CB_screenSave;
  CB_formTop := o.CB_formTop;
  CB_formLeft := o.CB_formLeft;

  //  Time
  defaultTime := o.defaultTime;
  netTimeSeconds := o.netTimeSeconds;
  swatchCentibeats := o.swatchCentibeats;
  fuzzyTimeBalloon := o.fuzzyTimeBalloon;
  displayIdleTime := o.displayIdleTime;
  display24Hour := o.display24Hour;
  hourPips := o.hourPips;
  hourChimes := o.hourChimes;
  halfChimes := o.halfChimes;
  quarterChimes := o.quarterChimes;
  threeQuarterChimes := o.threeQuarterChimes;
  christmasFont := o.christmasFont;
  easterFont := o.easterFont;
  valentinesFont := o.valentinesFont;
  haloweenFont := o.haloweenFont;

  //  Timer
  timerMilliSeconds := o.timerMilliSeconds;

  //  Analogue Klock
  analogueScreenSave := o.analogueScreenSave;
  analogueFormTop := o.analogueFormTop;
  analogueFormLeft := o.analogueFormLeft;

  //  LED Klock
  LEDScreenSave := o.LEDScreenSave;
  LEDFormTop := o.LEDFormTop;
  LEDFormLeft := o.LEDFormLeft;
  LEDlongDate := o.LEDlongDate;

  //  Binary Klock
  BinaryScreenSave := o.BinaryScreenSave;
  BinaryFormTop := o.BinaryFormTop;
  BinaryFormLeft := o.BinaryFormLeft;
  BinaryFormat := o.BinaryFormat;

  // Small Text Klock
  smallTextScreenSave := o.smallTextScreenSave;
  smallTextFormTop := o.smallTextFormTop;
  smallTextFormLeft := o.smallTextFormLeft;
  smallTextTransparent := o.smallTextTransparent;

  // memos
  useDefaultpassWord := o.useDefaultpassWord;
  defaultpassWord := o.defaultpassWord;
  decryptTimeOut := o.decryptTimeOut;

  //Sticky Notes
  stickyColor := o.stickyColor;
  stickyFont := o.stickyFont;

  //  Logging
  logging := o.logging;
  cullLogs := o.cullLogs;
  CullLogsDays := o.CullLogsDays;
end;

procedure Options.readOptions;
{  Read in the options file.
   The filename is specified when the user options class is created.
   The file info is re-read, in case is has changed.

   The read is now a two stage process.
   Stage 1 - the xml node is read, this return a string value.
   stage 2 - if the return is 'ERROR', the xml node is missing and then use the
             default value.  If not the return will hold the value which is passed
             to the options property.

   NOTE : This cures the missing child problem, BUT NOT the missing node.
}
var
  fvi: myFileVersionInfo;
  PassNode: TDOMNode;
  Doc: TXMLDocument;
  rtn: string;
begin
  try
    //  retrieve file info i.e build numner etc.
    fvi := myFileVersionInfo.Create;
    fvi.GetFileInfo;

    Comments := fvi.fileComments;
    companyName := fvi.fileCompanyName;
    fileDescription := fvi.fileFileDescription;
    fileVersion := fvi.fileFileVersion;
    InternalName := fvi.fileInternalName;
    legalCopyright := fvi.fileLegalCopyright;
    originalFileName := fvi.fileOriginalFileName;
    productName := fvi.fileProductName;
    productVersion := fvi.fileProductVersion;

    try
      // Read in xml file from disk
      ReadXMLFile(Doc, optionsName);
    except
      on E: Exception do
      begin
        ShowMessage('ERROR: reading XML file.' + LineEnding + E.Message + LineEnding +
          'Halting Program Execution');
        Halt;
      end;  //  on E:
    end;    //  try

    //  Global
    PassNode := Doc.DocumentElement.FindNode('Global');

    if assigned(PassNode) then
    begin
      rtn := readChildAttribute(PassNode, 'formPosition', 'Top');
      if rtn <> 'ERROR' then formTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'formPosition', 'Left');
      if rtn <> 'ERROR' then formLeft := StrToInt(rtn);

      rtn := readChild(PassNode, 'optionsName');
      if rtn <> 'ERROR' then optionsName := ansistring(rtn);
      rtn := readChild(PassNode, 'memoName');
      if rtn <> 'ERROR' then memoName := ansistring(rtn);
      rtn := readChild(PassNode, 'stickyName');
      if rtn <> 'ERROR' then stickyName := ansistring(rtn);
      rtn := readChild(PassNode, 'unitsName');
      if rtn <> 'ERROR' then unitsName := ansistring(rtn);
      rtn := readChild(PassNode, 'reminderName');
      if rtn <> 'ERROR' then reminderName := ansistring(rtn);
      rtn := readChild(PassNode, 'runAtStartUp');
      if rtn <> 'ERROR' then runAtStartUp := StrToBool(rtn);
      rtn := readChild(PassNode, 'screenSave');
      if rtn <> 'ERROR' then screenSave := StrToBool(rtn);
      rtn := readChild(PassNode, 'defaultTab');
      if rtn <> 'ERROR' then defaultTab := StrToInt(rtn);
      rtn := readChild(PassNode, 'volume');
      if rtn <> 'ERROR' then volume := ansistring(rtn);
      rtn := readChild(PassNode, 'monitorClipboard');
      if rtn <> 'ERROR' then monitorClipboard := StrToBool(rtn);
      rtn := readChild(PassNode, 'CB_screenSave');
      if rtn <> 'ERROR' then CB_screenSave := StrToBool(rtn);
      rtn := readChildAttribute(PassNode, 'CB_formPosition', 'Top');
      if rtn <> 'ERROR' then CB_formTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'CB_formPosition', 'Left');
      if rtn <> 'ERROR' then CB_formLeft := StrToInt(rtn);
    end;

    //  Time
    PassNode := Doc.DocumentElement.FindNode('Time');

    if assigned(PassNode) then
    begin
      rtn := readChild(PassNode, 'defaultTime');
      if rtn <> 'ERROR' then defaultTime := StrToInt(rtn);
      rtn := readChild(PassNode, 'netTimeSeconds');
      if rtn <> 'ERROR' then netTimeSeconds := StrToBool(rtn);
      rtn := readChild(PassNode, 'swatchCentibeats');
      if rtn <> 'ERROR' then swatchCentibeats := StrToBool(rtn);
      rtn := readChild(PassNode, 'fuzzyTimeBalloon');
      if rtn <> 'ERROR' then fuzzyTimeBalloon := StrToBool(rtn);
      rtn := readChild(PassNode, 'displayIdleTime');
      if rtn <> 'ERROR' then displayIdleTime := StrToBool(rtn);
      rtn := readChild(PassNode, 'display24Hour');
      if rtn <> 'ERROR' then display24Hour := StrToBool(rtn);
      rtn := readChild(PassNode, 'hourPips');
      if rtn <> 'ERROR' then hourPips := StrToBool(rtn);
      rtn := readChild(PassNode, 'hourChimes');
      if rtn <> 'ERROR' then hourChimes := StrToBool(rtn);
      rtn := readChild(PassNode, 'halfChimes');
      if rtn <> 'ERROR' then halfChimes := StrToBool(rtn);
      rtn := readChild(PassNode, 'quarterChimes');
      if rtn <> 'ERROR' then quarterChimes := StrToBool(rtn);
      rtn := readChild(PassNode, 'threeQuarterChimes');
      if rtn <> 'ERROR' then threeQuarterChimes := StrToBool(rtn);
      rtn := readChild(PassNode, 'christmasFont');
      if rtn <> 'ERROR' then christmasFont := StrToBool(rtn);
      rtn := readChild(PassNode, 'easterFont');
      if rtn <> 'ERROR' then easterFont := StrToBool(rtn);
      rtn := readChild(PassNode, 'valentinesFont');
      if rtn <> 'ERROR' then valentinesFont := StrToBool(rtn);
      rtn := readChild(PassNode, 'haloweenFont');
      if rtn <> 'ERROR' then haloweenFont := StrToBool(rtn);
    end;

    //  Timer
    PassNode := Doc.DocumentElement.FindNode('Timer');

    if assigned(PassNode) then
    begin
      rtn := readChild(PassNode, 'timerMilliSeconds');
      if rtn <> 'ERROR' then timerMilliSeconds := StrToBool(rtn);
    end;

    //  Analogue Klock
    PassNode := Doc.DocumentElement.FindNode('AnalogueKlock');

    if assigned(PassNode) then
    begin
      rtn := readChildAttribute(PassNode, 'analogueForm', 'Top');
      if rtn <> 'ERROR' then analogueFormTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'analogueForm', 'Left');
      if rtn <> 'ERROR' then analogueFormLeft := StrToInt(rtn);
      rtn := readChild(PassNode, 'analogueScreenSave');
      if rtn <> 'ERROR' then analogueScreenSave := StrToBool(rtn);
    end;

    //  LED Klock
    PassNode := Doc.DocumentElement.FindNode('LEDKlock');

    if assigned(PassNode) then
    begin
      rtn := readChildAttribute(PassNode, 'LEDForm', 'Top');
      if rtn <> 'ERROR' then LEDFormTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'LEDForm', 'Left');
      if rtn <> 'ERROR' then LEDFormLeft := StrToInt(rtn);
      rtn := readChild(PassNode, 'LEDScreenSave');
      if rtn <> 'ERROR' then LEDScreenSave := StrToBool(rtn);
      rtn := readChild(PassNode, 'LEDlongDate');
      if rtn <> 'ERROR' then LEDlongDate := StrToBool(rtn);
    end;

    //  Binary Klock
    PassNode := Doc.DocumentElement.FindNode('BinaryKlock');

    if assigned(PassNode) then
    begin
      rtn := readChildAttribute(PassNode, 'BinaryForm', 'Top');
      if rtn <> 'ERROR' then BinaryFormTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'BinaryForm', 'Left');
      if rtn <> 'ERROR' then BinaryFormLeft := StrToInt(rtn);
      rtn := readChild(PassNode, 'BinaryScreenSave');
      if rtn <> 'ERROR' then BinaryScreenSave := StrToBool(rtn);
      rtn := readChild(PassNode, 'BinaryFormat');
      if rtn <> 'ERROR' then BinaryFormat := StrToBool(rtn);
    end;

    // Small Text Klock
    PassNode := Doc.DocumentElement.FindNode('SmallTextKlock');

    if assigned(PassNode) then
    begin
      rtn := readChildAttribute(PassNode, 'SmallTextForm', 'Top');
      if rtn <> 'ERROR' then smallTextFormTop := StrToInt(rtn);
      rtn := readChildAttribute(PassNode, 'SmallTextForm', 'Left');
      if rtn <> 'ERROR' then smallTextFormLeft := StrToInt(rtn);
      rtn := readChild(PassNode, 'smallTextScreenSave');
      if rtn <> 'ERROR' then smallTextScreenSave := StrToBool(rtn);
      rtn := readChild(PassNode, 'smallTextTransparent');
      if rtn <> 'ERROR' then smallTextTransparent := StrToBool(rtn);
    end;

    // memos
    PassNode := Doc.DocumentElement.FindNode('Memo');

    if assigned(PassNode) then
    begin
      rtn := readChild(PassNode, 'useDefaultpassWord');
      if rtn <> 'ERROR' then useDefaultpassWord := StrToBool(rtn);
      rtn := readChild(PassNode, 'defaultpassWord');
      if rtn <> 'ERROR' then defaultpassWord := ansistring(rtn);
      rtn := readChild(PassNode, 'decryptTimeOut');
      if rtn <> 'ERROR' then decryptTimeOut := StrToInt(rtn);
    end;

    //Sticky Notes
    PassNode := Doc.DocumentElement.FindNode('StickyNote');

    if assigned(PassNode) then
    begin
      rtn := readChild(PassNode, 'stickyColor');
      if rtn <> 'ERROR' then stickyColor := StringToColor(rtn);
      rtn := readChild(PassNode, 'stickyFont');
      if rtn <> 'ERROR' then stickyFont := StringToFont(rtn);
    end;

    //  Logging
    PassNode := Doc.DocumentElement.FindNode('Logging');

    if assigned(PassNode) then
    begin
      rtn := readChild(PassNode, 'LogginginUse');
      if rtn <> 'ERROR' then logging := StrToBool(rtn);
      rtn := readChild(PassNode, 'cullLogs');
      if rtn <> 'ERROR' then cullLogs := StrToBool(rtn);
      rtn := readChild(PassNode, 'CullLogsDays');
      if rtn <> 'ERROR' then CullLogsDays := StrToInt(rtn);
    end;

  finally
    // finally, free the document
    Doc.Free;
  end;
end;

procedure Options.writeDefaultOptions;
{  Sets us some sensible defaults.
   Used if the useroptions file does not exist.
   The filename is specified when the user options class is created.
   The file info is re-read, in case is has changed
}
var
  fvi: myFileVersionInfo;
begin
  //  retrieve file info i.e build numner etc.
  fvi := myFileVersionInfo.Create;
  fvi.GetFileInfo;

  // Global
  Comments := fvi.fileComments;
  companyName := fvi.fileCompanyName;
  fileDescription := fvi.fileFileDescription;
  fileVersion := fvi.fileFileVersion;
  InternalName := fvi.fileInternalName;
  legalCopyright := fvi.fileLegalCopyright;
  originalFileName := fvi.fileOriginalFileName;
  productName := fvi.fileProductName;
  productVersion := fvi.fileProductVersion;

  //  optionsName set up in create
  memoName := GetAppConfigDir(False) + 'Memo.bin';
  stickyName := GetAppConfigDir(False) + 'StickyNotes.bin';
  unitsName := GetAppConfigDir(False) + 'Units.txt';
  reminderName := GetAppConfigDir(False) + 'kReminder.txt';
  runAtStartUp := false;
  screenSave := True;
  formTop := 100;              //  the forms top left.
  formLeft := 100;
  defaultTab := 0;
  volume := '123';
  monitorClipboard := True;
  CB_screenSave := True;
  CB_formTop := 0;              //  the clipboard manager top left.
  CB_formLeft := 0;

  //  Time
  defaultTime := 0;
  netTimeSeconds := true;
  swatchCentibeats := true;
  fuzzyTimeBalloon := true;
  displayIdleTime := true;
  display24Hour := true;
  hourPips := false;
  hourChimes := false;
  halfChimes := false;
  quarterChimes := false;
  threeQuarterChimes := false;
  christmasFont := true;
  easterFont := true;
  valentinesFont := true;
  haloweenFont := true;

  //  Timer
  timerMilliSeconds := True;

  //  Analogue Klock
  analogueScreenSave := True;
  analogueFormTop := 100;
  analogueFormLeft := 100;

  //  LED Klock
  LEDScreenSave := True;
  LEDFormTop := 100;
  LEDFormLeft := 100;
  LEDlongDate := True;

  //  Binary Klock
  BinaryScreenSave := True;
  BinaryFormTop := 100;
  BinaryFormLeft := 100;
  BinaryFormat := False;     //  default to BCD.

  // Small Text Klock
  smallTextScreenSave := True;
  smallTextFormTop := 100;
  smallTextFormLeft := 100;
  smallTextTransparent := True;

  // memos
  useDefaultpassWord := true;
  defaultpassWord := 'klock';
  decryptTimeOut := 30;

  //Sticky Notes
  stickyColor := clYellow;
  stickyFont := TFont.Create;

  //  Logging
  logging := True;
  cullLogs := False;
  CullLogsDays := 14;
end;

procedure Options.writeCurrentOptions;
{  Writes out the user options to a xml file.
   The filename is specified when the user options class is created.
   The file info is re-read, in case is has changed
}
var
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  fvi: myFileVersionInfo;
begin
  try
    //  retrieve file info i.e build numner etc.
    fvi := myFileVersionInfo.Create;
    fvi.GetFileInfo;

    // Create a document
    Doc := TXMLDocument.Create;

    // Create a root node
    RootNode := Doc.CreateElement('Klock');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    //  Global
    ElementNode := Doc.CreateElement('Global');

    ElementNode.AppendChild(writeStrChild(doc, 'Comments', fvi.fileComments));
    ElementNode.AppendChild(writeStrChild(doc, 'companyName', fvi.filecompanyName));
    ElementNode.AppendChild(writeStrChild(doc, 'fileDescription', fvi.filefileDescription));
    ElementNode.AppendChild(writeStrChild(doc, 'fileVersion', fvi.filefileVersion));
    ElementNode.AppendChild(writeStrChild(doc, 'InternalName', fvi.fileInternalName));
    ElementNode.AppendChild(writeStrChild(doc, 'legalCopyright', fvi.fileLegalCopyright));
    ElementNode.AppendChild(writeStrChild(doc, 'originalFileName', fvi.fileOriginalFileName));
    ElementNode.AppendChild(writeStrChild(doc, 'productName', fvi.fileProductName));
    ElementNode.AppendChild(writeStrChild(doc, 'productVersion', fvi.fileProductVersion));

    ElementNode.AppendChild(writeStrChild(doc, 'optionsName', optionsName));
    ElementNode.AppendChild(writeStrChild(doc, 'memoName', memoName));
    ElementNode.AppendChild(writeStrChild(doc, 'stickyName', stickyName));
    ElementNode.AppendChild(writeStrChild(doc, 'unitsName', unitsName));
    ElementNode.AppendChild(writeStrChild(doc, 'reminderName', reminderName));
    ElementNode.AppendChild(writeBolChild(doc, 'runAtStartUp', runAtStartUp));
    ElementNode.AppendChild(writeBolChild(doc, 'screenSave', screenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'formPosition', formTop, formLeft));
    ElementNode.AppendChild(writeIntChild(doc, 'defaultTab', defaultTab));
    ElementNode.AppendChild(writeStrChild(doc, 'volume', volume));
    ElementNode.AppendChild(writeBolChild(doc, 'monitorClipboard', monitorClipboard));
    ElementNode.AppendChild(writeBolChild(doc, 'CB_screenSave', CB_screenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'CB_formPosition', CB_formTop, CB_formLeft));

    RootNode.AppendChild(ElementNode);

    //  Time
    ElementNode := Doc.CreateElement('Time');

    ElementNode.AppendChild(writeIntChild(doc, 'defaultTime', defaultTime));
    ElementNode.AppendChild(writeBolChild(doc, 'netTimeSeconds', netTimeSeconds));
    ElementNode.AppendChild(writeBolChild(doc, 'swatchCentibeats', swatchCentibeats));
    ElementNode.AppendChild(writeBolChild(doc, 'fuzzyTimeBalloon', fuzzyTimeBalloon));
    ElementNode.AppendChild(writeBolChild(doc, 'displayIdleTime', displayIdleTime));
    ElementNode.AppendChild(writeBolChild(doc, 'display24Hour', display24Hour));
    ElementNode.AppendChild(writeBolChild(doc, 'hourPips', hourPips));
    ElementNode.AppendChild(writeBolChild(doc, 'hourChimes', hourChimes));
    ElementNode.AppendChild(writeBolChild(doc, 'halfChimes', halfChimes));
    ElementNode.AppendChild(writeBolChild(doc, 'quarterChimes', quarterChimes));
    ElementNode.AppendChild(writeBolChild(doc, 'threeQuarterChimes', threeQuarterChimes));
    ElementNode.AppendChild(writeBolChild(doc, 'christmasFont', christmasFont));
    ElementNode.AppendChild(writeBolChild(doc, 'easterFont', easterFont));
    ElementNode.AppendChild(writeBolChild(doc, 'valentinesFont', valentinesFont));
    ElementNode.AppendChild(writeBolChild(doc, 'haloweenFont', haloweenFont));

    RootNode.AppendChild(ElementNode);

    //  Timer
    ElementNode := Doc.CreateElement('Timer');

    ElementNode.AppendChild(writeBolChild(doc, 'timerMilliSeconds', timerMilliSeconds));

    RootNode.AppendChild(ElementNode);

    // Analogue Klock
    ElementNode := Doc.CreateElement('AnalogueKlock');

    ElementNode.AppendChild(writeBolChild(doc, 'analogueScreenSave', analogueScreenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'analogueForm', analogueFormTop, analogueFormLeft));

    RootNode.AppendChild(ElementNode);

    // LED Klock
    ElementNode := Doc.CreateElement('LEDKlock');

    ElementNode.AppendChild(writeBolChild(doc, 'LEDScreenSave', LEDScreenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'LEDForm', LEDFormTop, LEDFormLeft));
    ElementNode.AppendChild(writeBolChild(doc, 'LEDlongDate', LEDlongDate));

    RootNode.AppendChild(ElementNode);

    // Binary Klock
    ElementNode := Doc.CreateElement('BinaryKlock');

    ElementNode.AppendChild(writeBolChild(doc, 'BinaryScreenSave', BinaryScreenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'BinaryForm', BinaryFormTop, BinaryFormLeft));
    ElementNode.AppendChild(writeBolChild(doc, 'BinaryFormat', BinaryFormat));

    RootNode.AppendChild(ElementNode);

    // Small Text Klock
    ElementNode := Doc.CreateElement('SmallTextKlock');

    ElementNode.AppendChild(writeBolChild(doc, 'smallTextScreenSave', smallTextScreenSave));
    ElementNode.AppendChild(writeIntChildAttribute(Doc, 'SmallTextForm', smallTextFormTop, smallTextFormLeft));
    ElementNode.AppendChild(writeBolChild(doc, 'smallTextTransparent', smallTextTransparent));

    RootNode.AppendChild(ElementNode);

    // memos
    ElementNode := Doc.CreateElement('Memo');

    ElementNode.AppendChild(writeBolChild(doc, 'useDefaultpassWord', useDefaultpassWord));
    ElementNode.AppendChild(writeStrChild(doc, 'defaultpassWord', defaultpassWord));
    ElementNode.AppendChild(writeIntChild(doc, 'decryptTimeOut', decryptTimeOut));

    RootNode.AppendChild(ElementNode);

    //Sticky Notes
    ElementNode := Doc.CreateElement('StickyNote');

    ElementNode.AppendChild(writeColChild(doc, 'stickyColor', stickyColor));
    ElementNode.AppendChild(writeFontChild(doc, 'stickyFont', stickyFont));

    RootNode.AppendChild(ElementNode);
    // Logging
    ElementNode := Doc.CreateElement('Logging');

    ElementNode.AppendChild(writeBolChild(doc, 'LogginginUse', logging));
    ElementNode.AppendChild(writeBolChild(doc, 'cullLogs', cullLogs));
    ElementNode.AppendChild(writeIntChild(doc, 'CullLogsDays', CullLogsDays));

    RootNode.AppendChild(ElementNode);

    try
      // Save XML
      WriteXMLFile(Doc, optionsName);
    except
      on E: Exception do
      begin
        ShowMessage('ERROR: Writing XML file.' + LineEnding + E.Message + LineEnding +
          'Halting Program Execution');
        Halt;
      end;  //  on E:
    end;    //  try

  finally
    Doc.Free;
  end;
end;
//
//........................................ fileVersionInfo methods .............
//
procedure myFileVersionInfo.GetFileInfo;
{  Retrieves the file info from the current file.
   Called from the class myFileVersionInfo.
}
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);

  try
    FileVerInfo.ReadFileInfo;

    fileComments := FileVerInfo.VersionStrings.Values['Comments'];
    fileCompanyName := FileVerInfo.VersionStrings.Values['CompanyName'];
    fileFileDescription := FileVerInfo.VersionStrings.Values['FileDescription'];
    fileFileVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
    fileInternalName := FileVerInfo.VersionStrings.Values['InternalName'];
    fileLegalCopyright := FileVerInfo.VersionStrings.Values['LegalCopyright'];
    fileOriginalFileName := FileVerInfo.VersionStrings.Values['OriginalFilename'];
    fileProductName := FileVerInfo.VersionStrings.Values['ProductName'];
    fileProductVersion := FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
    FileVerInfo.Free;
  end;

end;
//
//........................................ Helper functions ....................
//
Function Options.readChild(PassNode: TDOMNode;  name: string): string;
{  Read a child node and return its [string] value.    }
var
    childNode: TDOMNode;
begin
    childNode := PassNode.FindNode(name);

    if assigned(childNode) then
      result := childNode.TextContent
    else
      result := 'ERROR';
end;

Function Options.readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
{  Read a child node and return its named [string] attribute.    }
var
    childNode: TDOMNode;
begin
  childNode := PassNode.FindNode(name);

  if assigned(childNode) then
    result := TDOMElement(childNode).GetAttribute(attribute)
  else
    result := 'ERROR';
end;

function Options.writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
{  Write a [string] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(WideString(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function Options.writeColChild(Doc: TXMLDocument; name: string; value: TColor): TDOMNode;
{  Write a [string] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(ColorToString(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function Options.writeFontChild(Doc: TXMLDocument; name: string; value: TFont): TDOMNode;
{  Write a [font] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(FontToString(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function Options.writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
{  Write a [boolean] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(BoolToStr(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function Options.writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
{  Write a [integer] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(IntToStr(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function Options.writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;
{  Write a [integer] attribute to a child node.

   It seems you have to write both attributes at once, i can read them singularly.
   So, this routine is hard coded for form position until i can fix.
}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TDOMElement(ItemNode).SetAttribute('Top', IntToStr(value1));
  TDOMElement(ItemNode).SetAttribute('Left', IntToStr(value2));
  TextNode := Doc.CreateTextNode('');
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;


end.








