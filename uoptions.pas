unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, fileinfo, winpeimagereader, Dialogs,
  uOptionsUtils;

type

  { Options }

  Options = class
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

     TODO :: causes a read failure if an option has been added to the class which is not in the XML file.
             Needs some way to check.
  }
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

    //  Logging
    _logging: Boolean;
    _cullLogs: Boolean;
    _CullLogsDays: integer;

    procedure checkDirectory;
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

  {$IFDEF DEBUG}
    optnFile := 'DEBUG_Options';
  {$else}
    optnFile := 'Options';
  {$endif}
  {$ifdef WIN32}
    optionsName := _dirName + optnFile + '32_temp.xml';
  {$else}
    optionsName := _dirName + optnFile + '64_temp.xml';
  {$endif}

  if FileExists(optionsName) then
    readOptions
  else
    writeDefaultOptions;
end;

constructor Options.Create(filename: string);
{  creates the options class with a specified filename.  }
begin
  checkDirectory;

  optionsName := _dirName + fileName;

  if FileExists(optionsName) then
    readOptions
  else
    writeDefaultOptions;
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

  //  Logging
  logging := o.logging;
  cullLogs := o.cullLogs;
  CullLogsDays := o.CullLogsDays;
end;

procedure Options.readOptions;
{  Read in the options file.
   The filename is specified when the user options class is created.
   The file info is re-read, in case is has changed
}
var
  fvi: myFileVersionInfo;
  PassNode: TDOMNode;
  Doc: TXMLDocument;
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

    formTop := StrToInt(readChildAttribute(PassNode, 'formPosition', 'Top'));
    formLeft := StrToInt(readChildAttribute(PassNode, 'formPosition', 'Left'));

    optionsName := ansistring(readChild(PassNode, 'optionsName'));
    runAtStartUp := StrToBool(readChild(PassNode, 'runAtStartUp'));
    screenSave := StrToBool(readChild(PassNode, 'screenSave'));
    defaultTab := StrToInt(readChild(PassNode, 'defaultTab'));
    volume := ansistring(readChild(PassNode, 'volume'));
    monitorClipboard := StrToBool(readChild(PassNode, 'monitorClipboard'));
    CB_screenSave := StrToBool(readChild(PassNode, 'CB_screenSave'));
    CB_formTop := StrToInt(readChildAttribute(PassNode, 'CB_formPosition', 'Top'));
    CB_formLeft := StrToInt(readChildAttribute(PassNode, 'CB_formPosition', 'Left'));

    //  Time
    PassNode := Doc.DocumentElement.FindNode('Time');

    defaultTime := StrToInt(readChild(PassNode, 'defaultTime'));
    netTimeSeconds := StrToBool(readChild(PassNode, 'netTimeSeconds'));
    swatchCentibeats := StrToBool(readChild(PassNode, 'swatchCentibeats'));
    fuzzyTimeBalloon := StrToBool(readChild(PassNode, 'fuzzyTimeBalloon'));
    displayIdleTime := StrToBool(readChild(PassNode, 'displayIdleTime'));
    display24Hour := StrToBool(readChild(PassNode, 'display24Hour'));
    hourPips := StrToBool(readChild(PassNode, 'hourPips'));
    hourChimes := StrToBool(readChild(PassNode, 'hourChimes'));
    halfChimes := StrToBool(readChild(PassNode, 'halfChimes'));
    quarterChimes := StrToBool(readChild(PassNode, 'quarterChimes'));
    threeQuarterChimes := StrToBool(readChild(PassNode, 'threeQuarterChimes'));
    christmasFont := StrToBool(readChild(PassNode, 'christmasFont'));

    //  Timer
    PassNode := Doc.DocumentElement.FindNode('Timer');

    timerMilliSeconds := StrToBool(readChild(PassNode, 'timerMilliSeconds'));

    //  Analogue Klock
    PassNode := Doc.DocumentElement.FindNode('AnalogueKlock');

    analogueFormTop := StrToInt(readChildAttribute(PassNode, 'analogueForm', 'Top'));
    analogueFormLeft := StrToInt(readChildAttribute(PassNode, 'analogueForm', 'Left'));
    analogueScreenSave := StrToBool(readChild(PassNode, 'analogueScreenSave'));

    //  LED Klock
    PassNode := Doc.DocumentElement.FindNode('LEDKlock');

    LEDFormTop := StrToInt(readChildAttribute(PassNode, 'LEDForm', 'Top'));
    LEDFormLeft := StrToInt(readChildAttribute(PassNode, 'LEDForm', 'Left'));
    LEDScreenSave := StrToBool(readChild(PassNode, 'LEDScreenSave'));
    LEDlongDate := StrToBool(readChild(PassNode, 'LEDlongDate'));

    //  Binary Klock
    PassNode := Doc.DocumentElement.FindNode('BinaryKlock');

    BinaryFormTop := StrToInt(readChildAttribute(PassNode, 'BinaryForm', 'Top'));
    BinaryFormLeft := StrToInt(readChildAttribute(PassNode, 'BinaryForm', 'Left'));
    BinaryScreenSave := StrToBool(readChild(PassNode, 'BinaryScreenSave'));
    BinaryFormat := StrToBool(readChild(PassNode, 'BinaryFormat'));

    // Small Text Klock
    PassNode := Doc.DocumentElement.FindNode('SmallTextKlock');

    smallTextFormTop := StrToInt(readChildAttribute(PassNode, 'SmallTextForm', 'Top'));
    smallTextFormLeft := StrToInt(readChildAttribute(PassNode, 'SmallTextForm', 'Left'));
    smallTextScreenSave := StrToBool(readChild(PassNode, 'smallTextScreenSave'));
    smallTextTransparent := StrToBool(readChild(PassNode, 'smallTextTransparent'));

    //  Logging
    PassNode := Doc.DocumentElement.FindNode('Logging');

    logging := StrToBool(readChild(PassNode, 'LogginginUse'));
    cullLogs := StrToBool(readChild(PassNode, 'cullLogs'));
    CullLogsDays := StrToInt(readChild(PassNode, 'CullLogsDays'));

  finally
    // finally, free the document
    Doc.Free;
  end;
end;

procedure Options.writeDefaultOptions;
{  Sets us some sensible defaults and then calls writeCurrentOptions to writs out the xml file.
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

  optionsName := optionsName;
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
  netTimeSeconds := True;
  swatchCentibeats := True;
  fuzzyTimeBalloon := True;
  displayIdleTime := True;
  display24Hour := True;
  hourPips := False;
  hourChimes := False;
  halfChimes := False;
  quarterChimes := False;
  threeQuarterChimes := False;
  christmasFont := True;

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

  //  Logging
  logging := True;
  cullLogs := False;
  CullLogsDays := 14;

  writeCurrentOptions;
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
//........................................ fileVersionInfo methods ............................................
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


end.







