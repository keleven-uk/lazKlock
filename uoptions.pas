unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, fileinfo, winpeimagereader, Dialogs, formAnalogueKlock;

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
    _screenSave: boolean;           //  do we save from position or not.
    _formTop: integer;              //  the forms top left.
    _formLeft: integer;
    _defaultTab: integer;
    _volume:String;

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

    //  Timer
    _timerMilliSeconds: boolean;

    //  Analogue Klock
    _analogueScreenSave: boolean;           //  do we save from position or not.
    _analogueFormTop: integer;              //  the forms top left.
    _analogueFormLeft: integer;

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

    //  Timer
    property timerMilliSeconds: boolean read _timerMilliSeconds write _timerMilliSeconds;

    // Analogue Kock
    property analogueScreenSave: boolean read _analogueScreenSave write _analogueScreenSave;
    property analogueFormTop: integer read _analogueFormTop write _analogueFormTop;
    property analogueFormLeft: integer read _analogueFormLeft write _analogueFormLeft;


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


//............................................ Options methods ............................................


constructor Options.Create; overload;
  {  creates the options class with a default filename.  }
begin
  checkDirectory;

  {$ifdef WIN32}
  optionsName := _dirName + 'Options32.xml';
  {$else}
  optionsName := _dirName + 'Options64.xml';
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

  //  Timer
  timerMilliSeconds := o.timerMilliSeconds;

  //  Analogue Klock
  analogueScreenSave := o.analogueScreenSave;
  analogueFormTop := o.analogueFormTop;
  analogueFormLeft := o.analogueFormLeft;
end;

procedure Options.readOptions;
  {  Read in the options file.
     The filename is specified when the user options class is created.
     The file info is re-read, in case is has changed
  }
var
  fvi: myFileVersionInfo;
  PassNode, childNode: TDOMNode;
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
        ShowMessage('ERROR: reading XML file.' + LineEnding + E.Message + LineEnding + 'Halting Program Execution');
        Halt;
      end;  //  on E:
    end;    //  try

    //  Global
    PassNode := Doc.DocumentElement.FindNode('Global');
    childNode := PassNode.FindNode('formPosition');
    _formTop := StrToInt(TDOMElement(childNode).GetAttribute('Top'));              //  the forms top left.
    _formLeft := StrToInt(TDOMElement(childNode).GetAttribute('Left'));

    childNode := PassNode.FindNode('optionsName');
    optionsName := ansistring(childNode.TextContent);

    childNode := PassNode.FindNode('runAtStartUp');
    runAtStartUp := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('screenSave');
    screenSave := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('defaultTab');
    defaultTab := StrToInt(childNode.TextContent);

    childNode := PassNode.FindNode('volume');
    volume := ansistring(childNode.TextContent);

    //  Time
    PassNode := Doc.DocumentElement.FindNode('Time');
    childNode := PassNode.FindNode('defaultTime');
    defaultTime := StrToInt(childNode.TextContent);

    childNode := PassNode.FindNode('netTimeSeconds');
    netTimeSeconds := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('swatchCentibeats');
    swatchCentibeats := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('fuzzyTimeBalloon');
    fuzzyTimeBalloon := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('displayIdleTime');
    displayIdleTime := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('display24Hour');
    display24Hour := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('hourPips');
    hourPips := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('hourChimes');
    hourChimes := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('halfChimes');
    halfChimes := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('quarterChimes');
    quarterChimes := StrToBool(childNode.TextContent);

    childNode := PassNode.FindNode('threeQuarterChimes');
    threeQuarterChimes := StrToBool(childNode.TextContent);

    //  Timer
    PassNode := Doc.DocumentElement.FindNode('Timer');
    childNode := PassNode.FindNode('timerMilliSeconds');
    timerMilliSeconds := StrToBool(childNode.TextContent);

    // Analogue Timer

    PassNode := Doc.DocumentElement.FindNode('AnalogueKlock');
    childNode := PassNode.FindNode('analogueForm');
    _analogueFormTop := StrToInt(TDOMElement(childNode).GetAttribute('Top'));              //  the forms top left.
    _analogueFormLeft := StrToInt(TDOMElement(childNode).GetAttribute('Left'));

    childNode := PassNode.FindNode('analogueScreenSave');
    analogueScreenSave := StrToBool(childNode.TextContent);
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
  runAtStartUp := True;
  screenSave := True;
  formTop := 100;              //  the forms top left.
  formLeft := 100;
  defaultTab := 0;
  volume := '123';

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

  //  Timer
  timerMilliSeconds := True;

  //  Analogue Klock
  analogueScreenSave := True;
  analogueFormTop := 100;
  analogueFormLeft := 100;

  writeCurrentOptions;
end;

procedure Options.writeCurrentOptions;
  {  Writes out the user options to a xml file.
     The filename is specified when the user options class is created.
     The file info is re-read, in case is has changed
  }
var
  Doc: TXMLDocument;
  RootNode, ElementNode, ItemNode, TextNode: TDOMNode;
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

    ItemNode := Doc.CreateElement('Comments');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileComments));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('companyName');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileCompanyName));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('fileDescription');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileFileDescription));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('fileVersion');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileFileVersion));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('InternalName');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileInternalName));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('legalCopyright');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileLegalCopyright));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('originalFileName');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileOriginalFileName));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('productName');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileProductName));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('productVersion');
    TextNode := Doc.CreateTextNode(WideString(fvi.fileProductVersion));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('optionsName');
    TextNode := Doc.CreateTextNode(WideString(optionsName));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('runAtStartUp');
    TextNode := Doc.CreateTextNode(BoolToStr(runAtStartUp));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('screenSave');
    TextNode := Doc.CreateTextNode(BoolToStr(screenSave));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('formPosition');              //  the forms top left.
    TDOMElement(ItemNode).SetAttribute('Top', IntToStr(formTop));
    TDOMElement(ItemNode).SetAttribute('Left', IntToStr(formLeft));
    TextNode := Doc.CreateTextNode('');
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('defaultTab');
    TextNode := Doc.CreateTextNode(IntToStr(defaultTab));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('volume');
    TextNode := Doc.CreateTextNode(WideString(volume));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    RootNode.AppendChild(ElementNode);

    //  Time
    ElementNode := Doc.CreateElement('Time');
    ItemNode := Doc.CreateElement('defaultTime');
    TextNode := Doc.CreateTextNode(IntToStr(defaultTime));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('netTimeSeconds');
    TextNode := Doc.CreateTextNode(BoolToStr(netTimeSeconds));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('swatchCentibeats');
    TextNode := Doc.CreateTextNode(BoolToStr(swatchCentibeats));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('fuzzyTimeBalloon');
    TextNode := Doc.CreateTextNode(BoolToStr(fuzzyTimeBalloon));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('displayIdleTime');
    TextNode := Doc.CreateTextNode(BoolToStr(displayIdleTime));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('display24Hour');
    TextNode := Doc.CreateTextNode(BoolToStr(display24Hour));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('hourPips');
    TextNode := Doc.CreateTextNode(BoolToStr(hourPips));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('hourChimes');
    TextNode := Doc.CreateTextNode(BoolToStr(hourChimes));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('halfChimes');
    TextNode := Doc.CreateTextNode(BoolToStr(halfChimes));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('quarterChimes');
    TextNode := Doc.CreateTextNode(BoolToStr(quarterChimes));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('threeQuarterChimes');
    TextNode := Doc.CreateTextNode(BoolToStr(threeQuarterChimes));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    RootNode.AppendChild(ElementNode);

    //  Timer
    ElementNode := Doc.CreateElement('Timer');
    ItemNode := Doc.CreateElement('timerMilliSeconds');
    TextNode := Doc.CreateTextNode(BoolToStr(timerMilliSeconds));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    RootNode.AppendChild(ElementNode);

    // Analogue Klock
    ElementNode := Doc.CreateElement('AnalogueKlock');

    ItemNode := Doc.CreateElement('analogueScreenSave');
    TextNode := Doc.CreateTextNode(BoolToStr(analogueScreenSave));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('analogueForm');              //  the forms top left.
    TDOMElement(ItemNode).SetAttribute('Top', IntToStr(analogueFormTop));
    TDOMElement(ItemNode).SetAttribute('Left', IntToStr(analogueFormLeft));
    TextNode := Doc.CreateTextNode('');
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    RootNode.AppendChild(ElementNode);

    try
      // Save XML
      WriteXMLFile(Doc, optionsName);
    except
      on E: Exception do
      begin
        ShowMessage('ERROR: Writing XML file.' + LineEnding + E.Message + LineEnding + 'Halting Program Execution');
        Halt;
      end;  //  on E:
    end;    //  try

  finally
    Doc.Free;
  end;
end;


//........................................ fileVersionInfo methods ............................................


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







