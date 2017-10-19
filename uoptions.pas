unit uOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, fileinfo, winpeimagereader, Dialogs;

type
  Options = class
  {  Holds the current user options.  The options are hard coded in this class.

     When the class is created, the constructor first checks that the user directory
     exists - this should be something like c:\Users\<user>\AppData\Local\<app Name>\Options.xml.
     The file name can be passed to the contructor, if it's absent the 'Options.xml' is used.
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

     NOTE :: All values are string.  So, all will be returned as strings and all should returned as strings.

     TODO :: causes a read failure if an option has been added to the class which is not in the XML file.
             Needs some way to check.
  }
    private
      _fileName: String;
      _dirName: String;
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

      _screenSave: String;           //  to we save from position or not.
      _formTop: string;              //  the forms top left.
      _formLeft: string;
      _defaultTab: string;

      //  Time
      _defaultTime: String;
      _netTimeSeconds: String;
      _swatchCentibeats: String;
      _fuzzyTimeBalloon: String;

      //  Timer
      _timerMilliSeconds: string;


      procedure checkDirectory;
    public
      //  Global - file stuff
      property Comments: String read _Comments write _Comments;
      property companyName: String read _companyName write _companyName;
      property fileDescription: String read _fileDescription write _fileDescription;
      property fileVersion: String read _fileVersion write _fileVersion;
      property InternalName: String read _InternalName write _InternalName;
      property legalCopyright: String read _legalCopyright write _legalCopyright;
      property originalFileName: String read _originalFileName write _originalFileName;
      property productName: String read _productName write _productName;
      property productVersion: String read _productVersion write _productVersion;

      //  Global - other stuff
      property screenSave: String read _screenSave write _screenSave;
      property formTop: String read _formTop write _formTop;
      property formLeft: String read _formLeft write _formLeft;
      property defaultTab: String read _defaultTab write _defaultTab;

      //  Time
      property defaultTime: String read _defaultTime write _defaultTime;
      property netTimeSeconds: String read _netTimeSeconds write _netTimeSeconds;
      property swatchCentibeats: String read _swatchCentibeats write _swatchCentibeats;
      property fuzzyTimeBalloon: String read _fuzzyTimeBalloon write _fuzzyTimeBalloon;

      //  Timer
      property timerMilliSeconds: String read _timerMilliSeconds write _timerMilliSeconds;

      constructor Create; overload;
      constructor Create(filename: String); overload;

      procedure readOptions;
      procedure writeCurrentOptions;
      procedure writeDefaultOptions;
  end;  //  class

  myFileVersionInfo = class
  {  Retrieves the current file info.

     see http://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company.

     NOTE :: The information in Project / Project Options / Version Ino in the IDE
     must be up to date.
  }

    private
      _comments: String;
      _companyName: String;
      _fileDescription: String;
      _fileVersion: String;
      _InternalName: String;
      _legalCopyright: String;
      _originalFileName: String;
      _productName: String;
      _productVersion: String;

    public
      property fileComments: String read _comments write _comments;
      property fileCompanyName: String read _companyName write _companyName;
      property fileFileDescription: String read _fileDescription write _fileDescription;
      property fileFileVersion: String read _fileVersion write _fileVersion;
      property fileInternalName: String read _InternalName write _InternalName;
      property fileLegalCopyright: String read _legalCopyright write _legalCopyright;
      property fileOriginalFileName: String read _originalFileName write _originalFileName;
      property fileProductName: String read _productName write _productName;
      property fileProductVersion: String read _productVersion write _productVersion;

      procedure GetFileInfo;
  end;

implementation

//
//............................................ Options methods ............................................
//

  constructor Options.Create; overload;
  {  creates the options class with a default filename.  }
  begin

    checkDirectory;

    _filename := _dirName + 'Options.xml';

    If FileExists(_filename) Then
      readOptions
    else
      writeDefaultOptions;

  end;

  constructor Options.Create(fileName: String); overload;
  {  creates the options class with a specified filename.  }
  begin
    checkDirectory;

    _filename := _dirName + fileName;

    If FileExists(_filename) Then
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

    If NOT DirectoryExists(_dirName) Then
      If Not CreateDir (_dirName) Then
        ShowMessage('Failed to create directory !');
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
      fvi:= myFileVersionInfo.create;
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
        ReadXMLFile(Doc, _filename);
      except
        on E: Exception do
        begin
          ShowMessage('ERROR: reading XML file.' + LineEnding
                    + E.Message + LineEnding
                    + 'Halting Program Execution');
          Halt;
        end;  //  on E:
      end;    //  try

      //  Global
      PassNode := Doc.DocumentElement.FindNode('Global');
      childNode := PassNode.FindNode('formPosition');
      _formTop := ansiString(TDOMElement(childNode).GetAttribute('Top'));              //  the forms top left.
      _formLeft := ansiString(TDOMElement(childNode).GetAttribute('Left'));

      childNode := PassNode.FindNode('screenSave');
      screenSave := ansiString(childNode.TextContent);

      childNode := PassNode.FindNode('defaultTab');
      defaultTab := ansiString(childNode.TextContent);

      //  Time
      PassNode := Doc.DocumentElement.FindNode('Time');
      childNode := PassNode.FindNode('defaultTime');
      defaultTime := ansiString(childNode.TextContent);

      childNode := PassNode.FindNode('netTimeSeconds');
      netTimeSeconds := ansiString(childNode.TextContent);

      childNode := PassNode.FindNode('swatchCentibeats');
      swatchCentibeats := ansiString(childNode.TextContent);

      childNode := PassNode.FindNode('fuzzyTimeBalloon');
      fuzzyTimeBalloon := ansiString(childNode.TextContent);

      //  Timer
      PassNode := Doc.DocumentElement.FindNode('Timer');
      childNode := PassNode.FindNode('timerMilliSeconds');
      timerMilliSeconds := ansiString(childNode.TextContent);

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
    fvi:= myFileVersionInfo.create;
    fvi.GetFileInfo;

    // Global
    Comments := fvi.fileComments ;
    companyName := fvi.fileCompanyName;
    fileDescription := fvi.fileFileDescription;
    fileVersion := fvi.fileFileVersion;
    InternalName := fvi.fileInternalName;
    legalCopyright := fvi.fileLegalCopyright;
    originalFileName := fvi.fileOriginalFileName;
    productName := fvi.fileProductName;
    productVersion := fvi.fileProductVersion;

    screenSave := 'True';
    formTop := '100';              //  the forms top left.
    formLeft := '100';
    defaultTab :='0';

    //  Time
    defaultTime :='0';
    netTimeSeconds:= 'True';
    swatchCentibeats:= 'True';
    fuzzyTimeBalloon := 'True';

    //  Timer
    timerMilliSeconds := 'True';

    writeCurrentOptions;
  end;

  procedure Options.writeCurrentOptions;
  {  Writes out the user options to a xml file.
     The filename is specified when the user options class is created.
     The file info is re-read, in case is has changed
  }
  var
  Doc: TXMLDocument;
  RootNode, ElementNode,ItemNode,TextNode: TDOMNode;
  fvi: myFileVersionInfo;
  begin
    try
      //  retrieve file info i.e build numner etc.
      fvi:= myFileVersionInfo.create;
      fvi.GetFileInfo;

      // Create a document
      Doc := TXMLDocument.Create;

      // Create a root node
      RootNode := Doc.CreateElement('Klock');
      Doc.Appendchild(RootNode);
      RootNode:= Doc.DocumentElement;

      //  Global
      ElementNode:=Doc.CreateElement('Global');

      ItemNode:=Doc.CreateElement('Comments');
      TextNode:=Doc.CreateTextNode(wideString(fvi.fileComments));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('companyName');
      TextNode:=Doc.CreateTextNode(wideString(fvi.fileCompanyName));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('fileDescription');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileFileDescription));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('fileVersion');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileFileVersion));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('InternalName');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileInternalName));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('legalCopyright');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileLegalCopyright));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('originalFileName');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileOriginalFileName));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('productName');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileProductName));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('productVersion');
      TextNode:=Doc.CreateTextNode(WideString(fvi.fileProductVersion));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('screenSave');
      TextNode:=Doc.CreateTextNode(WideString(screenSave));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('formPosition');              //  the forms top left.
      TDOMElement(ItemNode).SetAttribute('Top', WideString(formTop));
      TDOMElement(ItemNode).SetAttribute('Left', WideString(formLeft));
      TextNode:=Doc.CreateTextNode('');
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('defaultTab');
      TextNode:=Doc.CreateTextNode(WideString(defaultTab));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      RootNode.AppendChild(ElementNode);

      //  Time
      ElementNode:=Doc.CreateElement('Time');
      ItemNode:=Doc.CreateElement('defaultTime');
      TextNode:=Doc.CreateTextNode(WideString(defaultTime));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('netTimeSeconds');
      TextNode:=Doc.CreateTextNode(WideString(netTimeSeconds));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('swatchCentibeats');
      TextNode:=Doc.CreateTextNode(WideString(swatchCentibeats));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      ItemNode:=Doc.CreateElement('fuzzyTimeBalloon');
      TextNode:=Doc.CreateTextNode(WideString(fuzzyTimeBalloon));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      RootNode.AppendChild(ElementNode);

      //  Timer
      ElementNode:=Doc.CreateElement('Timer');
      ItemNode:=Doc.CreateElement('timerMilliSeconds');
      TextNode:=Doc.CreateTextNode(WideString(timerMilliSeconds));
      ItemNode.AppendChild(TextNode);
      ElementNode.AppendChild(ItemNode);

      RootNode.AppendChild(ElementNode);

      try
        // Save XML
        WriteXMLFile(Doc, _filename);
      except
        on E: Exception do
        begin
          ShowMessage('ERROR: Writing XML file.' + LineEnding
                    + E.Message + LineEnding
                    + 'Halting Program Execution');
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



