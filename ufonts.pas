unit uFonts;
{  A class to add and remove system fonts.
   The fonts to be added are held in the list Fonts.

   See - https://forum.lazarus.freepascal.org/index.php?topic=21032.0
         mainly post by RAW.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Windows, Messages, typinfo;

type

  fontStore = class

    private
      _fontTypes: TStringList;
      _fontFiles: TstringList;
      _fontNames: TstringList;
      _fontDir: String;

      property fontFiles: TStringList read _fontFiles write _fontFiles;
      property fontNames: TStringList read _fontNames write _fontNames;
      property fontDir: string read _fontDir write _fontDir;

      procedure findFontFiles;
    public
      property fontTypes: TStringList read _fontTypes;

      constructor Create; overload;
      destructor Destroy; override;
      procedure addFonts;
      procedure removeFonts;
      function getFont(fontIndex: Integer): string;
  end;

  CONST
  MM_MAX_NUMAXES =  16;
  FR_PRIVATE     = $10;
  FR_NOT_ENUM    = $20;

 TYPE
  PDesignVector = ^TDesignVector;
  TDesignVector = Packed Record
   dvReserved: DWORD;
   dvNumAxes : DWORD;
   dvValues  : Array[0..MM_MAX_NUMAXES-1] Of LongInt;
  End;

  Function AddFontResourceEx    (Dir : PAnsiChar;
                                Flag: Cardinal;
                                PDV : PDesignVector): Int64; StdCall;
                                External 'GDI32.dll' Name 'AddFontResourceExA';

  Function RemoveFontResourceEx (Dir : PAnsiChar;
                                Flag: Cardinal;
                                PDV : PDesignVector): Int64; StdCall;
                                External 'GDI32.dll' Name 'RemoveFontResourceExA';

  Function AddFont (Dir : PAnsiChar; Flag: DWORD): LongBool; StdCall; External 'GDI32.dll' Name 'AddFontResourceExA';
  Function RemoveFont (Dir : PAnsiChar; Flag: DWORD): LongBool; StdCall; External 'GDI32.dll' Name 'RemoveFontResourceExA';

implementation

uses
  formklock;

constructor fontStore.Create; overload;
{  When the font class is created, populate all three string lists.

      fontTypes - a list of the differnt font types.  This is a human friendly font
                  named that is used in the drop down menus within klock.  Loaded
                  from a text file within the font dir.
      fontFiles - a list of the different font files, scanned from the font directory.
      fontNames - a list of the different font names.  This it actual font name
                 used to load the font.  Loaded form a text file within the font dir.

      The text files must be updated when a new font is added to the directory
      and MUST match.

      If either one of the text files are missing, then just return the default font.
}
begin

  fontDir := ExtractFilePath(Application.ExeName) + 'fonts\';

  _fontTypes := TStringList.Create;
  try
    fontTypes.LoadFromFile(fontDir + 'fontTypes.txt');
  except
    klog.writeLog('ERROR : unable to load fontTypes.txt');
    fontTypes.add('default')
  end;

  klog.writeLog(format('Found %d font Types in %s', [fontTypes.Count, fontDir]));

  _fontNames := TStringList.Create;
  try
    fontNames.LoadFromFile(fontDir + 'fontNames.txt');
  except
    klog.writeLog('ERROR : unable to load fontNames');
    fontNames.add('default')
  end;

  klog.writeLog(format('Found %d font Names in %s', [fontNames.Count, fontDir]));

  findFontFiles;              //  Scan for font files.
  addFonts;                   //  Add fonts to system.
end;

destructor fontStore.Destroy;
{  Unload everything when finished with the font class.    }
begin
  fontFiles.Free;
  fontTypes.Free;
  fontNames.Free;
end;

function fontStore.getFont(fontIndex: Integer): string;
{  Return the actual font name from a user friendly font name.    }
begin
  result := fontNames.Strings[fontIndex];
end;

procedure fontStore.addFonts;
{  Adds all the fonts in the list Fonts.
   Checks that the font file exists.
}
Var
 fontFile: String;
begin
  try

    for fontFile in fontFiles do
    begin
      If FileExists(fontFile) Then
        If AddFontResourceEx(PAnsiChar(fontFile), FR_Private, Nil) <> 0 then
          begin
            SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
            kLog.writeLog('Adding ' + fontFile);
          end;

    end;  // for
  except
    on E: Exception do
    begin
      kLog.writeLog('ERROR: Adding Fonts.' + E.Message);
    end;  //  on E:
  end;    //  try

end;

procedure fontStore.removeFonts;
{  Removes all the fonts in the list Fonts.
   Checks that the font file exists.
}
Var
 fontFile: String;
begin
  try

    for fontFile in fontFiles do
    begin
      If FileExists(fontFile) Then
       If RemoveFontResourceEx(PAnsiChar(fontFile), FR_Private, Nil) <> 0 then
        begin
          SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
          kLog.writeLog('Removing ' + fontFile);
        end;

    end;  // for
  except
    on E: Exception do
    begin
      kLog.writeLog('ERROR: removing Fonts.' + E.Message);
    end;  //  on E:
  end;    //  try

end;

procedure fontStore.findFontFiles;
{  Scans the font directory for all font files [.tff]    }
begin
  _fontFiles := TStringList.Create;
  FindAllFiles(fontFiles, fontDir, '*.ttf', True);

  klog.writeLog(format('Found %d font files in %s', [fontFiles.Count, fontDir]));
end;

end.



