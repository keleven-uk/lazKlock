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
  Fonts = (BarCode39, NancyBlackett, BrailleLatin, Semaphore, Christmas, Hack,
  rmbunny, SweetHearts, groovyGhosties, ChristmasCard);

  fontStore = class

    private

    public
      procedure addFonts;
      procedure removeFonts;
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

procedure fontStore.addFonts;
{  Adds all the fonts in the list Fonts.
   Checks that the font file exists.
}
Var
 strAppPath: String;
 fontName: String;
 f: integer;
begin
    try
    strAppPath:= ExtractFilePath(Application.ExeName) + 'fonts\';

    for f := ord(low(Fonts)) to ord(high(Fonts)) do
    begin
      fontName := strAppPath + GetEnumName(TypeInfo(Fonts), f) + '.ttf';

      If FileExists(fontName) Then
        If AddFontResourceEx(PAnsiChar(fontName), FR_Private, Nil) <> 0 then
          begin
            SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
            kLog.writeLog('Adding ' + fontName);
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
 strAppPath: String;
 fontName: String;
 f: integer;
begin
  try
    strAppPath:= ExtractFilePath(Application.ExeName) + 'fonts\';

    for f := ord(low(Fonts)) to ord(high(Fonts)) do
    begin
      fontName := strAppPath + GetEnumName(TypeInfo(Fonts), f) + '.ttf';

      If FileExists(fontName) Then
       If RemoveFontResourceEx(PAnsiChar(fontName), FR_Private, Nil) <> 0 then
        begin
          SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
          kLog.writeLog('Removing ' + fontName);
        end;

    end;  // for
  except
    on E: Exception do
    begin
      kLog.writeLog('ERROR: removing Fonts.' + E.Message);
    end;  //  on E:
  end;    //  try

end;

end.



