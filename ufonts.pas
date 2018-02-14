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
  Fonts = (BarCode39, NancyBlackett, BrailleLatin, Semaphore, KBABCDoodles,
  Backwards, UpsideDown, Christmas, rmbunny, SweetHearts, groovyGhosties,
  ChristmasCard, Hack, GushingMeadow, GallaudetRegular, chintzy, chintzys,
  CartoonBones, Behistun, HieraticNumerals);

  fontStore = class

    private
      _fontTypes: TStringList;
    public
      property fontTypes: TStringList read _fontTypes;

      constructor Create; overload;
      function getFont(fontIndex: Integer): string;
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

constructor fontStore.Create; overload;
begin
  //  Create a string list of user friendly fonr names.
  //  If a new font is added - add a nice name and the actual font name below
  //  also the file name above..

  _fontTypes := TStringList.Create;;
  _fontTypes.CommaText := ('"Default", "Bar Code", "Nancy Blackett", "Semaphore",' +
  '"Braille", "Dominoes", "Backwards", "Upside Down", "Christmas", "Christmas Card",' +
  '"Easter Bunny", "Sweet Hearts", "Groovy Ghosties", "Gushing Meadow", "Gallaudet",' +
  '"chintzy", "chintzy Shadow", "Cartoon Bones", "Behistun", "Hieratic Numerals"');
end;

function fontStore.getFont(fontIndex: Integer): string;
{  Return the actual font name from a user friendly font name.    }
begin
  case fontIndex of
    0: Result := 'default';
    1: Result := 'Bar Code 39';
    2: Result := 'Nancy Blackett semaphore';
    3: Result := 'Semaphore';
    4: Result := 'BrailleLatin';
    5: Result := 'KBABCDoodles';
    6: Result := 'Backwards';
    7: Result := 'Upside down surprise by georgia';
    8: Result := 'Christmas';
    9: Result := 'Christmas Card';
    10: Result := 'RMBunny';
    11: Result := 'Sweet Hearts BV';
    12: Result := 'Groovy Ghosties';
    13: Result := 'SF Gushing Meadow';
    14: Result := 'Gallaudet';
    15: Result := 'Chintzy CPU BRK';
    16: Result := 'Chintzy CPU Shadow BRK';
    17: Result := 'Cartoon Bones';
    18: Result := 'Behistun';
    19: Result := 'Hieratic Numerals';
  end;
end;

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



