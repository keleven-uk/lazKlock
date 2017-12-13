unit uFonts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Windows, Messages, typinfo;

type
  Fonts = (BarCode39, NancyBlackett, BrailleLatin, Semaphore);

  fontStore = class

    private

    public
      procedure addFonts(klockHandle: QWord);
      procedure removeFonts(klockHandle: QWord);
  end;

  Function AddFont (Dir : PAnsiChar; Flag: DWORD): LongBool; StdCall; External GDI32 Name 'AddFontResourceExA';
  Function RemoveFont (Dir : PAnsiChar; Flag: DWORD): LongBool; StdCall; External GDI32 Name 'RemoveFontResourceExA';

implementation

procedure fontStore.addFonts(klockHandle: QWord);
Var
 strAppPath: String;
 fontName: String;
 f: integer;
begin
  strAppPath:= ExtractFilePath(Application.ExeName) + '/fonts/';

  for f := ord(low(Fonts)) to ord(high(Fonts)) do
  begin
    fontName := GetEnumName(TypeInfo(Fonts), f) + '.ttf';

    If FileExists(strAppPath+fontName) Then
      If AddFont(PAnsiChar(strAppPath+fontName), $10) Then
        SendMessage(klockHandle, WM_FONTCHANGE, 0, 0)
       else
        ShowMessage('Error :: Loading Font');
  end;  // for
end;

procedure fontStore.removeFonts(klockHandle: QWord);
Var
 strAppPath: String;
 fontName: String;
 f: integer;
begin
  try
    strAppPath:= ExtractFilePath(Application.ExeName) + '/fonts/';

    for f := ord(low(Fonts)) to ord(high(Fonts)) do
    begin
      fontName := GetEnumName(TypeInfo(Fonts), f) + '.ttf';

      If FileExists(strAppPath+fontName) Then
       If RemoveFont(PAnsiChar(strAppPath+fontName), $10) Then
        SendMessage(klockHandle, WM_FONTCHANGE, 0, 0)
      else
       ShowMessage('Error :: Removing Font');
    end;  // for
  except
    on E: Exception do
    begin
      ShowMessage('ERROR: removing Fonts.' + LineEnding + E.Message);
    end;  //  on E:
  end;    //  try
end;

end.



