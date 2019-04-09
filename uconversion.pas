unit UConversion;

{   Based on article in http://www.dreamincode.net/forums/topic/121316-an-updatable-unit-conversion-calculator/

   A Helper module that contains functions linked to the convert tab.

   File to hold conversion constants is held in user options and passed in.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, LazFileUtils, UKlockUtils;

procedure checkConversionUnitsFile(convFile: string);
procedure readConversionUnitsFile(convFile: string);
procedure parseConversionUnitsFile(mode: string);
procedure EditConversionUnitsFile(convFile: string);
procedure cleartextFiles;

implementation

uses
  formklock;

procedure checkConversionUnitsFile(convFile: string);
{  Creates the default units.txt.
   This file is used to hold conversion data.
}
VAR
  unitFile: TextFile;
begin
  if fileExists(convFile) then exit;

  AssignFile(unitFile, convFile);

  try
    klog.writeLog('Creating Conversion Units File.');
    rewrite(unitFile);

    writeLn(unitFile, '# Klock Conversion file');
    writeLn(unitFile, '# If adding - Please use the same format');
    writeLn(unitFile, '#');
    writeLn(unitFile, 'Category, Weight');
    writeLn(unitFile, 'Category, Distance');
    writeLn(unitFile, 'Category, Area');
    writeLn(unitFile, 'Category, Liquid');
    writeLn(unitFile, 'Category, Volume');
    writeLn(unitFile, 'Category, Power');
    writeLn(unitFile, 'Category, Presure');
    writeLn(unitFile, 'Weight, LB to KG, 0.45359237');
    writeLn(unitFile, 'Weight, KG to LB, 2.20462262');
    writeLn(unitFile, 'Weight, Ton to KG, 6.350293');
    writeLn(unitFile, 'Weight, KG to Ton, 0.157473');
    writeLn(unitFile, 'Distance, Mile to KM, 1.609344');
    writeLn(unitFile, 'Distance, KM to Mile, 0.621371192');
    writeLn(unitFile, 'Distance, Inch to CM, 2.54');
    writeLn(unitFile, 'Distance, CM to Inch, 0.3937');
    writeLn(unitFile, 'Distance, Furlong to Metre, 201.168');
    writeLn(unitFile, 'Distance, Metre to Furlong, .0004970970');
    writeLn(unitFile, 'Distance, Fathom to Metre, 1.8288');
    writeLn(unitFile, 'Distance, Metre to Fathom, 0.546806');
    writeLn(unitFile, 'Area, Square Foot to Square Metre, 0.029280304');
    writeLn(unitFile, 'Area, Square Metre to Square Foot, 10.763910');
    writeLn(unitFile, 'Area, Square Yard to Square Metre, 0.836127');
    writeLn(unitFile, 'Area, Square Metre to Square Yard, 1.19599');
    writeLn(unitFile, 'Liquid, Gallon to Litre, 4.546');
    writeLn(unitFile, 'Liquid, Litre to Gallon, 0.22');
    writeLn(unitFile, 'Liquid, Pint to Litre, 0.56826125');
    writeLn(unitFile, 'Liquid, Litre to Pint, 1.7597538772348');
    writeLn(unitFile, 'Volume, Cubic Centimetres to Cubic Inches, 0.061');
    writeLn(unitFile, 'Volume, Cubic Centimetres to Cubic Inches, 0.061');
    writeLn(unitFile, 'Power, Horse power to Watts, 745.7');
    writeLn(unitFile, 'Power, Watts to Horse Power, 0.001341022');
    WriteLn(unitFile, 'Presure, Bar to PSI, 14.5038');
    WriteLn(unitFile, 'Presure, PSI to bar, 0.0689476');

    CloseFile(unitFile);
  except
    on E: EInOutError do
      klog.writeLog('ERROR : Creating Units File');
  end;
end;

procedure readConversionUnitsFile(convFile: string);
{  Loads the data file units.txt, which contains the data for the conversions
   The file is loaded into a global string list, this improves performance later on.
   So this is only carried out when the conversion tab is chosen.

   i.e.
   Category, Weight
   Category, Distance
   Weight, LB To KG, 0.45359237
   Weight, KG To LB, 2.20462262
   Distance, Mile To KM, 1.609344
   Distance, KN To Mile, 0.621371192
}
begin
  if fileExists(convFile) then
     ConversionUnits.LoadFromFile(convFile)
  else
    klog.writeLog('ERROR : Reading Units File');
end;

procedure parseConversionUnitsFile(mode: string);
{  Parses the conversion file.
   This can be called with several modes -
     LoadCategory - loads the categories only and populates the required combo bow.
     LoadUnits - loads the units only and populates the required combo box.
     SelectUnits - determines the conversion factor that matched the two combo boxes.
}
var
  f   : integer;
  line: TStringArray;
begin
  line := TStringArray.create;

  if mode = 'LoadUnits' then
    frmMain.CmbBxConvertTo.Items.Clear;  //  Clear convert to - in case of reload.

  for f := 0 to ConversionUnits.Count - 1 do
  begin
    if ConversionUnits[f].StartsWith('#') then continue;  //  ignore comments.

    line := ConversionUnits[f].Split(',');

    case mode of
      'LoadCategory':
      begin
        if line[0] = 'Category' then
          frmMain.CmbBxCategory.Items.Add(trim(line[1]));

        frmMain.CmbBxCategory.ItemIndex := 0;
      end;
      'LoadUnits':
      begin
        if line[0] = frmMain.CmbBxCategory.Items[frmMain.CmbBxCategory.ItemIndex] then
          frmMain.CmbBxConvertTo.Items.Add(line[1]);

        frmMain.CmbBxConvertTo.ItemIndex := 0;
      end;
      'SelectUnit':
      begin
        if line[1] = frmMain.CmbBxConvertTo.Items[frmMain.CmbBxConvertTo.ItemIndex] then
        begin
          try
            unitConvertfactor := StrToFloat(line[2]);
          except
            On E : Exception do
              klog.writeLog('ERROR: converting Converion factor to double from string.');
          end;  //  try
        end;    //  if b[1] = frmMain.CmbBxConvertTo.Items[frmMain.CmbBxConvertTo.ItemIndex] then
      end;      //  'SelectUnit':
    end;        //  case mode of
  end;          // for f := 0 to ConversionUnits.Count - 1 do

end;

procedure EditConversionUnitsFile(convFile: string);
{  Loads the conversion file into notepad, so it can be edited or added to.   }
begin
  if fileExists(convFile) then
    doCommandEvent('notepad.exe', convFile)
  else
    klog.writeLog('ERROR : Missing Units File');
end;

procedure cleartextFiles;
{  Clears required edit boxes and disables the convert button.    }
begin
  frmmain.EdtConverionResult.Text     := '';
  frmmain.EdtConverionValue.Text      := '';
  frmMain.btnConverionConvert.Enabled := false;
end;

end.

