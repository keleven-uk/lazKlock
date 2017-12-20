unit formInfo;
{  Displays useful [hopefully] information.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Grids, uInfoUtils, Types;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    btnClose: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblYear: TLabel;
    lstBxInfo: TListBox;
    SpnEdtYear: TSpinEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstBxInfoDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure SpnEdtYearChange(Sender: TObject);
  private
    _info: string;

    procedure updateInfo;
  public
    property Info: string read _info write _info;
  end;

var
  frmInfo: TfrmInfo;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmInfo }

procedure TfrmInfo.FormShow(Sender: TObject);
begin
  klog.writeLog('FormInfo Show' + Info);

  if info = 'Power Source' then
  begin
    lblYear.Visible := False;
    SpnEdtyear.Visible := False;
  end;

  SpnEdtyear.Value := Currentyear;

  updateInfo;
end;

procedure TfrmInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmInfo.SpnEdtYearChange(Sender: TObject);
{  Updates the information when the year changes.    }
begin
  updateInfo;
end;

procedure TfrmInfo.updateInfo;
{  Updates the labels.    }
var
  strResults: TStringList;
  i: integer;
begin
  strResults := TStringList.Create;

  case Info of
    'Daylight Saving': strResults := getDaylightSaving(SpnEdtyear.Value);
    'Easter Dates': strResults := getEasterDates(SpnEdtyear.Value);
    'Lent Dates': strResults := getLentDates(SpnEdtyear.Value);
    'Power Source': strResults := getPower;
  end;

  lstBxInfo.Items := strResults;
  strResults.free;
end;

procedure TfrmInfo.lstBxInfoDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
{  Centers the test.    }
var
  ts: TTextStyle;
begin
  lstBxInfo.Canvas.FillRect(ARect);
  ts := lstBxInfo.Canvas.TextStyle;
  ts.Alignment := taCenter;
  lstBxInfo.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, lstBxInfo.Items[Index], ts);
end;

end.

