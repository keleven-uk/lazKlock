unit formInfo;

{  Displays useful [hopefully] information.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, mooncomp, uInfoUtils, Types;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    btnClose: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblYear: TLabel;
    lstBxInfo: TListBox;
    moonPhase: TMoon;
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
  klog.writeLog('FormInfo Show ' + Info);

  GroupBox1.Caption := info;

  if info = 'Power Source' then
  begin
    lblYear.Visible := false;
    SpnEdtyear.Visible := false;
  end;

  if (info = 'Moon Stuff') then
  begin
    moonPhase.Enabled := true;
    SpnEdtYear.Visible := false;
    lblYear.Visible := false;
    lstBxInfo.Height := 123;
    lstBxInfo.Left := 8;
    lstBxInfo.Top := 64;
    lstBxInfo.Width := 327;
  end
  else if (info = 'Sun Stuff') then
  begin
  moonPhase.Enabled := false;
  SpnEdtYear.Visible := false;
  lblYear.Visible := false;
  lstBxInfo.Height := 152;
  lstBxInfo.Left := 8;
  lstBxInfo.Top := 0;
  lstBxInfo.Width := 327;
  end
  else
  begin
    moonPhase.Enabled := false;
    lstBxInfo.Height := 152;
    lstBxInfo.Left := 8;
    lstBxInfo.Top := 0;
    lstBxInfo.Width := 327;

    lblYear.Visible := true;
    SpnEdtyear.Visible := true;
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
begin
  strResults := TStringList.Create;

  try
    case Info of
      'Daylight Saving': strResults := getDaylightSaving(SpnEdtyear.Value);
      'Easter Dates': strResults := getEasterDates(SpnEdtyear.Value);
      'Lent Dates': strResults := getLentDates(SpnEdtyear.Value);
      'Chinese Year': strResults := getChineseDates(SpnEdtyear.Value);
      'Power Source': strResults := getPower;
      'Moon Stuff': strResults := getMoonStuff;
      'Sun Stuff': strResults := getSunStuff;
    end;

    lstBxInfo.Items := strResults;
  finally
    strResults.free;
  end;
end;

procedure TfrmInfo.lstBxInfoDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
{  Centers the text.    }
var
  ts: TTextStyle;
begin
  lstBxInfo.Canvas.FillRect(ARect);
  ts := lstBxInfo.Canvas.TextStyle;
  ts.Alignment := taCenter;
  lstBxInfo.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, lstBxInfo.Items[Index], ts);
end;

end.

