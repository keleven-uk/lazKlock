unit formInfo;

{  Displays useful [hopefully] information.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, mooncomp, uInfoUtils, LCLType, typinfo, Windows, Moon, MouseAndKeyInput;

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

  lstBxInfo.Height := 182;
  lstBxInfo.Left := 8;
  lstBxInfo.Top := 0;
  lstBxInfo.Width := 368;
  // listbox can be scrolled by double width horizontally now:
  SendMessage (lstBxInfo.Handle, LB_SETHORIZONTALEXTENT, lstBxInfo.Width * 2, 0);

  case info of
    'Power Source',
    'Monitor Stuff':
    begin
      moonPhase.Enabled := false;
      lblYear.Visible := false;
      SpnEdtyear.Visible := false;

    end;
    'Moon Stuff':
    begin
      moonPhase.Enabled := true;
      SpnEdtYear.Visible := false;
      lblYear.Visible := false;
      lstBxInfo.Height := 123;
      lstBxInfo.Top := 64;
    end;
    'Sun Stuff':
    begin
      moonPhase.Enabled := false;
      SpnEdtYear.Visible := false;
      lblYear.Visible := false;
    end;
    else
    begin
      lstBxInfo.Height := 152;
      moonPhase.Enabled := false;
      lblYear.Visible := true;
      SpnEdtyear.Visible := true;
      SpnEdtyear.Value := Currentyear;
    end;
  end;

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
      'Monitor Stuff': strResults := getMonitorStuff;
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
  lstBxInfo.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, lstBxInfo.Items[Index], ts);
end;

end.

