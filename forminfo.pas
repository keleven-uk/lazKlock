unit formInfo;
{  Displays useful [hopefully] information.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, uInfoUtils;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    btnClose: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblYear: TLabel;
    lblInfo5: TLabel;
    lblInfo4: TLabel;
    lblInfo3: TLabel;
    lblInfo2: TLabel;
    lblInfo1: TLabel;
    SpnEdtYear: TSpinEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpnEdtYearChange(Sender: TObject);
  private
    _info: string;

    procedure updateLabels;
    procedure clearLabels;
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

procedure TfrmInfo.FormCreate(Sender: TObject);
{  Calls the relevent function in uInfoUtils.    }
begin
;
end;

procedure TfrmInfo.FormShow(Sender: TObject);
begin
  klog.writeLog('FormInfo Show' + Info);

  if info = 'Power Source' then
  begin
    lblYear.Visible := False;
    SpnEdtyear.Visible := False;
  end;

  SpnEdtyear.Value := Currentyear;
  clearLabels;
  updateLabels;
end;

procedure TfrmInfo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmInfo.SpnEdtYearChange(Sender: TObject);
{  Updates the information when the year changes.    }
begin
  updateLabels;
end;

procedure TfrmInfo.clearLabels;
begin
  lblinfo1.Caption := '';
  lblinfo2.Caption := '';
  lblinfo3.Caption := '';
  lblinfo4.Caption := '';
  lblinfo5.Caption := '';
end;

procedure TfrmInfo.updateLabels;
{  Updates the labels.    }
var
  strResults: TStringList;
begin
  strResults := TStringList.Create;

  case Info of
    'Daylight Saving': strResults := getDaylightSaving(SpnEdtyear.Value);
    'Easter Dates': strResults := getEasterDates(SpnEdtyear.Value);
    'Lent Dates': strResults := getLentDates(SpnEdtyear.Value);
    'Power Source': strResults := getPower;
  end;

  klog.writeLog(format('list count = %d', [strResults.Count]));
  if strResults.Count > 0 then lblinfo1.Caption := strResults[0];
  if strResults.Count > 1 then lblinfo2.Caption := strResults[1];
  if strResults.Count > 2 then lblinfo3.Caption := strResults[2];
  if strResults.Count > 3 then lblinfo4.Caption := strResults[3];
  if strResults.Count > 4 then lblinfo5.Caption := strResults[4];

  strResults.free;
end;

end.

