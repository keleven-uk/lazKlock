unit formBiorhythm;

{  Display a simple Biorhythm chart, using the Birth date set up user options.
   NB  This form is not shown model.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TAIntervalSources, Forms,
  Controls, Graphics, Dialogs, EditBtn, StdCtrls, dateutils;

type

  { TfrmBiorhythm }

  TfrmBiorhythm = class(TForm)
    Chart1                      : TChart;
    Combined                    : TLineSeries;
    emotional                   : TLineSeries;
    intellectual                : TLineSeries;
    physical                    : TLineSeries;
    todayMark                   : TBarSeries;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    Label1                      : TLabel;

    procedure FormCreate(Sender: TObject);
  private
    procedure plotChart(daysAlive: integer);
  public

  end;

var
  frmBiorhythm: TfrmBiorhythm;

implementation

uses
  formklock;


{$R *.lfm}

{ TForm1 }

procedure TfrmBiorhythm.FormCreate(Sender: TObject);
var
  birthdate : TdateTime;
  daysAlive : integer;
begin
  klog.writeLog('formBiorhythm Show ');

  birthdate      := userOptions.birthdate;
  daysAlive      := DaysBetween(today, birthdate);
  Label1.Caption := format('You have been alive %d days', [daysAlive]);

  plotChart(daysAlive);
end;


procedure TfrmBiorhythm.plotChart(daysAlive: integer);
const
  N = 60;
var
  i    : Integer;
  x    : Double;
  min  : integer;
  days : double;
  sdate: TdateTime;
begin
  klog.writeLog('formBiorhythm.plotChart ');

  min   := daysAlive - 30;
  sdate := today;
  sdate := sdate - 30;

  for i:=0 to N-1 do begin
    x    := MIN + i;
    days := 2 * PI * x;

    physical.AddXY(sdate, sin(days / 23));
    emotional.AddXY(sdate, sin(days / 28));
    intellectual.AddXY(sdate, sin(days / 33));
    Combined.AddXY(sdate, sin(days / 23) + sin(days / 28) + sin(days / 33));

    sdate := sdate + 1;
  end;

  todayMark.AddXY(today,2);

end;

end.

