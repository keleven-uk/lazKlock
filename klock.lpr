program klock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, formklock, formAbout, formHelp, formOptions, uFuzzyTime,
  formLicense, UKlockUtils, formReminderInput, uOptions;

{$R *.res}

begin
  Application.Title:='lazKlock';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmHelp, frmHelp);
  Application.CreateForm(TfrmLicense, frmLicense);
  Application.CreateForm(TfrmReminderInput, frmReminderInput);
  Application.Run;
end.

