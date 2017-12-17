program klock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, formklock, formAbout, formHelp,
  formOptions, uFuzzyTime, formLicense, UKlockUtils, formReminderInput,
  uOptions, formAnalogueKlock, uFonts, uOptionsUtils, ULogging,
  UformClipBoardUtils, formClipBoard;

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
  Application.CreateForm(TfrmAnalogueKlock, frmAnalogueKlock);
  Application.CreateForm(TfrmClipBoard, frmClipBoard);
  Application.Run;
end.

