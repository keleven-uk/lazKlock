program klock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, formklock, formAbout,
  formOptions, uFuzzyTime, formLicense, UKlockUtils, formReminderInput,
  uOptions, formAnalogueKlock, uFonts, ULogging, UformClipBoardUtils,
  formClipBoard, uInfoUtils, formInfo, formLEDKlock, formBinaryKlock,
  formSmallTextKlock, UConversion, ustickyNote, ustickyNotes, formStickyNote;

{$R *.res}

begin
  Application.Title:='lazKlock';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmLicense, frmLicense);
  Application.CreateForm(TfrmReminderInput, frmReminderInput);
  Application.CreateForm(TfrmAnalogueKlock, frmAnalogueKlock);
  Application.CreateForm(TfrmClipBoard, frmClipBoard);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TfrmLEDKlock, frmLEDKlock);
  Application.CreateForm(TfrmBinaryKlock, frmBinaryKlock);
  Application.CreateForm(TfrmSmallTextKlock, frmSmallTextKlock);
  Application.CreateForm(TfrmStickyNote, frmStickyNote);
  Application.Run;
end.

