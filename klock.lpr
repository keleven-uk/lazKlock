program klock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, tachartlazaruspkg, formklock,
  formOptions, uFuzzyTime, formAnalogueKlock, SysUtils, UformClipBoardUtils,
  formClipBoard, formLEDKlock, formBinaryKlock, formSmallTextKlock, formStickyNote,
  formFloatingKlock, formSplashScreen, formBiorhythm, formEvent;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='lazKlock';

  {$IFDEF TEST}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$endif}

  Application.Initialize;
  frmSplashScreen := TfrmSplashScreen.Create(nil);
  frmSplashScreen.Show;
  frmSplashScreen.Update;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmAnalogueKlock, frmAnalogueKlock);
  Application.CreateForm(TfrmClipBoard, frmClipBoard);
  Application.CreateForm(TfrmLEDKlock, frmLEDKlock);
  Application.CreateForm(TfrmBinaryKlock, frmBinaryKlock);
  Application.CreateForm(TfrmSmallTextKlock, frmSmallTextKlock);
  Application.CreateForm(TfrmStickyNote, frmStickyNote);
  Application.CreateForm(TfrmFloatingKlock, frmFloatingKlock);
  Application.CreateForm(TfrmBiorhythm, frmBiorhythm);
  Application.CreateForm(TfrmEvent, frmEvent);

  frmSplashScreen.Hide;
  frmSplashScreen.Free;

  Application.Run;
end.

