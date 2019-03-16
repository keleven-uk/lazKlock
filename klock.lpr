program klock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, tachartlazaruspkg, formklock,
  formOptions, uFuzzyTime, SysUtils, UformClipBoardUtils, formClipBoard,
  formStickyNote, formSplashScreen, formEvent;

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
  Application.CreateForm(TfrmClipBoard, frmClipBoard);
  Application.CreateForm(TfrmStickyNote, frmStickyNote);
  Application.CreateForm(TfrmEvent, frmEvent);

  frmSplashScreen.Hide;
  frmSplashScreen.Free;
  Application.Run;
end.

