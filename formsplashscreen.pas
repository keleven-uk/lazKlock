unit formSplashScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmSplashScreen }

  TfrmSplashScreen = class(TForm)
    lblSplashScreen: TLabel;
    MemoSplashScreenInfo: TMemo;
    MemoSplashScreenData: TMemo;
  private

  end;

var
  frmSplashScreen: TfrmSplashScreen;

implementation

{$R *.lfm}

{ TfrmSplashScreen }


end.

